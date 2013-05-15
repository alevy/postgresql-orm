{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Database.PostgreSQL.ORM.Association where

import qualified Data.ByteString as S
import Data.List
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Data.AsTypeOf
import Data.GetField
import Database.PostgreSQL.ORM.DBSelect
import Database.PostgreSQL.ORM.Model

-- | A trivial 'ToRow' instance that can be passed to 'query'.
newtype TrivParam = TrivParam [Action] deriving (Show)
instance ToRow TrivParam where
  toRow (TrivParam actions) = actions

-- | A data structure representing a relationship between a model @a@
-- and a model @b@.  At a high level, an @Association a b@ tells you
-- how to find rows of type @b@s given rows of type @a@.  More
-- concretely, this boils down to being able to make two types of
-- query.
--
--  * You already have an instance of type @a@, and want to find all
--    the @b@s associated with it.  For that you use fields
--    'assocQuery' and 'assocParam'.  If @a :: A@ and @assoc ::
--    Association A B@, to get a list of @B@s you would say:
--
-- >   bs <- map lookupRow <$> query conn (assocQuery assoc) (assocParam assoc a)
--
--  * You want to look up a bunch of @b@s, but filter using predicates
--    on the associated @a@s (e.g., get a list of users who have
--    commented on posts by a particular user).  For this purpose, you
--    can use 'assocSelect', which allows you to 'addWhere' predicates
--    mentioning columns in both @a@ and @b@.
--
-- Note that an association is asymmetric.  It tells you how to get
-- @b@s from @a@s, but not vice versa.  In practice, there will almost
-- always be an association in the other direction, too.  Several
-- functions in this file therefore create both 'Association's
-- simultaneously and return them as a pair.
data Association a b = Association {
    assocSelect :: !(DBSelect (LookupRow b))
    -- ^ General select returning all fields of @a@ from the join
    -- relation between @a@ and @b@.
  , assocQuery :: !Query
    -- ^ An optimized 'Query' to find all the 'a's associated with a
    -- particular @b@.  This can often be done more efficiently than
    -- through 'assocSelect'.
  , assocParam :: !(a -> TrivParam)
    -- ^ The query parameters for the query returned by 'assocQuery'.
  }

instance Show (Association b a) where
  show assoc = "Association " ++ show (assocSelect assoc) ++ " " ++
               show (assocQuery assoc) ++ " ?"

data GDBRefInfo reftype child parent = DBRefInfo {
    dbrefSelector :: !(child -> GDBRef reftype parent)
    -- ^ Field selector returning a reference.
  , dbrefQColumn :: !S.ByteString
    -- ^ Double-quoted, table-qualified name of the database column
    -- storing the reference.  (E.g.,
    -- @dbrefQColumn = \"\\\"Table\\\".\\\"Column\\\"\"@)
  }

instance Show (GDBRefInfo rt c p) where
  show ri = "DBRefInfo ? " ++ show (dbrefQColumn ri)

type DBRefInfo = GDBRefInfo NormalRef

data ExtractRef a = ExtractRef deriving (Show)
instance Extractor ExtractRef (GDBRef rt a) (DBRef a) THasOne where
  extract _ (DBRef k) = THasOne $ DBRef k
instance Extractor ExtractRef (GDBRef rt a) (DBRef (As alias a)) THasOne where
  extract _ (DBRef k) = THasOne $ DBRef k
instance Extractor ExtractRef (Maybe (GDBRef rt a)) (DBRef a) THasOne where
  extract _ (Just (DBRef k)) = THasOne $ DBRef k
  extract _ _                = error "Maybe DBRef is Nothing"
instance Extractor ExtractRef (Maybe (GDBRef rt a)) (DBRef (As alias a))
         THasOne where
  extract _ (Just (DBRef k)) = THasOne $ DBRef k
  extract _ _                = error "Maybe DBRef is Nothing"

defaultDBRefInfo :: (Model child, Model parent
                    , GetField ExtractRef child (DBRef parent)) =>
                    DBRefInfo child parent
defaultDBRefInfo = ri
  where extractor = (const ExtractRef :: g p -> ExtractRef (DBRef p)) ri
        child = undef2 ri
        childids = modelIdentifiers `gAsTypeOf` child
        ri = DBRefInfo {
            dbrefSelector = getFieldVal extractor
          , dbrefQColumn = modelQColumns childids !! getFieldPos extractor child
          }

mkJoin :: DBSelect a -> DBSelect b -> Query -> Query -> DBSelect a
mkJoin dbsa dbsb (Query kw) (Query on) = dbsa {
    selJoins = selJoins dbsa ++ Join kw (selFrom dbsb) on : selJoins dbsb
    -- XXX maybe concatenate where clauses
  }

dbrefAssocs :: (Model child, Model parent) =>
               GDBRefInfo rt child parent
               -> (Association child parent, Association parent child)
dbrefAssocs ri = (c_p, p_c)
  where idp = modelIdentifiers `gAsTypeOf1` ri
        idc = modelIdentifiers `gAsTypeOf2` ri
        on = Query $ "ON " <> modelQPrimaryColumn idp
             <> " = " <> dbrefQColumn ri
        psel = modelDBSelect `gAsTypeOf` (LookupRow $ undef1 ri)
        csel = modelDBSelect `gAsTypeOf` (LookupRow $ undef2 ri)
        c_p = Association {
            assocSelect = mkJoin psel csel "JOIN" on
          , assocQuery = defaultModelLookupQuery idp
          , assocParam = \c -> TrivParam [toField $ dbrefSelector ri c]
          }
        p_c = Association {
            assocSelect = mkJoin csel psel "JOIN" on
          , assocQuery = Query $ modelSelectFragment idc <>
                         " WHERE " <> dbrefQColumn ri <> " = ?"
          , assocParam = \p -> TrivParam [toField $ primaryKey p]
          }

has :: (Model child, Model parent, GetField ExtractRef child (DBRef parent)) =>
       Association parent child
has = snd $ dbrefAssocs defaultDBRefInfo

belongsTo :: (Model child, Model parent
             , GetField ExtractRef child (DBRef parent)) =>
             Association child parent
belongsTo = fst $ dbrefAssocs defaultDBRefInfo

splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast s = Just $ go s
  where go [a]    = ([], a)
        go (a:as) = case go as of (as', a') -> (a:as', a')
        go []     = error "splitLast"

nestAssoc :: Association a b -> Association b c -> Association (a :. b) c
nestAssoc ab bc = Association {
    assocSelect = (assocSelect bc) {
       selJoins = selJoins (assocSelect bc) ++ selJoins (assocSelect ab) }
  , assocQuery = assocQuery bc
  , assocParam = \(_ :. b) -> assocParam bc b
  }

chainAssoc :: (Model a, Model b, Model c) =>
              Association a b -> Association b c -> Association a c
chainAssoc ab bc =
  case splitLast $ selJoins $ assocSelect bc of
    Just (js, Join kw b on) | b == modelQTable idb -> bc {
        assocSelect = sel
      , assocQuery = renderDBSelect $ (assocSelect bc) {
          selJoins = js ++ [Join kw subquery on]
        }
      , assocParam = assocParam ab
      }
    _ -> bc { assocSelect = sel
            , assocQuery = renderDBSelect $ addWhere sel
                           (Query $ modelQPrimaryColumn ida <> " = ?") () 
            , assocParam = \a -> TrivParam [toField $ primaryKey a]
            }
  where idb = modelIdentifiers `gAsTypeOf1` ab
        ida = modelIdentifiers `gAsTypeOf2` ab
        subquery = S.concat ["(", fromQuery $ assocQuery ab
                            , ") AS ", modelQTable idb]
        sel = (assocSelect bc) {
            selJoins = selJoins (assocSelect bc) ++ selJoins (assocSelect ab)
          }

data JoinTable a b = JoinTable {
    jtTable :: !S.ByteString
    -- ^ Name of the join table in the database.  (Not quoted.)
  , jtColumnA :: !S.ByteString
    -- ^ Name of the column in table 'jtTable' that contains a 'DBRef'
    -- to model @a@.  (Not quoted or table-qualified.)
  , jtColumnB :: !S.ByteString
    -- ^ Like 'jtColumnA' for model @b@.
  } deriving (Show)

defaultJoinTable :: (Model a, Model b) => JoinTable a b
defaultJoinTable = jti
  where a = modelInfo `gAsTypeOf2` jti
        b = modelInfo `gAsTypeOf1` jti
        jti = JoinTable {
            jtTable = S.intercalate "_" $ sort [modelTable a, modelTable b]
          , jtColumnA = S.intercalate "_"
                        [modelTable a, modelColumns a !! modelPrimaryColumn a]
          , jtColumnB = S.intercalate "_"
                        [modelTable b, modelColumns b !! modelPrimaryColumn b]
          }

jtQTable :: JoinTable a b -> S.ByteString
jtQTable = quoteIdent . jtTable

jtQColumnA :: JoinTable a b -> S.ByteString
jtQColumnA jt = S.concat [ jtQTable jt, ".", quoteIdent $ jtColumnA jt]

jtQColumnB :: JoinTable a b -> S.ByteString
jtQColumnB jt = S.concat [ jtQTable jt, ".", quoteIdent $ jtColumnB jt]

jtFlip :: JoinTable a b -> JoinTable b a
jtFlip jt = jt { jtColumnA = jtColumnB jt , jtColumnB = jtColumnA jt }

jtAddStatement :: JoinTable a b -> Query
jtAddStatement jt = Query $ S.concat [
    "INSERT INTO ", jtQTable jt, " ("
  , quoteIdent $ jtColumnA jt, ", ", quoteIdent $ jtColumnB jt
  , ") VALUES (?, ?) EXCEPT SELECT "
  , jtQColumnA jt, ", ", jtQColumnB jt, " FROM ", quoteIdent $ jtTable jt
  ]

jtRemoveStatement :: JoinTable a b -> Query
jtRemoveStatement jt = Query $ S.concat [
    "DELETE FROM ", quoteIdent $ jtTable jt, " WHERE "
  , jtQColumnA jt, " = ? AND ", jtQColumnB jt, " = ?"
  ]

jtAssoc :: (Model a, Model b) => JoinTable a b -> (Association a b)
jtAssoc jt = Association {
    assocSelect = sel
  , assocQuery = q
  , assocParam = \a -> TrivParam [toField $ primaryKey a]
  }
  where ida = modelIdentifiers `gAsTypeOf2` jt
        idb = modelIdentifiers `gAsTypeOf1` jt
        joins = [Join "JOIN" (jtQTable jt) $
                 "ON " <> modelQPrimaryColumn idb <> " = " <> jtQColumnB jt
                , Join "JOIN" (modelQTable ida) $
                  "ON " <> jtQColumnA jt <> " = " <> modelQPrimaryColumn ida]
        sel = selb { selJoins = selJoins selb ++ joins }
          where selb = modelDBSelect `asTypeOf` sel
        q = Query $ S.concat [
            modelSelectFragment idb, " JOIN ", jtQTable jt
            , " ON ", modelQPrimaryColumn idb, " = ", jtQColumnB jt
            , " WHERE ", jtQColumnA jt, " = ?"
          ]

jtAssocs :: (Model a, Model b) =>
            JoinTable a b -> (Association a b, Association b a)
jtAssocs jt = (jtAssoc jt, jtAssoc $ jtFlip jt)


{-
data T1 = T1 deriving (Show, Generic)
instance RowAlias T1

data Author = Author {
    authorId :: DBKey
  } deriving (Show, Generic)
instance Model Author where modelInfo = underscoreModelInfo "author"

data Post = Post {
    postId :: DBKey
  , postAuthorId :: DBRef Author
  } deriving (Show, Generic)
instance Model Post where modelInfo = underscoreModelInfo "post"

data Comment = Comment {
    commentId :: DBKey
  , commentPostId :: DBRef Post
  } deriving (Show, Generic)
instance Model Comment where modelInfo = underscoreModelInfo "comment"


author_posts :: Association Author Post
post_author :: Association Post Author
(post_author, author_posts) = dbrefAssocs defaultDBRefInfo


post_comments :: Association Post Comment
post_comments = has
comment_post :: Association Comment Post
comment_post = belongsTo

comment_author :: Association Comment Author
comment_author = chainAssoc comment_post post_author

author_comments :: Association Author Comment
author_comments =  chainAssoc author_posts post_comments



data Quizog = Quizog {
    qId :: !DBKey
  , qNone :: !(Maybe Int32)
  , qName :: !String
  , qEd :: !String
  } deriving (Show, Generic)
instance Model Quizog

quizog :: Quizog
quizog = Quizog NullKey (Just 0) "Quizog!!!" "Q.E.D."

comment_quizog_table :: JoinTable Comment Quizog
comment_quizog_table = defaultJoinTable

comment_quizog :: Association Comment Quizog
quizog_comment :: Association Quizog Comment
(comment_quizog, quizog_comment) = jtAssocs comment_quizog_table


data RefTest = RefTest { rtId :: DBKey
                       , rtRef :: (Maybe (DBRefUnique Quizog))
                       } deriving (Show, Generic)
instance Model RefTest

refTest :: RefTest
refTest = RefTest NullKey (Just (DBRef 99))

refTest' :: As T1 RefTest
refTest' = As refTest

xx :: Association Quizog RefTest
yy :: Association RefTest Quizog
(yy,xx) = dbrefAssocs defaultDBRefInfo

zz :: Association RefTest (As T1 Quizog)
zz = belongsTo

exr :: ExtractRef (DBRef Quizog)
exr = ExtractRef

{-
myjt :: JoinTable Quizog RefTest
myjt = defaultJoinTable
-}

mkc :: IO Connection
mkc = connectPostgreSQL ""

c :: Connection
{-# NOINLINE c #-}
c = unsafePerformIO mkc

-}

dumpAssoc :: Association a b -> IO ()
dumpAssoc a = do
  printq $ renderDBSelect $ assocSelect a
  printq $ assocQuery a
