{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.ORM.Association {-(
    Association(..)
  , dbJoin, dbProject
  , findAssociated, findAssociatedWhere, findBothAssociatedWhere
    -- * Associations based on parent-child relationships
  , GDBRefInfo(..), DBRefInfo, defaultDBRefInfo, dbrefAssocs, has, belongsTo
    -- * Join table Associations
  , JoinTable(..), defaultJoinTable
  , jtAddStatement, jtRemoveStatement, jtParam, jtAssocs
  , joinTable
    -- * Nested and chained associations
  , nestAssoc, chainAssoc
    -- * Miscellaneous and internal details
  , TrivParam(..)
  , jtFlip, jtAssoc
  , dumpAssoc
  )-} where

import Control.Applicative
import qualified Data.ByteString as S
import Data.Functor
import Data.List
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Data.AsTypeOf
import Data.GetField
import Database.PostgreSQL.ORM.DBSelect
import Database.PostgreSQL.ORM.Model


data JoinTable a b = JoinTable { refA :: !(DBRef a)
                               , refB :: !(DBRef b)
                               } deriving (Show)

joinTableIdentifiers :: ModelInfo a -> ModelInfo b
                        -> ModelIdentifiers (JoinTable a b)
joinTableIdentifiers mia mib = ModelIdentifiers {
    modelQTable = name
  , modelQColumns = [colA, colB]
  , modelQPrimaryColumn = error "No primary column in JoinTable"
  , modelQWriteColumns = error "update Join table through dedicated functions"
  , modelQualifier = Just name
  }
  where err = error "update Join table through dedicated functions"
        compound x y = quoteIdent $ S.concat [x, "_", y]
        name = quoteIdent $ S.intercalate "_" [modelTable mia, modelTable mib]
        colA = S.concat [name, ".", compound (modelTable mia)
                         (modelColumns mia !! modelPrimaryColumn mia)]
        colB = S.concat [name, ".", compound (modelTable mib)
                         (modelColumns mib !! modelPrimaryColumn mib)]

instance (Model a, Model b) => Model (JoinTable a b) where
  modelInfo = error "attempt to use Join table as normal model"
  modelIdentifiers = joinTableIdentifiers modelInfo modelInfo
  modelRead = JoinTable <$> field <*> field
  modelWrite = error "update Join table through dedicated functions"
  
instance ToRow (JoinTable a b) where
  toRow jt = [toField $ refA jt, toField $ refB $ jt]

{-

-- | A trivial 'ToRow' instance that can be passed to 'query'.  This
-- structure is useful when you want to build up parameters piecemeal
-- or stick them in a data structure.  To ensure a uniform type
-- regardless of the original types of the parameters, they can be
-- pre-marshalled in a monomorphic list of 'Action's.  @TrivParam@ is
-- a newtype wrapper around such a list, because a newtype is required
-- to declare a new instance of 'ToRow'.  (An alternative would be to
-- declare an @instance 'ToRow' ['Action']@ or @instance ('ToField' a)
-- => 'ToRow' [a]@, but that raises the risk of conflicting with other
-- packages doing the same.)
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
--    Association A B@, to get a list of @B@s you can do the following
--    (though 'findAssociated' already does it):
--
-- @
--   bs <- 'map' 'lookupRow' &#x3C;$&#x3E; 'query' conn (assocQuery assoc) (assocParam assoc a)
-- @
--
--  * You want to look up a bunch of @b@s, but filter using predicates
--    on the associated @a@s (e.g., get a list of users who have
--    commented on posts by a particular user).  For this purpose, you
--    can use 'assocSelect', which allows you to 'addWhere' predicates
--    mentioning columns in both @a@ and @b@.  (Or use the helper
--    function 'findAssociatedWhere', which wraps 'addWhere' for you.)
--
-- 'assocQuery' and 'assocParam' are strictly less general than
-- 'assocQuery'.  However, in common situations the former may be
-- implemented more efficiently (because it knows how to extract
-- fields other than the primary key from @a@, and hence may be able
-- to avoid touching @a@'s index in the database).
--
-- Note that an @Association@ is asymmetric.  It tells you how to get
-- @b@s from @a@s, but not vice versa.  In practice, there will almost
-- always be an association in the other direction, too.  Several
-- functions in this file therefore create an @Association@ and its
-- inverse simultaneously, returning them as a pair.
data Association a b = Association {
    assocSelect :: !(DBSelect b)
    -- ^ General select returning all fields of @a@ from the join
    -- relation between @a@ and @b@.
  , assocQuery :: !Query
    -- ^ An optimized 'Query' to find all the 'a's associated with a
    -- particular @b@.  This can often be done more efficiently than
    -- through 'assocSelect'.
  , assocParam :: !(a -> TrivParam)
    -- ^ The query parameters for the query returned by 'assocQuery'.
  }

instance Show (Association a b) where
  show assoc = "Association " ++ show (assocSelect assoc) ++ " " ++
               show (assocQuery assoc) ++ " ?"

-- | Follow an association to return all all of the @b@s associated
-- with a particular @a@.
findAssociated :: (Model b) =>
                  Association a b -> Connection -> a -> IO [b]
findAssociated assoc c a =
  map lookupRow <$> query c (assocQuery assoc) (assocParam assoc a)

-- | Find all of the @b@s matching a @WHERE@ predicate.  Unlike
-- 'findWhere', the predicate can mention fields of an associated
-- model @a@.  Moreover, if a compound association is used, such as
-- one created with 'chainAssoc' or 'nestAssoc', you can mention
-- fields in intermediary tables as well.
findAssociatedWhere :: (Model b, ToRow p) =>
                       Association a b -> Query -> Connection -> p -> IO [b]
findAssociatedWhere assoc wh c p = map lookupRow <$> query c q p
  where q = renderDBSelect $ addWhere wh () (assocSelect assoc)

findBothAssociatedWhere :: (Model a, Model b, ToRow p) =>
                           Association a b -> Query -> Connection -> p
                           -> IO [a :. b]
findBothAssociatedWhere assoc wh c p = r
  where asel = (const $ modelDBSelect
                :: (Model a) => IO [a :. b] -> DBSelect a) r
        insa = \sel -> sel {
          selFields = selFields asel <> ", " <> selFields sel }
        q = renderDBSelect $ insa $ addWhere wh () $ assocSelect assoc
        r = map lookupRow <$> query c q p


-- | A common type of association is when one model contains a 'DBRef'
-- or 'DBRefUnique' pointing to another model.  In this case, the
-- model containing the 'DBRef' is known as the /child/, and the
-- referenced model is known as the /parent/.
--
-- Two pieces of information are required to describe a parent-child
-- relationship:  First, the field selector that extracts the Haskell
-- 'DBRef' from the haskell type @child@, and second the name of the
-- database column that stores this 'DBRef' field.
--
-- For example, consider the following:
--
-- > data Author = Author {
-- >     authorId :: DBKey
-- >   } deriving (Show, Generic)
-- > instance Model Author
-- > 
-- > data Post = Post {
-- >     postId :: DBKey
-- >   , postAuthorId :: DBRef Author
-- >   } deriving (Show, Generic)
-- > instance Model Post
-- >
-- > post_author_refinfo :: DBRefInfo Post Author
-- > post_author_refinfo = DBRefInfo {
-- >     dbrefSelector = postAuthorId
-- >   , dbrefQColumn = "\"post\".\"postAuthorId\""
-- >   }
--
-- Note that the parent-child relationship described by a @GDBRefInfo@
-- is asymmetric, but bidirectional.  When a @'DBRefInfo' child
-- parent@ exists, the schema should generally /not/ permit the
-- existence of a valid @'DBRefInfo' parent child@ structure.
-- However, the 'dbrefAssocs' function generates 'Association's in
-- both directions from a single 'DBRefInfo'.
--
-- Constructing such parent-child 'Association's requires knowing how
-- to extract primary keys from the @parent@ type as well as the name
-- of the column storing primary keys in @parent@.  Fortunately, this
-- information is already available from the 'Model' class, and thus
-- does not need to be in the @GDBRefInfo@.  (Most functions on
-- @GDBRefInfo@s require @parent@ and @child@ to be instances of
-- 'Model'.)
--
-- When your 'Model's are instances of 'Generic' (which will usually
-- be the case), a 'DBRefInfo' structure can be computed automatically
-- by 'defaultDBRefInfo'.  This is the recommended way to produce a
-- @GDBRefInfo@.  (Alternatively, see 'has' and 'belongsTo' to make
-- use of an entirely implicit @DBRefInfo@.)
data GDBRefInfo reftype child parent = DBRefInfo {
    dbrefSelector :: !(child -> GDBRef reftype parent)
    -- ^ Field selector returning a reference.
  , dbrefQColumn :: !S.ByteString
    -- ^ Literal SQL for the database column storing the reference.
    -- This should be double-quoted and table-qualified, in case the
    -- column name is a reserved keyword, contains capital letters, or
    -- conflicts with the name of a column in the joined table.  An
    -- example would be:  @dbrefQColumn =
    -- \"\\\"table_name\\\".\\\"column_name\\\"\"@
  }

instance Show (GDBRefInfo rt c p) where
  show ri = "DBRefInfo ? " ++ show (dbrefQColumn ri)

-- | @DBRefInfo@ is a type alias for the common case that the
-- reference in a 'GDBRefInfo' is a 'DBRef' (as opposed to a
-- 'DBRefUnique').  The functions in this library do not care what
-- type of reference is used.  The type is generalized to 'GDBRefInfo'
-- just to make it easier to assign a selector to 'dbrefSelector' when
-- the selector returns a 'DBRefUnique'.  For example,
-- 'defaultDBRefInfo' returns a 'DBRefInfo' regardless of the flavor
-- of reference actually encountered.
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

-- | Creates a 'DBRefInfo' from a model @child@ that references
-- @parent@.  For this to work, the @child@ type must be an instance
-- of 'Generic' and must contain exactly one field of the any of the
-- following types:
--
--   1. @'GDBRef' rt parent@, which matches both @'DBRef' parent@ and
--   @'DBRefUnique' parent@.
--
--   2. @Maybe ('GDBRef' rt parent)@, for cases where the reference
--   might be @NULL@.  Note, however, that an exception will be thrown
--   if you call 'findAssociated' on a child whose reference is
--   'Nothing'.
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
mkJoin dbsa dbsb kw on = dbsa {
    selJoins = selJoins dbsa ++ Join kw (selFrom dbsb) on : selJoins dbsb
    -- XXX maybe concatenate where clauses
  }

-- | Generate both the child-parent and parent-child 'Association's
-- implied by a 'GDBRefInfo'.
dbrefAssocs :: (Model child, Model parent) =>
               GDBRefInfo rt child parent
               -> (Association child parent, Association parent child)
dbrefAssocs ri = (c_p, p_c)
  where idp = modelIdentifiers `gAsTypeOf1` ri
        idc = modelIdentifiers `gAsTypeOf2` ri
        on = Query $ "ON " <> modelQPrimaryColumn idp
             <> " = " <> dbrefQColumn ri
        psel = modelDBSelect `gAsTypeOf` (undef1 ri)
        csel = modelDBSelect `gAsTypeOf` (undef2 ri)
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

-- | Short for
--
-- > snd $ dbrefAssocs defaultDBRefInfo
--
-- Note the inverse 'Association' is given by 'belongsTo'.  For
-- example, given the @Author@ and @Post@ models described in the
-- documentation for 'GDBRefInfo', in which each @Post@ references an
-- @Author@, you might say:
--
-- > author_post :: Association Author Post
-- > author_post = has
-- >
-- > post_author :: Association Post Author
-- > post_author = belongsTo
has :: (Model child, Model parent, GetField ExtractRef child (DBRef parent)) =>
       Association parent child
has = snd $ dbrefAssocs defaultDBRefInfo

-- | The inverse of 'has'.  Short for
--
-- > fst $ dbrefAssocs defaultDBRefInfo
--
-- See an example at 'has'.
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
    Just (js, Join kw b on) | fromQuery b == modelQTable idb -> bc {
        assocSelect = sel
      , assocQuery = renderDBSelect $ (assocSelect bc) {
          selJoins = js ++ [Join kw subquery on]
        }
      , assocParam = assocParam ab
      }
    _ -> bc { assocSelect = sel
            , assocQuery = renderDBSelect $ addWhere
                           (Query $ modelQPrimaryColumn ida <> " = ?") () 
                           sel
            , assocParam = \a -> TrivParam [toField $ primaryKey a]
            }
  where idb = modelIdentifiers `gAsTypeOf1` ab
        ida = modelIdentifiers `gAsTypeOf2` ab
        subquery = Query $ S.concat ["(", fromQuery $ assocQuery ab
                                    , ") AS ", modelQTable idb]
        sel = (assocSelect bc) {
            selJoins = selJoins (assocSelect bc) ++ selJoins (assocSelect ab)
          }

-- | Note that all names in a @JoinTable@ should be unquoted.
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

jtParam :: (Model a, Model b) => JoinTable a b -> a -> b -> TrivParam
jtParam _ a b = TrivParam [toField (primaryKey a), toField (primaryKey b)]

jtAssoc :: (Model a, Model b) => JoinTable a b -> (Association a b)
jtAssoc jt = Association {
    assocSelect = sel
  , assocQuery = q
  , assocParam = \a -> TrivParam [toField $ primaryKey a]
  }
  where ida = modelIdentifiers `gAsTypeOf2` jt
        idb = modelIdentifiers `gAsTypeOf1` jt
        joins = [Join "JOIN" (Query $ jtQTable jt) $ Query $
                 "ON " <> modelQPrimaryColumn idb <> " = " <> jtQColumnB jt
                , Join "JOIN" (Query $ modelQTable ida) $ Query $
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

joinTable :: (Model a, Model b) => Association a b
joinTable = jtAssoc defaultJoinTable

{-

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

-}
