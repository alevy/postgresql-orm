{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Database.PostgreSQL.ORM.Association where

import qualified Data.ByteString as S
import Data.Function
import Data.Functor
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Data.AsTypeOf
import Data.GetField
import Database.PostgreSQL.ORM.DBSelect
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.SqlType

import GHC.Generics
import Database.PostgreSQL.ORM.CreateTable
import System.IO.Unsafe

newtype TrivParam = TrivParam [Action] deriving (Show)
instance ToRow TrivParam where
  toRow (TrivParam actions) = actions

-- | How to find an @a@ given some predicate on @b@s ('assocSelect'),
-- or a particular @b@ ('assocQuery' and 'assocParam').
data Association a b = Association {
    assocSelect :: !(DBSelect (LookupRow a))
    -- ^ General select returning all fields of @a@ from the join
    -- relation between @a@ and @b@.
  , assocQuery :: !Query
    -- ^ An optimized 'Query' to find all the 'a's associated with a
    -- particular @b@.  This can often be done more efficiently than
    -- through 'assocSelect'.
  , assocParam :: !(b -> TrivParam)
    -- ^ The query parameters for the query returned by 'assocQuery'.
  }

instance Show (Association a b) where
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

type DBRefInfo = GDBRefInfo NormalRef

data ExtractRef a = ExtractRef deriving (Show)
instance Extractor ExtractRef (GDBRef rt a) (DBRef a) THasOne where
  extract _ (DBRef k) = THasOne $ DBRef k
instance Extractor ExtractRef (GDBRef rt a) (DBRef (As alias a)) THasOne where
  extract _ (DBRef k) = THasOne $ DBRef k
instance Extractor ExtractRef (Maybe (GDBRef rt a)) (DBRef a) THasOne where
  extract _ (Just (DBRef k)) = THasOne $ DBRef k
instance Extractor ExtractRef (Maybe (GDBRef rt a)) (DBRef (As alias a))
         THasOne where
  extract _ (Just (DBRef k)) = THasOne $ DBRef k

instance Show (GDBRefInfo rt c p) where
  show ri = "DBRefInfo ? " ++ show (dbrefQColumn ri)

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

dbrefAssocs :: (Model child, Model parent) => GDBRefInfo rt child parent
               -> (Association child parent, Association parent child)
dbrefAssocs ri = (c_p, p_c)
  where idp = modelIdentifiers `gAsTypeOf1` c_p
        idc = modelIdentifiers `gAsTypeOf1` p_c
        on = " ON " <> modelQPrimaryColumn idp <> " = " <> dbrefQColumn ri
        c_p = Association {
            assocSelect = fix $ \r -> (modelDBSelect `asTypeOf` r) {
               selJoins = [Query $ "JOIN " <> modelQTable idp <> on] }
          , assocQuery = Query $ modelSelectFragment idc <>
                         " WHERE " <> dbrefQColumn ri <> " = ?"
          , assocParam = \p -> TrivParam [toField $ primaryKey p]
          }
        p_c = Association {
            assocSelect = fix $ \r -> (modelDBSelect `asTypeOf` r) {
               selJoins = [Query $ "JOIN " <> modelQTable idc <> on] }
          , assocQuery = defaultModelLookupQuery idp
          , assocParam = \c -> TrivParam [toField $ dbrefSelector ri c]
          }

has :: (Model a, Model b, GetField ExtractRef b (DBRef a)) => Association a b
has = snd $ dbrefAssocs defaultDBRefInfo

belongsTo :: (Model a, Model b, GetField ExtractRef b (DBRef a)) =>
             Association b a
belongsTo = fst $ dbrefAssocs defaultDBRefInfo


chainAssoc :: Association a b -> Association b c -> Association a (b :. c)
chainAssoc ab bc = Association {
    assocSelect = (assocSelect ab) {
       selJoins = selJoins (assocSelect ab) ++ selJoins (assocSelect bc) }
  , assocQuery = assocQuery ab
  , assocParam = \(b :. _) -> assocParam ab b
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

jtCreateStatement :: (Model a, Model b) => JoinTable a b -> Query
jtCreateStatement jt = Query $ S.concat [
    "CREATE TABLE ", quoteIdent $ jtTable jt, " ("
    , S.intercalate ", " $ sort [typa, typb]
    , ", UNIQUE (", S.intercalate ", " $ sort [ida, idb], "))"
  ]
  where ida = quoteIdent $ jtColumnA jt
        idb = quoteIdent $ jtColumnB jt
        refa = (undefined :: JoinTable a b -> DBRef a) jt
        refb = (undefined :: JoinTable a b -> DBRef b) jt
        typa = ida <> " " <> sqlBaseType refa <> " ON DELETE CASCADE NOT NULL"
        typb = idb <> " " <> sqlBaseType refb <> " ON DELETE CASCADE NOT NULL"

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
  , assocParam = \b -> TrivParam [toField $ primaryKey b]
  }
  where ida = modelIdentifiers `gAsTypeOf2` jt
        idb = modelIdentifiers `gAsTypeOf1` jt
        joins = [Query $ "JOIN " <> jtQTable jt <>
                 " ON " <> modelQPrimaryColumn ida <> " = " <> jtQColumnA jt
                , Query $ "JOIN " <> modelQTable idb <>
                  " ON " <> jtQColumnB jt <> " = " <> modelQPrimaryColumn idb]
          
        sel = sela { selJoins = selJoins sela ++ joins }
          where sela = modelDBSelect `asTypeOf` sel
        q = Query $ S.concat [
            modelSelectFragment ida, " JOIN ", jtQTable jt
            , " ON ", modelQPrimaryColumn ida, " = ", jtQColumnA jt
            , " WHERE ", jtQColumnB jt, " = ?"
          ]

jtAssocs :: (Model a, Model b) =>
            JoinTable a b -> (Association a b, Association b a)
jtAssocs jt = (jtAssoc jt, jtAssoc $ jtFlip jt)


{-
joinThroughModelInfo :: (Model jt, Model a, Model b
                        , GetField ExtractRef jt (DBRef a)
                        , GetField ExtractRef jt (DBRef b)) =>
                        ModelInfo jt -> JoinTableInfo a b
joinThroughModelInfo jt = jti
  where (a, b) = joinTableInfoModels jti
        extractor = const ExtractRef :: ModelInfo a -> ExtractRef (DBRef a)
        jti = JoinTableInfo {
            jtTable = modelTable jt
          , jtAllowModification = False
          , jtColumnA = modelColumns jt !! getFieldPos (extractor a) (undef1 jt)
          , jtKeyA = modelColumns a !! modelPrimaryColumn a
          , jtColumnB = modelColumns jt !! getFieldPos (extractor b) (undef1 jt)
          , jtKeyB = modelColumns b !! modelPrimaryColumn b
          }

joinThroughModel :: (Model jt, Model a, Model b
                    , GetField ExtractRef jt (DBRef a)
                    , GetField ExtractRef jt (DBRef b)) =>
                   jt -> JoinTableInfo a b
joinThroughModel = joinThroughModelInfo . gAsTypeOf modelInfo


-}


data T1 = T1 deriving (Show, Generic)
instance RowAlias T1

data Quizog = Quizog {
    qId :: !DBKey
  , qNone :: !(Maybe Int32)
  , qName :: !String
  , qEd :: !String
  } deriving (Show, Generic)
instance Model Quizog

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

zz :: Association Quizog RefTest
zz = has

exr :: ExtractRef (DBRef Quizog)
exr = ExtractRef

myjt :: JoinTable Quizog RefTest
myjt = defaultJoinTable

mkc :: IO Connection
mkc = connectPostgreSQL ""

c :: Connection
{-# NOINLINE c #-}
c = unsafePerformIO mkc
