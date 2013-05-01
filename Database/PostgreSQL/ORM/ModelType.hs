{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DefaultSignatures,
    FlexibleContexts, FlexibleInstances, TypeOperators, OverloadedStrings #-}

module Database.PostgreSQL.ORM.Model (
    DBKeyType, DBKey(..), isNullKey
    , Model(..), ModelInfo(..)
    , LookupRow(..), UpdateRow(..), InsertRow(..)
    , DBRef(..), mkDBRef, dbRefToInfo
    , defaultModelInfo
    , defaultModelInfoName, defaultModelColumns, defaultModelGetPrimaryKey
    , defaultModelRead, defaultModelWrite
    , defaultModelLookupQuery, defaultModelUpdateQuery, defaultModelInsertQuery
    ) where

import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Int
import Data.Monoid
import Data.List
import Data.Typeable
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Database.PostgreSQL.ORM.RequireSelector

type DBKeyType = Int64

data DBKey = DBKey !DBKeyType | NullKey deriving (Typeable)

instance Eq DBKey where
  (DBKey a) == (DBKey b) = a == b
  _         == _         = error "compare NullKey"
instance Ord DBKey where
  compare (DBKey a) (DBKey b) = compare a b
  compare _ _                 = error "compare NullKey"

instance Show DBKey where
  showsPrec n (DBKey k) = showsPrec n k
  showsPrec _ NullKey   = ("null" ++)
instance Read DBKey where
  readsPrec n s = case readsPrec n s of
    [] | [("null", r)] <- lex s -> [(NullKey, r)]
    kr -> map (\(k, r) -> (DBKey k, r)) kr

instance FromField DBKey where
  fromField _ Nothing = pure NullKey
  fromField f bs      = DBKey <$> fromField f bs
instance ToField DBKey where
  toField (DBKey k) = toField k
  toField NullKey   = toField Null

isNullKey :: DBKey -> Bool
isNullKey NullKey = True
isNullKey _       = False


data ModelInfo a = Model {
    modelInfoName :: !S.ByteString
    -- ^ The name of the database table corresponding to this model.
    -- The default is the same as the type name.
  , modelColumns :: ![S.ByteString]
    -- ^ The name of columns in the database table that corresponds to
    -- this model.  The column names should appear in the order that the
    -- data fields occur in the haskell data type @a@ (or at least the
    -- order in which 'modelRead' parses them).  The default is to use
    -- the Haskell field names for @a@.  This default will fail to
    -- compile if @a@ is not defined using record syntax.
  , modelGetPrimaryKey :: !(a -> DBKey)
    -- ^ Return the primary key of a particular model instance.
  , modelRead :: !(RowParser a)
    -- ^ Parse a database row corresponding to the model.
  , modelWrite :: !(a -> [Action])
    -- ^ Format all fields but the primary key for writing the model
    -- to the database.
  , modelLookupQuery :: !Query
  , modelUpdateQuery :: !Query
  , modelInsertQuery :: !Query
  }

instance Show (ModelInfo a) where
  show a = intercalate " " ["Model", show $ modelInfoName a
                           , show $ modelColumns a, "???"
                           , show $ fromQuery $ modelLookupQuery a
                           , show $ fromQuery $ modelInsertQuery a
                           , show $ fromQuery $ modelUpdateQuery a]

class GDatatypeName f where
  gDatatypeName :: f p -> S.ByteString
instance (Datatype c) => GDatatypeName (M1 i c f) where 
  gDatatypeName a = fromString $ datatypeName a
defaultModelInfoName :: (Generic a, GDatatypeName (Rep a)) => a -> S.ByteString
defaultModelInfoName = gDatatypeName . from

class GColumns f where
  gColumns :: f p -> [S.ByteString]
instance GColumns U1 where
  gColumns _ = []
instance (Selector c, RequireSelector c) => GColumns (M1 S c f) where
  gColumns s = [fromString $ selName s]
instance (GColumns a, GColumns b) => GColumns (a :*: b) where
  gColumns ~(a :*: b) = gColumns a ++ gColumns b
instance (GColumns f) => GColumns (M1 C c f) where
  gColumns ~(M1 fp) = gColumns fp
instance (GColumns f) => GColumns (M1 D c f) where
  gColumns ~(M1 fp) = gColumns fp
defaultModelColumns :: (Generic a, GColumns (Rep a)) => a -> [S.ByteString]
defaultModelColumns = gColumns . from

-- | This class extracts the first field in a data structure when the
-- field is of type 'DBKey'.  If you get a compilation error because
-- of this class, then move the 'DBKey' first in your data structure
-- or redefine 'primaryKeyIndex'.
class GPrimaryKey0 f where
  gPrimaryKey0 :: f p -> DBKey
instance (Selector c, RequireSelector c) =>
         GPrimaryKey0 (M1 S c (K1 i DBKey)) where
  gPrimaryKey0 (M1 (K1 k)) = k
instance (GPrimaryKey0 a) => GPrimaryKey0 (a :*: b) where
  gPrimaryKey0 (a :*: _) = gPrimaryKey0 a
instance (GPrimaryKey0 f) => GPrimaryKey0 (M1 C c f) where
  gPrimaryKey0 (M1 fp) = gPrimaryKey0 fp
instance (GPrimaryKey0 f) => GPrimaryKey0 (M1 D c f) where
  gPrimaryKey0 (M1 fp) = gPrimaryKey0 fp

-- | Extract the primary key of type 'DBKey' from a model when the
-- 'DBKey' is the first element of the data structure.  Fails to
-- compile if the first field is not of type 'DBKey'.
defaultModelGetPrimaryKey :: (Generic a, GPrimaryKey0 (Rep a)) => a -> DBKey
defaultModelGetPrimaryKey = gPrimaryKey0 . from


class GFromRow f where
  gFromRow :: RowParser (f p)
instance GFromRow U1 where
  gFromRow = return U1
instance (FromField c) => GFromRow (K1 i c) where
  gFromRow = K1 <$> field
instance (GFromRow a, GFromRow b) => GFromRow (a :*: b) where
  gFromRow = (:*:) <$> gFromRow <*> gFromRow
instance (GFromRow f) => GFromRow (M1 i c f) where
  gFromRow = M1 <$> gFromRow
defaultFromRow :: (Generic a, GFromRow (Rep a)) => RowParser a
defaultFromRow = to <$> gFromRow

defaultModelRead :: (Generic a, GFromRow (Rep a)) => RowParser a
defaultModelRead = defaultFromRow


class GToRow f where
  gToRow :: f p -> [Action]
instance GToRow U1 where
  gToRow _ = []
instance (ToField c) => GToRow (K1 i c) where
  gToRow (K1 c) = [toField c]
instance (GToRow a, GToRow b) => GToRow (a :*: b) where
  gToRow (a :*: b) = gToRow a ++ gToRow b
instance (GToRow f) => GToRow (M1 i c f) where
  gToRow (M1 fp) = gToRow fp
defaultToRow :: (Generic a, GToRow (Rep a)) => a -> [Action]
defaultToRow = gToRow . from

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (_:t) = t
deleteAt n (h:t) = h:deleteAt (n-1) t
deleteAt _ _     = []

defaultModelWrite :: (Generic a, GToRow (Rep a)) =>
                     Int        -- ^ Field number of the primary key
                     -> a       -- ^ Model to write
                     -> [Action] -- ^ Writes all fields except primary key
defaultModelWrite pki = deleteAt pki . defaultToRow


q :: S.ByteString -> S.ByteString
q iden = S8.pack $ '"' : (go $ S8.unpack iden)
  where go ('"':cs) = '"':'"':go cs
        go ('\0':_) = error $ "q: illegal NUL character in " ++ show iden
        go (c:cs)   = c:go cs
        go []       = '"':[]

fmtCols :: [S.ByteString] -> S.ByteString
fmtCols cs = "(" <> S.intercalate ", " (map q cs) <> ")"


defaultModelLookupQuery :: S.ByteString -- ^ Name of database table
                           -> [S.ByteString] -- ^ Names of columns
                           -> Int            -- ^ Index of primary key field
                           -> Query
defaultModelLookupQuery t cs pki = Query $ S.concat [
  "select ", fmtCols cs, " from ", q t, " where ", q (cs !! pki), " = ?"
  ]

defaultModelUpdateQuery :: S.ByteString -- ^ Name of database table
                           -> [S.ByteString] -- ^ Names of columns
                           -> Int            -- ^ Index of primary key field
                           -> Query
defaultModelUpdateQuery t cs pki = Query $ S.concat [
    "update ", q t, " set "
    , S.intercalate ", " (map (\c -> q c <> " = ?") $ deleteAt pki cs)
    , " where ", cs !! pki, " = ?"
  ]

defaultModelInsertQuery :: S.ByteString -- ^ Name of database table
                           -> [S.ByteString] -- ^ Names of columns
                           -> Int            -- ^ Index of primary key field
                           -> Query
defaultModelInsertQuery t cs0 pki = Query $ S.concat $ [
  "insert into ", q t, " ", fmtCols cs1, " values ("
  , S.intercalate ", " $ map (const "?") cs1
  , ") returning ", fmtCols cs0
  ]
  where cs1 = deleteAt pki cs0


defaultModelInfo :: (Generic a, GToRow (Rep a), GFromRow (Rep a)
                    , GPrimaryKey0 (Rep a), GColumns (Rep a)
                    , GDatatypeName (Rep a)) => ModelInfo a
defaultModelInfo = m
  where m = Model { modelInfoName = mname
                  , modelColumns = cols
                  , modelGetPrimaryKey = defaultModelGetPrimaryKey
                  , modelRead = defaultModelRead
                  , modelWrite = defaultModelWrite pki
                  , modelLookupQuery = defaultModelLookupQuery mname cols pki
                  , modelInsertQuery = defaultModelInsertQuery mname cols pki
                  , modelUpdateQuery = defaultModelUpdateQuery mname cols pki
                  }
        unModel :: ModelInfo a -> a
        unModel _ = undefined
        a = unModel m
        mname = defaultModelInfoName a
        pki = 0
        cols = defaultModelColumns a


class Model a where
  modelInfo :: ModelInfo a
  default modelInfo :: (Generic a, GToRow (Rep a), GFromRow (Rep a)
                       , GPrimaryKey0 (Rep a), GColumns (Rep a)
                       , GDatatypeName (Rep a)) => ModelInfo a
  {-# INLINE modelInfo #-}
  modelInfo = defaultModelInfo

modelToInfo :: (Model a) => a -> ModelInfo a
{-# INLINE modelToInfo #-}
modelToInfo _ = modelInfo

modelName :: (Model a) => a -> S.ByteString
{-# INLINE modelName #-}
modelName = modelInfoName . modelToInfo

primaryKey :: (Model a) => a -> DBKey
{-# INLINE primaryKey #-}
primaryKey a = modelGetPrimaryKey modelInfo a

newtype DBRef a = DBRef DBKeyType deriving (Eq, Ord, Typeable)

mkDBRef :: (Model a) => a -> DBRef a
mkDBRef a
  | (DBKey k) <- primaryKey a = DBRef k
  | otherwise = error $ "mkDBRef " ++ S8.unpack (modelName a) ++ ": NullKey"

dbRefToInfo :: (Model a) => DBRef a -> ModelInfo a
{-# INLINE dbRefToInfo #-}
dbRefToInfo _ = modelInfo

instance (Model a) => Show (DBRef a) where
  showsPrec n r@(DBRef k) = showParen (n > 10) $
    ("DBRef{" ++ ) . (mname ++) . ("} " ++) . showsPrec 11 k
    where mname = S8.unpack $ modelInfoName $ dbRefToInfo r

instance FromField (DBRef a) where fromField f bs = DBRef <$> fromField f bs
instance ToField (DBRef a) where toField (DBRef k) = toField k


newtype LookupRow a = LookupRow { lookupRow :: a } deriving (Show, Typeable)
instance (Model a) => FromRow (LookupRow a) where
  fromRow = LookupRow <$> modelRead modelInfo

newtype UpdateRow a = UpdateRow a deriving (Show, Typeable)
instance (Model a) => ToRow (UpdateRow a) where
  toRow (UpdateRow a) = toRow $ InsertRow a :. Only (primaryKey a)

newtype InsertRow a = InsertRow a deriving (Show, Typeable)
instance (Model a) => ToRow (InsertRow a) where
  toRow (InsertRow a) = modelWrite modelInfo a




{-
data Foo = Foo {
    fooKey :: !DBKey
  , fooNone :: !Int32
  , fooString :: !String
  , fooParent :: !(Maybe (DBRef Foo))
  } deriving (Show, Generic)
                                    
instance Model Foo

foo :: Foo
foo = Foo (DBKey 4) 77 "hi" Nothing
-}
