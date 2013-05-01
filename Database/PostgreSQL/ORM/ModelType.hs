{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DefaultSignatures,
    FlexibleContexts, FlexibleInstances, TypeOperators, OverloadedStrings #-}

module Database.PostgreSQL.ORM.Model {-(
    DBKeyType, DBKey(..), isNullKey, DBRef(..), mkDBRef, dbRefModel
    , Row(..), NewRow(..), Model(..)
  , defaultModelName, defaultModelColumns, defaultPrimaryColumn
  , defaultPrimaryKey, defaultFromRow, defaultToRow)-} where

import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Int
import Data.Maybe
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


data Model a = Model {
    modelName :: !S.ByteString
  , modelColumns :: ![S.ByteString]
  , modelGetPrimaryKey :: !(a -> DBKey)
  , modelRead :: !(RowParser a)
  , modelWrite :: !(a -> [Action])
  , modelLookupQuery :: !Query
  , modelUpdateQuery :: !Query
  , modelInsertQuery :: !Query
  }

instance Show (Model a) where
  show a = intercalate " " ["Model", show $ modelName a
                           , show $ modelColumns a, "???"
                           , show $ fromQuery $ modelLookupQuery a
                           , show $ fromQuery $ modelInsertQuery a
                           , show $ fromQuery $ modelUpdateQuery a]

class GDatatypeName f where
  gDatatypeName :: f p -> S.ByteString
instance (Datatype c) => GDatatypeName (M1 i c f) where 
  gDatatypeName a = fromString $ datatypeName a
defaultModelName :: (Generic a, GDatatypeName (Rep a)) => a -> S.ByteString
defaultModelName = gDatatypeName . from

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

-- | Extract the primary key of type 'DBKey' from a 'Model' when the
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
deleteAt 0 (h:t) = t
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
                                   


{-
  
class Model a where
  -- | The name of the database table corresponding to this model.
  -- The default is the same as the type name.
  modelName :: a -> S.ByteString
  default modelName :: (Generic a, GModelName (Rep a)) => a -> S.ByteString
  modelName = defaultModelName

  -- | The name of columns in the database table that corresponds to
  -- this model.  The column names should appear in the order that the
  -- data fields occur in the haskell data type @a@.  The default is
  -- to use the Haskell field names for @a@.  This default will fail
  -- to compile if @a@ is not defined using record syntax.
  modelColumns :: a -> [S.ByteString]
  default modelColumns :: (Generic a, GColumns (Rep a)) => a -> [S.ByteString]
  modelColumns = defaultModelColumns

  -- | A function for extracting the primary key from from a model, as
  -- well as the index of that primary key within 'modelColumns'.  No
  -- checking is performed to ensure that the column index is correct.
  primaryKeyAndIndex :: a -> (a -> DBKey, Int)
  default primaryKeyAndIndex :: (Generic a, GPrimaryKey (Rep a)) =>
                                a -> (a -> DBKey, Int)
  primaryKeyAndIndex _ = defaultPrimaryKeyAndIndex

  modelRead :: RowParser a
  default modelRead :: (Generic a, GFromRow (Rep a)) => RowParser a
  modelRead = defaultFromRow

  modelWrite :: a -> [Action]
  default modelWrite :: (Generic a, GToRow (Rep a)) => a -> [Action]
  modelWrite a = deleteAt (primaryKeyIndex a) $ defaultToRow a

  lookupQuery :: a -> Query
  lookupQuery a0 = Query $ "select " <> fmtCols (modelColumns a) <>
                   " from " <> modelName a <>
                   " where " <> q (primaryColumn a) <> " = ?"
    where a = undefined `asTypeOf` a0

  insertQuery :: a -> Query
  insertQuery a0 = Query $ "insert into " <> modelName a <> " " <>
                   fmtCols cols <> " values " <>
                   fmtCols (map (const "?") cols) <>
                   " returning " <> q (primaryColumn a)
    where a = undefined `asTypeOf` a0
          cols = deleteAt (primaryKeyIndex a) (modelColumns a)

  updateQuery :: a -> Query
  updateQuery a0 = Query $ "update " <> modelName a <> " set " <>
                   S.intercalate ", " (map (\c -> q c <> " = ?") cols) <>
                   " where " <> primaryColumn a <> " = ?"
    where a = undefined `asTypeOf` a0
          cols = deleteAt (primaryKeyIndex a) (modelColumns a)

  -- | A list of column names that should not be created with the
  -- default type.
  columnTypes :: a -> [(S.ByteString, S.ByteString)]
  columnTypes _ = []

primaryKey :: (Model a) => a -> DBKey
primaryKey a = fst (primaryKeyAndIndex a) a

primaryKeyIndex :: (Model a) => a -> Int
primaryKeyIndex a = snd $ primaryKeyAndIndex a

primaryColumn :: (Model a) => a -> S.ByteString
primaryColumn a = modelColumns a !! primaryKeyIndex a

newtype ReadRow a = ReadRow { readRow :: a } deriving (Show, Typeable)
instance (Model a) => FromRow (ReadRow a) where
  fromRow = ReadRow <$> modelRead

newtype WriteRow a = WriteRow a deriving (Show, Typeable)
instance (Model a) => ToRow (WriteRow a) where
  toRow (WriteRow a) = modelWrite a

newtype DBRef a = DBRef DBKeyType deriving (Eq, Ord, Typeable)

mkDBRef :: (Model a) => a -> DBRef a
mkDBRef a
  | (DBKey k) <- primaryKey a = DBRef k
  | otherwise = error $ "mkDBRef (" ++ S8.unpack (modelName a) ++ "): NullKey"

dbRefModel :: DBRef a -> a
dbRefModel _ = error "dbRefModel"

instance (Model a) => Show (DBRef a) where
  showsPrec n r@(DBRef k) = showParen (n > 10) $
    ("DBRef{" ++ ) . (S8.unpack (modelName $ dbRefModel r) ++) . ("} " ++)
    . showsPrec 11 k

instance FromField (DBRef a) where fromField f bs = DBRef <$> fromField f bs
instance ToField (DBRef a) where toField (DBRef k) = toField k


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
