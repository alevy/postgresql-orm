{-# LANGUAGE DeriveDataTypeable, DefaultSignatures,
    FlexibleContexts, FlexibleInstances, TypeOperators, OverloadedStrings #-}

module Database.PostgreSQL.ORM.Model (
    DBKeyType, DBKey(..), isNullKey
    , Model(..), ModelInfo(..), modelToInfo, modelName, primaryKey
    , LookupRow(..), UpdateRow(..), InsertRow(..)
    , GDBRef(..), DBRef, DBURef, mkDBRef
      -- * Database operations
    , findKey, findRef, save
      -- * Low-level functions providing piecemeal access to defaults
    , defaultModelInfo
    , defaultModelInfoName, defaultModelColumns, defaultModelGetPrimaryKey
    , defaultModelRead, defaultModelWrite
    , defaultModelLookupQuery, defaultModelUpdateQuery, defaultModelInsertQuery
      -- * Low-level functions for generic FromRow/ToRow
    , GFromRow(..), defaultFromRow, GToRow(..), defaultToRow
      -- * Helper functions and miscellaneous internals
    , quoteIdent, gmodelToInfo, ShowRefType(..), NormalRef(..), UniqueRef(..)
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char
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

-- | The value of keys in the database.
type DBKeyType = Int64

-- | A datatype representing the primary key of a row in the model.
-- This should be @NullKey@ for a model that is being constructed and
-- has not yet been inserted into the database, and @DBKey@ for a
-- model stored in the database.  A @DBKey@ should be the first field
-- in every 'Model' data structure (otherwise the default 'Model'
-- instance will intentionally cause a compilation error).
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


-- | Every 'Model' has a @ModelInfo@ structure associated with it.
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
  , modelPrimaryColumn :: !Int
    -- ^ The 0-based index of the primary key column in 'modelColumns'
  , modelGetPrimaryKey :: !(a -> DBKey)
    -- ^ Return the primary key of a particular model instance.
  , modelRead :: !(RowParser a)
    -- ^ Parse a database row corresponding to the model.
  , modelWrite :: !(a -> [Action])
    -- ^ Format all fields except the primary key for writing the
    -- model to the database.
  , modelLookupQuery :: !Query
    -- ^ A query template for looking up a model by its primary key.
    -- Should expect a single query parameter, namely the 'DBKey'
    -- being looked up.
  , modelUpdateQuery :: !Query
    -- ^ A query template for updating an existing 'Model' in the
    -- database.  Expects as query parameters every column of the
    -- model /except/ the primary key, followed by the primary key.
    -- (The primary key is not written to the database, just used to
    -- select the row to change.)
  , modelInsertQuery :: !Query
    -- ^ A query template for inserting a new 'Model' in the database.
    -- The query parameters are all columns /except/ the primary key.
  , modelDeleteQuery :: !Query
    -- ^ A query template for deleting a 'Model' from the database.
    -- The query paremeter is the primary key of the row to delete.
  }

instance Show (ModelInfo a) where
  show a = intercalate " " ["Model", show $ modelInfoName a
                           , show $ modelColumns a, show $ modelPrimaryColumn a
                           , "???"
                           , show $ fromQuery $ modelLookupQuery a
                           , show $ fromQuery $ modelInsertQuery a
                           , show $ fromQuery $ modelUpdateQuery a]

class GDatatypeName f where
  gDatatypeName :: f p -> String
instance (Datatype c) => GDatatypeName (M1 i c f) where 
  gDatatypeName a = datatypeName a
defaultModelInfoName :: (Generic a, GDatatypeName (Rep a)) => a -> S.ByteString
defaultModelInfoName = fromString . maybeFold. gDatatypeName . from
  where maybeFold s | h:t <- s, not (any isUpper t) = toLower h:t
                    | otherwise                     = s

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
-- of this class, then move the 'DBKey' first in your data structure.
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

quoteIdent :: S.ByteString -> S.ByteString
quoteIdent = q

fmtCols :: Bool -> [S.ByteString] -> S.ByteString
fmtCols False cs = S.intercalate ", " (map q cs)
fmtCols True cs = "(" <> fmtCols False cs <> ")"

defaultModelLookupQuery :: S.ByteString -- ^ Name of database table
                           -> [S.ByteString] -- ^ Names of columns
                           -> Int            -- ^ Index of primary key field
                           -> Query
defaultModelLookupQuery t cs pki = Query $ S.concat [
  "select ", fmtCols False cs, " from ", q t, " where ", q (cs !! pki), " = ?"
  ]

defaultModelUpdateQuery :: S.ByteString -- ^ Name of database table
                           -> [S.ByteString] -- ^ Names of columns
                           -> Int            -- ^ Index of primary key field
                           -> Query
defaultModelUpdateQuery t cs pki = Query $ S.concat [
    "update ", q t, " set "
    , S.intercalate ", " (map (\c -> q c <> " = ?") $ deleteAt pki cs)
    , " where ", q (cs !! pki), " = ?"
  ]

defaultModelInsertQuery :: S.ByteString -- ^ Name of database table
                           -> [S.ByteString] -- ^ Names of columns
                           -> Int            -- ^ Index of primary key field
                           -> Query
defaultModelInsertQuery t cs0 pki = Query $ S.concat $ [
  "insert into ", q t, " ", fmtCols True cs1, " values ("
  , S.intercalate ", " $ map (const "?") cs1
  , ") returning ", fmtCols False cs0
  ]
  where cs1 = deleteAt pki cs0

defaultModelDeleteQuery :: S.ByteString -- ^ Name of database table
                           -> [S.ByteString] -- ^ Names of columns
                           -> Int            -- ^ Index of primary key field
                           -> Query
defaultModelDeleteQuery t cs pki = Query $ S.concat [
  "delete from ", q t, " where ", q (cs !! pki), " = ?"
  ]


defaultModelInfo :: (Generic a, GToRow (Rep a), GFromRow (Rep a)
                    , GPrimaryKey0 (Rep a), GColumns (Rep a)
                    , GDatatypeName (Rep a)) => ModelInfo a
defaultModelInfo = m
  where m = Model { modelInfoName = mname
                  , modelColumns = cols
                  , modelPrimaryColumn = pki
                  , modelGetPrimaryKey = defaultModelGetPrimaryKey
                  , modelRead = defaultModelRead
                  , modelWrite = defaultModelWrite pki
                  , modelLookupQuery = defaultModelLookupQuery mname cols pki
                  , modelInsertQuery = defaultModelInsertQuery mname cols pki
                  , modelUpdateQuery = defaultModelUpdateQuery mname cols pki
                  , modelDeleteQuery = defaultModelDeleteQuery mname cols pki
                  }
        unModel :: ModelInfo a -> a
        unModel _ = undefined
        a = unModel m
        mname = defaultModelInfoName a
        pki = 0
        cols = defaultModelColumns a


-- | Class of data types representing a database table.  Provides a
-- reasonable default implementation for types that are members of the
-- 'Generic' class (using GHC's @DeriveGeneric@ extension), provided
-- two conditions hold:
--
--   1. The data type must be defined using record selector syntax.
--
--   2. The very first field of the data type must be a 'DBKey' to
--      represent the primary key.  (If the 'DBKey' does not appear
--      first in your source code, right after the opening brace, the
--      default will intentionally fail to compile.)
--
-- All information is encapsulated in a single data type, to make it
-- relatively simple to have alternate patterns.  E.g., instead of:
--
-- >   instance Model MyType
--
-- You can use:
--
-- >   instance Model MyType where modelInfo = someOtherPattern
--
-- Provided you defined your own @someOtherPattern@ function (possibly
-- using some of the low-level defaults in this file).
--
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

gmodelToInfo :: (Model a) => g a -> ModelInfo a
{-# INLINE gmodelToInfo #-}
gmodelToInfo _ = modelInfo

modelName :: (Model a) => a -> S.ByteString
{-# INLINE modelName #-}
modelName = modelInfoName . modelToInfo

primaryKey :: (Model a) => a -> DBKey
{-# INLINE primaryKey #-}
primaryKey a = modelGetPrimaryKey modelInfo a

newtype GDBRef reftype table = GDBRef DBKeyType deriving (Typeable)

class ShowRefType rt where showRefType :: r rt t -> String

instance (ShowRefType rt, Model t) => Show (GDBRef rt t) where
  showsPrec n r@(GDBRef k) = showParen (n > 10) $
    (showRefType r ++) . ("{" ++) . (mname ++) . ("} " ++) . showsPrec 11 k
    where mname = S8.unpack $ modelInfoName $ gmodelToInfo r
instance FromField (GDBRef rt t) where
  {-# INLINE fromField #-}
  fromField f bs = GDBRef <$> fromField f bs
instance ToField (GDBRef rt t) where
  {-# INLINE toField #-}
  toField (GDBRef k) = toField k

data NormalRef = NormalRef deriving (Show, Typeable)
-- | A Haskell data type representing a foreign key reference in in
-- the database.  This data type only makes sense to use when type @a@
-- is also 'Model'.
type DBRef = GDBRef NormalRef
instance ShowRefType NormalRef where showRefType _ = "DBRef"

data UniqueRef = UniqueRef deriving (Show, Typeable)
-- | A @DBURef@ is like a 'DBRef', but with an added uniqeuness
-- constraint.  In other words, if type @A@ contains a @DBURef B@,
-- then each @B@ has one (or at most one) @A@ associated with it.  By
-- contrast, if type @A@ contains a @'DBRef' B@, then each @B@ may be
-- associated with many rows of type @A@.
type DBURef = GDBRef UniqueRef
instance ShowRefType UniqueRef where showRefType _ = "DBURef"

mkDBRef :: (Model a) => a -> GDBRef rt a
mkDBRef a
  | (DBKey k) <- primaryKey a = GDBRef k
  | otherwise = error $ "mkDBRef " ++ S8.unpack (modelName a) ++ ": NullKey"


-- | A newtype wrapper in the 'FromRow' class, permitting every model
-- to used as the result of a database query.
newtype LookupRow a = LookupRow { lookupRow :: a } deriving (Show, Typeable)
instance (Model a) => FromRow (LookupRow a) where
  fromRow = LookupRow <$> modelRead modelInfo

-- | A newtype wrapper in the 'ToRow' class, which marshalls every
-- field except the primary key.  For use with 'modelInsertQuery'.
newtype InsertRow a = InsertRow a deriving (Show, Typeable)
instance (Model a) => ToRow (InsertRow a) where
  toRow (InsertRow a) = modelWrite modelInfo a

-- | A newtype wrapper in the 'ToRow' class, which marshalls every
-- field except the primary key, followed by the primary key.  For use
-- with 'modelUpdateQuery'.
newtype UpdateRow a = UpdateRow a deriving (Show, Typeable)
instance (Model a) => ToRow (UpdateRow a) where
  toRow (UpdateRow a) = toRow $ InsertRow a :. Only (primaryKey a)

findKey :: (Model r) => Connection -> DBKey -> IO (Maybe r)
findKey _ NullKey = error "findKey: NullKey"
findKey c k = action
  where getInfo :: (Model r) => IO (Maybe r) -> ModelInfo r
        getInfo _ = modelInfo
        m = getInfo action
        action = do rs <- query c (modelLookupQuery m) (Only k)
                    case rs of [r] -> return $ Just $ lookupRow $ r
                               _   -> return Nothing

findRef :: (Model r) => Connection -> GDBRef rt r -> IO (Maybe r)
findRef c (GDBRef k) = findKey c (DBKey k)

-- | Write a 'Model' to the database.  If the primary key is
-- 'NullKey', the item is written with an @INSERT@ query, read back
-- from the database, and returned with its primary key filled in.  If
-- the primary key is not 'NullKey', then the 'Model' is writen with
-- an @UPDATE@ query and returned as-is.
save :: (Model r) => Connection -> r -> IO r
save c r | NullKey <- primaryKey r = do
               rs <- query c (modelInsertQuery m) (InsertRow r)
               case rs of [r'] -> return $ lookupRow r'
                          _    -> fail "save: database did not return row"
         | otherwise = do
               n <- execute c (modelUpdateQuery m) (UpdateRow r)
               case n of 1 -> return r
                         _ -> fail $ "save: database updated " ++ show n
                                     ++ " records"
  where m = modelToInfo r

destroyByRef :: (Model a) => Connection -> GDBRef rt a -> IO ()
destroyByRef c a =
  void $ execute c (modelDeleteQuery $ gmodelToInfo a) (Only a)

destroy :: (Model a) => Connection -> a -> IO ()
destroy c a =
  void $ execute c (modelDeleteQuery $ modelToInfo a) (Only $ primaryKey a)
