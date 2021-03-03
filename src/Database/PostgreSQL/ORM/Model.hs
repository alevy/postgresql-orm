{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

-- | The main database ORM interface. This module contains
-- functionality for moving a Haskell data structure in and out of a
-- database table.
--
-- The most important feature is the 'Model' class, which encodes a
-- typed database interface (i.e., the ORM layer). This class has a
-- default implementation for types that are members of the 'Generic'
-- class (using GHC's @DeriveGeneric@ extension), provided the
-- following conditions hold:
--
--   1. The data type must have a single constructor that is defined
--      using record selector syntax.
--
--   2. The very first field of the data type must be a 'DBKey' to
--      represent the primary key.  Other orders will cause a
--      compilation error.
--
--   3. Every field of the data structure must be an instance of
--      'FromField' and 'ToField'.
--
-- If these three conditions hold and your database naming scheme
-- follows the conventions of 'defaultModelInfo'--namely that the
-- table name is the same as the type name with the first character
-- downcased, and the field names are the same as the column
-- names--then it is reasonable to have a completely empty (default)
-- instance declaration:
--
-- >   data MyType = MyType { myKey :: !DBKey
-- >                        , myName :: !S.ByteString
-- >                        , myCamelCase :: !Int
-- >                        , ...
-- >                        } deriving (Show, Generic)
-- >   instance Model MyType
--
-- The default 'modelInfo' method is called 'defaultModelInfo'. You
-- may wish to use almost all of the defaults, but tweak a few things.
-- This is easily accomplished by overriding a few fields of the
-- default structure. For example, suppose your database columns use
-- exactly the same name as your Haskell field names, but the name of
-- your database table is not the same as the name of the Haskell data
-- type. You can override the database table name (field 'modelTable')
-- as follows:
--
-- >   instance Model MyType where
-- >       modelInfo = defaultModelInfo { modelTable = "my_type" }
--
-- Finally, if you dislike the conventions followed by
-- 'defaultModelInfo', you can simply implement an alternate pattern.
-- An example of this is 'underscoreModelInfo', which strips a prefix
-- off every field name and converts everything from camel-case to
-- underscore notation:
--
-- >   instance Model MyType where
-- >       modelInfo = underscoreModelInfo "my"
--
-- The above code will associate @MyType@ with a database table
-- @my_type@ having column names @key@, @name@, @camel_case@, etc.
--
-- You can implement other patterns like 'underscoreModelInfo' by
-- calling 'defaultModelInfo' and modifying the results.
-- Alternatively, you can directly call the lower-level functions from
-- which 'defaultModelInfo' is built ('defaultModelTable',
-- 'defaultModelColumns', 'defaultModelGetPrimaryKey').
module Database.PostgreSQL.ORM.Model (
      -- * The Model class
      Model(..), ModelInfo(..), ModelIdentifiers(..), ModelQueries(..)
    , underscoreModelInfo
      -- * Data types for holding primary keys
    , DBKeyType, DBKey(..), isNullKey
    , DBRef, DBRefUnique, GDBRef(..), mkDBRef
      -- * Database operations on Models
    , findAll, findRow, save, save_, trySave, destroy, destroyByRef
      -- * Functions for accessing and using Models
    , modelName, primaryKey, modelSelectFragment
    , LookupRow(..), UpdateRow(..), InsertRow(..)
      -- * Table aliases
    , As(..), fromAs, toAs, RowAlias(..)
      -- * Low-level functions providing manual access to defaults
    , defaultModelInfo
    , defaultModelTable, defaultModelColumns, defaultModelGetPrimaryKey
    , defaultModelIdentifiers
    , defaultModelWrite
    , defaultModelQueries
    , defaultModelLookupQuery, defaultModelUpdateQuery
    , defaultModelInsertQuery, defaultModelDeleteQuery
      -- * Helper functions and miscellaneous internals
    , quoteIdent, NormalRef(..), UniqueRef(..)
    , ModelCreateInfo(..), emptyModelCreateInfo
    , defaultFromRow, defaultToRow
    , printq
      -- ** Helper classes
      -- $HelperClasses
    , GPrimaryKey0, GColumns, GDatatypeName
    , GFromRow, GToRow
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Char
import Data.Data
import Data.Int
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid
import Data.List hiding (find)
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.ORM.Validations
import GHC.Generics

import Database.PostgreSQL.Escape (quoteIdent)

-- | A type large enough to hold database primary keys.  Do not use
-- this type directly in your data structures.  Use 'DBKey' to hold a
-- `Model`'s primary key and 'DBRef' to reference the primary key of
-- another model.
type DBKeyType = Int64

-- | The type of the Haskell data structure field containing a model's
-- primary key.
--
-- Every 'Model' must have exactly one @DBKey@, and the @DBKey@ must
-- be the `Model`'s very first field in the Haskel data type
-- definition.  (The ordering is enforced by
-- 'defaultModelGetPrimaryKey', which, through use of the
-- @DeriveGeneric@ extension, fails to compile when the first field is
-- not a @DBKey@.)
--
-- Each 'Model' stored in the database should have a unique non-null
-- primary key.  However, the key is determined at the time the
-- 'Model' is inserted into the database.  While you are constructing
-- a new 'Model' to insert, you will not have its key.  Hence, you
-- should use the value @NullKey@ to let the database chose the key.
--
-- If you wish to store a `Model`'s primary key as a reference in
-- another 'Model', do not copy the 'DBKey' structure.  Use 'mkDBRef'
-- to convert the `Model`'s primary key to a foreign key reference.
data DBKey = DBKey !DBKeyType | NullKey deriving (Data, Typeable, Generic)

instance A.ToJSON DBKey where
  toJSON NullKey = A.Null
  toJSON (DBKey k) = A.toJSON k

instance A.FromJSON DBKey where
  parseJSON (A.Number a) = return $ DBKey (floor a)
  parseJSON A.Null = return NullKey
  parseJSON _ = fail "Expected Number or Null"

instance Eq DBKey where
  (DBKey a) == (DBKey b) = a == b
  _         == _         = error "compare NullKey"
instance Ord DBKey where
  compare (DBKey a) (DBKey b) = compare a b
  compare _ _                 = error "compare NullKey"

instance Show DBKey where
  showsPrec n (DBKey k) = showsPrec n k
  showsPrec _ NullKey   = ("NullKey" ++)

instance FromField DBKey where
  fromField _ Nothing = pure NullKey
  fromField f bs      = DBKey <$> fromField f bs
instance ToField DBKey where
  toField (DBKey k) = toField k
  toField NullKey   = toField Null

-- | Returns 'True' when a 'DBKey' is 'NullKey'.
isNullKey :: DBKey -> Bool
isNullKey NullKey = True
isNullKey _       = False


-- | Many operations can take either a 'DBRef' or a 'DBRefUnique'
-- (both of which consist internally of a 'DBKeyType').  Hence, these
-- two types are just type aliases to a generalized reference type
-- @GDBRef@, where @GDBRef@'s first type argument, @reftype@, is a
-- phantom type denoting the flavor of reference ('NormalRef' or
-- 'UniqueRef').
newtype GDBRef reftype table = DBRef DBKeyType
  deriving (Eq, Data, Typeable, Num, Integral, Real, Ord, Enum, Bounded, Generic)

instance A.ToJSON (GDBRef t a) where
  toJSON (DBRef k) = A.toJSON k

instance A.FromJSON (GDBRef t a) where
  parseJSON (A.Number n) = return $ DBRef (floor n)
  parseJSON _ = fail "Expected Number"

instance (Model t) => Show (GDBRef rt t) where
  showsPrec n (DBRef k) = showsPrec n k
instance (Model t) => Read (GDBRef rt t) where
  readsPrec n str = map wrap $ readsPrec n str
    where wrap (k, s) = (DBRef k, s)
instance FromField (GDBRef rt t) where
  {-# INLINE fromField #-}
  fromField f bs = DBRef <$> fromField f bs
instance ToField (GDBRef rt t) where
  {-# INLINE toField #-}
  toField (DBRef k) = toField k

-- | Phantom type for instantiating 'GDBRef' that represents a one-to-many
-- relationship between tables.
data NormalRef = NormalRef deriving (Show, Data, Typeable)

-- | A @DBRef T@ represents a many-to-one relationship between tables. For
-- example, if type @A@ contains a @DBRef B@, then each @B@ is associated
-- with many @A@'s. By contrast, a @'DBRefUnique'@ represents a one-to-one
-- relationship.
--
-- @DBRef@ is a type alias of kind @* -> *@.  The type @DBRef T@
-- references an instance of type @T@ by the primary key of its
-- database row. The type argument @T@ should be an instance of
-- 'Model'.
type DBRef = GDBRef NormalRef

-- | Phantom type for instantiating 'GDBRef' that represents a one-to-one
-- relationship between tables.
data UniqueRef = UniqueRef deriving (Show, Data, Typeable)

-- | A @DBRefUnique T@ represents a one-to-one relationship between types. For
-- example, if type @A@ contains a @DBRefUnique B@, then each @A@ is associated
-- with one (or at most one) @B@, and each @B@ has one (or at most one) @A@
-- associated with it.
--
-- By contrast, a @'DBRef'@ represents a many-to-one relationship.
type DBRefUnique = GDBRef UniqueRef
-- Functionally, @DBRefUnique@ and @DBRef@ are treated the same by
-- this module.  However, other modules make a distinction.  In
-- particular, the 'modelCreateStatement' corresponding to a
-- 'DBRefUnique' will include a @UNIQUE@ constraint.

-- | Create a reference to the primary key of a 'Model', suitable for
-- storing in a 'DBRef' or 'DBRefUnique' field of a different 'Model'.
mkDBRef :: (Model a) => a -> GDBRef rt a
mkDBRef a
  | (DBKey k) <- primaryKey a = DBRef k
  | otherwise = error $ "mkDBRef " ++ S8.unpack (modelName a) ++ ": NullKey"


-- | A @ModelInfo T@ contains the information necessary for mapping
-- @T@ to a database table.  Each @'Model'@ type has a single
-- @ModelInfo@ associated with it, accessible through the 'modelInfo'
-- method of the 'Model' class.  Note the table and column names must
-- all be unquoted in this data structure, as they will later be
-- quoted using 'quoteIdent' by the 'modelIdentifiers' method.
data ModelInfo a = ModelInfo {
    modelTable :: !S.ByteString
    -- ^ The name of the database table corresponding to this model.
    -- The default 'modelInfo' instance uses 'defaultModelTable',
    -- which is the name of your data type with the first letter
    -- downcased.
  , modelColumns :: ![S.ByteString]
    -- ^ The names of the database columns corresponding to fields of
    -- this model.  The column names should appear in the order in
    -- which the fields are defined in the Haskell data type @a@
    -- (which should also be the order in which 'modelRead' parses
    -- them to an @a@ and 'modelWrite' marshalls them).
    --
    -- Note that all queries generated by the library specify explicit
    -- column names.  Hence the order of columns does not need to
    -- match their order in the database table.  They should instead
    -- match the order of fields in the Haskell data structure.
    --
    -- The default, given by 'defaultModelColumns', is to use the
    -- Haskell field names for @a@.  This default will fail to compile
    -- if @a@ is not defined using record syntax.
  , modelPrimaryColumn :: !Int
    -- ^ The 0-based index of the primary key column in
    -- 'modelColumns'.  This should be 0 when your data structure's
    -- first field is its 'DBKey' (highly recommended, and required by
    -- 'defaultModelGetPrimaryKey').  If you customize this field, you
    -- must also customize 'modelGetPrimaryKey'--no check is made that
    -- the two are consistent.
  , modelGetPrimaryKey :: !(a -> DBKey)
    -- ^ Return the primary key of a particular model instance.  If
    -- you customize this field, you must also customize
    -- 'modelPrimaryColumn'--no check is made that the two are
    -- consistent.
  }

instance Show (ModelInfo a) where
  show a = intercalate " " [
      "Model", show $ modelTable a, show $ modelColumns a
    , show $ modelPrimaryColumn a , "?"]


-- $HelperClasses
--
-- These classes are used internally to manipulate the 'Rep'
-- representations of 'Generic' data structures.  You should not be
-- defining instances of or using these classes directly.  The names
-- are exported so that you can include them in the context of the
-- type signatures of your functions, should you wish to make use of
-- the various @default@... funcitons in this file.

-- | This class returns the name of a datatype.
class GDatatypeName f where
  gDatatypeName :: f p -> String
instance (Datatype c) => GDatatypeName (D1 c f) where
  gDatatypeName a = datatypeName a
-- | The default name of the database table corresponding to a Haskell
-- type.  The default is the same as the type name with the first
-- letter converted to lower-case.  (The rationale is that Haskell
-- requires types to start with a capital letter, but all-lower-case
-- table names are easier to use in queries because PostgreSQL
-- generally does not require them to be quoted.)
defaultModelTable :: (Generic a, GDatatypeName (Rep a)) => a -> S.ByteString
defaultModelTable = fromString . caseFold. gDatatypeName . from
  where caseFold (h:t) = toLower h:t
        caseFold s     = s

-- | This class extracts the field names of a Haskell data structure. Only
-- defined for types with a single constructor that uses record syntax.
class GColumns f where
  gColumns :: f p -> [S.ByteString]
instance GColumns U1 where
  gColumns _ = []
instance (Selector c) => GColumns (M1 S c f) where
  gColumns s = [fromString $ selName s]
instance (GColumns a, GColumns b) => GColumns (a :*: b) where
  gColumns ~(a :*: b) = gColumns a ++ gColumns b
instance (GColumns f) => GColumns (M1 C c f) where
  gColumns ~(M1 fp) = gColumns fp
instance (GColumns f) => GColumns (M1 D c f) where
  gColumns ~(M1 fp) = gColumns fp
-- | Returns the Haskell field names in a data structure.
defaultModelColumns :: (Generic a, GColumns (Rep a)) => a -> [S.ByteString]
defaultModelColumns = gColumns . from

-- | This class extracts the first field in a data structure when the
-- field is of type 'DBKey'.  If you get a compilation error because
-- of this class, then move the 'DBKey' first in your data structure.
class GPrimaryKey0 f where
  gPrimaryKey0 :: f p -> DBKey
instance GPrimaryKey0 (S1 c (K1 i DBKey)) where
  {-# INLINE gPrimaryKey0 #-}
  gPrimaryKey0 (M1 (K1 k)) = k
instance (GPrimaryKey0 a) => GPrimaryKey0 (a :*: b) where
  {-# INLINE gPrimaryKey0 #-}
  gPrimaryKey0 (a :*: _) = gPrimaryKey0 a
instance (GPrimaryKey0 f) => GPrimaryKey0 (C1 c f) where
  {-# INLINE gPrimaryKey0 #-}
  gPrimaryKey0 (M1 fp) = gPrimaryKey0 fp
instance (GPrimaryKey0 f) => GPrimaryKey0 (D1 c f) where
  {-# INLINE gPrimaryKey0 #-}
  gPrimaryKey0 (M1 fp) = gPrimaryKey0 fp

-- | Extract the primary key of type 'DBKey' from a model when the
-- 'DBKey' is the first element of the data structure.  Fails to
-- compile if the first field is not of type 'DBKey'.
defaultModelGetPrimaryKey :: (Generic a, GPrimaryKey0 (Rep a)) => a -> DBKey
{-# INLINE defaultModelGetPrimaryKey #-}
defaultModelGetPrimaryKey = gPrimaryKey0 . from


class GFromRow f where
  gFromRow :: RowParser (f p)
instance GFromRow U1 where
  {-# INLINE gFromRow #-}
  gFromRow = return U1
instance (FromField c) => GFromRow (K1 i c) where
  {-# INLINE gFromRow #-}
  gFromRow = K1 <$> field
instance (GFromRow a, GFromRow b) => GFromRow (a :*: b) where
  {-# INLINE gFromRow #-}
  gFromRow = (:*:) <$> gFromRow <*> gFromRow
instance (GFromRow f) => GFromRow (M1 i c f) where
  {-# INLINE gFromRow #-}
  gFromRow = M1 <$> gFromRow
-- | This function provides a 'fromRow' function for 'Generic' types,
-- suitable as a default of the 'FromRow' class.  This module uses it
-- as the default implementation of 'modelRead'.
defaultFromRow :: (Generic a, GFromRow (Rep a)) => RowParser a
{-# INLINE defaultFromRow #-}
defaultFromRow = to <$> gFromRow


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
-- | This function provides a 'toRow' function for 'Generic' types
-- that marshalls each field of the data type in the order in which it
-- appears in the type definition.  This function is /not/ a suitable
-- implementation of 'modelWrite' (since it marshals the primary key,
-- which is not supposed to be written).  However, it is required
-- internally by 'defaultModelWrite', and exposed in the unlikely
-- event it is of use to alternate generic 'modelWrite' functions.
-- You probably don't want to call this function.
defaultToRow :: (Generic a, GToRow (Rep a)) => a -> [Action]
defaultToRow = gToRow . from

-- | Removes a single element from the list at the position specified.
-- (Internal)
deleteAt :: Int -> [a] -> [a]
deleteAt 0 (_:t) = t
deleteAt n (h:t) = h:deleteAt (n-1) t
deleteAt _ _     = []

-- | Returns a series of 'Action's serializing each field of a data
-- structure (in the order of the Haskell datatype definition),
-- /except/ the primary key, since the primary key should never be
-- written to a database.  Every field must be an instance of
-- 'ToField'.
defaultModelWrite :: forall a. (Model a, Generic a, GToRow (Rep a))
                  => a -> [Action]
{-# INLINE defaultModelWrite #-}
defaultModelWrite a = deleteAt pki $ defaultToRow a
  where pki = modelPrimaryColumn (modelInfo :: ModelInfo a)

-- | The default definition of 'modelInfo'. See the documentation at
-- 'Model' for more information.  Sets 'modelTable' to the name of the
-- type with the first character converted to lower-case.  Sets
-- 'modelColumns' to the names of the Haskell field selectors.  Sets
-- 'modelPrimaryColumn' to @0@ and extracts the first field of the
-- structure for 'modelGetPrimaryKey'.  Will fail to compile unless
-- the data structure is defined with record syntax and that its first
-- field is of type 'DBKey'.
--
-- Note that defaults for the individual fields are available in
-- separate functions (e.g., 'defaultModelTable') with fewer class
-- requirements in the context, in case you want to make piecemeal use
-- of defaults.  The default for 'modelPrimaryColumn' is 0.  If you
-- overwrite that, you will need to overwrite 'modelGetPrimaryKey' as
-- well (and likely vice versa).
defaultModelInfo :: forall a.
                    (Generic a, GDatatypeName (Rep a), GColumns (Rep a)
                    , GPrimaryKey0 (Rep a)) => ModelInfo a
defaultModelInfo = m
  where m = ModelInfo { modelTable = defaultModelTable a
                      , modelColumns = defaultModelColumns a
                      , modelPrimaryColumn = 0
                      , modelGetPrimaryKey = defaultModelGetPrimaryKey
                      }
        a = undefined :: a

-- | An alternate 'Model' pattern in which Haskell type and field
-- names are converted from camel-case to underscore notation.  The
-- first argument is a prefix to be removed from field names (since
-- Haskell requires field names to be unique across data types, while
-- SQL allows the same column names to be used in different tables).
--
-- For example:
--
-- > data Bar = Bar {
-- >     barId :: !DBKey
-- >   , barNameOfBar :: !String
-- >   , barParent :: !(Maybe (DBRef Bar))
-- >   } deriving (Show, Generic)
-- >
-- > instance Model Bar where modelInfo = underscoreModelInfo "bar"
--
-- would associate type @Bar@ with a database table called @bar@ with
-- fields @id@, @name_of_bar@, and @parent@.
underscoreModelInfo :: (Generic a, GToRow (Rep a), GFromRow (Rep a)
                       , GPrimaryKey0 (Rep a), GColumns (Rep a)
                       , GDatatypeName (Rep a)) =>
                       S.ByteString -> ModelInfo a
underscoreModelInfo prefix = def {
      modelTable = toUnderscore True $ modelTable def
    , modelColumns = map fixCol $ modelColumns def
    }
  where def = defaultModelInfo
        plen = S.length prefix
        fixCol c = toUnderscore False $ stripped
          where stripped | prefix `S.isPrefixOf` c = S.drop plen c
                         | otherwise               = c

-- | Convert a name from camel-case to underscore notation.
-- I.e., names of the form "MSizeForm" are changed to "MSize_From".
-- @skipFirst@ determines if the first character should be ignored
-- in the conversion.
toUnderscore :: Bool -> S.ByteString -> S.ByteString
toUnderscore skipFirst | skipFirst = S8.pack . skip . S8.unpack
                       | otherwise = S8.pack . go True . S8.unpack
  where skip "" = ""
        skip (h:t) = toLower h : go True t
        go _ ""                      = ""
        go _ (h:t) | not (isUpper h) = h : go False t
        go True (h:t)                = toLower h : go True t
        go False (h:t)               = '_' : toLower h : go True t


-- | SQL table and column identifiers that should be copied verbatim
-- into queries.  For normal models, these will simply be quoted
-- versions of the fields in the corresponding 'ModelInfo'.  However,
-- for special cases, the fields of this structure can contain
-- unquoted SQL including @JOIN@ keywords.  In the case of joins,
-- different elements of 'modelQColumns' may be qualified by different
-- table names.
--
-- Note that 'modelQColumns' and 'modelQPrimaryColumn' both contain
-- table-qualified names (e.g., @\"\\\"my_type\\\".\\\"key\\\"\"@),
-- while 'modelQWriteColumns' contains only the quoted column names.
data ModelIdentifiers a = ModelIdentifiers {
    modelQTable :: !S.ByteString
    -- ^ Literal SQL for the name of the table.
  , modelQColumns :: ![S.ByteString]
    -- ^ Literal SQL for each, table-qualified column.
  , modelQPrimaryColumn :: S.ByteString
    -- ^ Literal SQL for the model's table-qualified primary key
    -- column.
  , modelQWriteColumns :: [S.ByteString]
    -- ^ Literal SQL for all the columns except the primary key.
    -- These are the columns that should be included in an @INSERT@ or
    -- @UPDATE@.  Note that unlike the other fields, these column
    -- names should /not/ be table-qualified.
  , modelQualifier :: !(Maybe S.ByteString)
    -- ^ When all columns in 'modelQColumns' are qualified by the same
    -- table name, this field contains 'Just' the table name.
    -- For the ':.' type (in which different columns have different
    -- table qualifications), this field is 'Nothing'.
    --
    -- For normal models, this field will be identical to
    -- 'modelQTable'.  However, for 'As' models, 'modelQTable' will
    -- contain unquoted SQL such as @\"\\\"MyType\\\" AS
    -- \\\"my_alias\\\"\"@, in which case @modelQualifier@ will
    -- contain @'Just' \"\\\"my_alias\\\"\"@.
  , modelOrigTable :: !(Maybe S.ByteString)
    -- ^ The original, unquoted name of the table representing the
    -- model in the database.  Ordinarily, this should be the same as
    -- 'modelTable' in 'ModelInfo', but in the case of 'As' aliases,
    -- the 'modelTable' is an alias, and 'modelOrigTable' is the
    -- original table.  'Nothing' for joins.
  } deriving (Show)

-- | The default simply quotes the 'modelInfo' and 'modelColumns'
-- fields of 'ModelInfo' using 'quoteIdent'.
defaultModelIdentifiers :: ModelInfo a -> ModelIdentifiers a
defaultModelIdentifiers mi = ModelIdentifiers {
    modelQTable = qtable
  , modelQColumns = qcols
  , modelQPrimaryColumn = qcols !! pki
  , modelQWriteColumns = deleteAt pki $ map quoteIdent $ modelColumns mi
  , modelQualifier = Just qtable
  , modelOrigTable = Just $ modelTable mi
  }
  where qtable = quoteIdent (modelTable mi)
        qcol c = S.concat [qtable, ".", quoteIdent c]
        qcols = map qcol $ modelColumns mi
        pki = modelPrimaryColumn mi

-- | Standard CRUD (create\/read\/update\/delete) queries on a model.
data ModelQueries a = ModelQueries {
    modelLookupQuery :: !Query
    -- ^ A query template for looking up a model by its primary key.
    -- Expects a single query parameter, namely the 'DBKey' or 'DBRef'
    -- being looked up.
  , modelUpdateQuery :: !Query
    -- ^ A query template for updating an existing 'Model' in the
    -- database.  Expects as query parameters a value for every column
    -- of the model /except/ the primary key, followed by the primary
    -- key.  (The primary key is not written to the database, just
    -- used to select the row to change.)
  , modelInsertQuery :: !Query
    -- ^ A query template for inserting a new 'Model' in the database.
    -- The query parameters are values for all columns /except/ the
    -- primary key.  The query returns the full row as stored in the
    -- database (including the values of fields, such as the primary
    -- key, that have been chosen by the database server).
  , modelDeleteQuery :: !Query
    -- ^ A query template for deleting a 'Model' from the database.
    -- Should have a single query parameter, namely the 'DBKey' of the
    -- row to delete.
  } deriving (Show)

-- | Default SQL lookup query for a model.
defaultModelLookupQuery :: ModelIdentifiers a -> Query
defaultModelLookupQuery mi = Query $ S.concat [
  modelSelectFragment mi, " WHERE ", modelQPrimaryColumn mi, " = ?"
  ]

-- | Default SQL update query for a model.
defaultModelUpdateQuery :: ModelIdentifiers a -> Query
defaultModelUpdateQuery mi = Query $ S.concat [
    "UPDATE ", modelQTable mi, " SET "
    , S.intercalate ", " $ map (<> " = ?") $ modelQWriteColumns mi
    , " WHERE ", modelQPrimaryColumn mi, " = ?"
    , " RETURNING ", S.intercalate ", " (modelQColumns mi)
  ]

-- | Default SQL insert query for a model.
defaultModelInsertQuery :: ModelIdentifiers a -> Query
defaultModelInsertQuery mi
  | null (modelQWriteColumns mi) = Query $ S.concat [
    "INSERT INTO ", modelQTable mi, " DEFAULT VALUES RETURNING "
    , S.intercalate ", " $ modelQColumns mi ]
  | otherwise = Query $ S.concat $ [
  "INSERT INTO ", modelQTable mi
  , " (", S.intercalate ", " $ modelQWriteColumns mi
  , ") VALUES (", S.intercalate ", " $ map (const "?") $ modelQWriteColumns mi
  , ") RETURNING ", S.intercalate ", " $ modelQColumns mi
  ]

-- | Default SQL delete query for a model.
defaultModelDeleteQuery :: ModelIdentifiers a -> Query
defaultModelDeleteQuery mi = Query $ S.concat [
  "DELETE FROM ", modelQTable mi
  , " WHERE ", modelQPrimaryColumn mi, " = ?"
  ]

-- | The default value of 'modelQueries'.
defaultModelQueries :: ModelIdentifiers a -> ModelQueries a
defaultModelQueries mi = ModelQueries {
    modelLookupQuery = defaultModelLookupQuery mi
  , modelUpdateQuery = defaultModelUpdateQuery mi
  , modelInsertQuery = defaultModelInsertQuery mi
  , modelDeleteQuery = defaultModelDeleteQuery mi
  }

-- | Extra information for "Database.PostgreSQL.ORM.CreateTable".  You
-- probably don't need to use this.
data ModelCreateInfo a = ModelCreateInfo {
    modelCreateColumnTypeExceptions :: ![(S.ByteString, S.ByteString)]
    -- ^ A list of (column-name, type) pairs for which you want to
    -- override the default.
  , modelCreateExtraConstraints :: !S.ByteString
    -- ^ Extra constraints to stick at the end of the @CREATE TABLE@
    -- statement.
  } deriving (Show)

-- | A 'ModelCreateInfo' that doesn't imply any extra constraints or
-- exceptions.
emptyModelCreateInfo :: ModelCreateInfo a
emptyModelCreateInfo = ModelCreateInfo {
    modelCreateColumnTypeExceptions = []
  , modelCreateExtraConstraints = S.empty
  }

-- | The class of data types that represent a database table.  This
-- class conveys information necessary to move a Haskell data
-- structure in and out of a database table.  The most important field
-- is 'modelInfo', which describes the database table and column
-- names.  'modelInfo' has a reasonable default implementation for
-- types that are members of the 'Generic' class (using GHC's
-- @DeriveGeneric@ extension), provided the following conditions hold:
--
--   1. The data type must have a single constructor that is defined
--      using record selector syntax.
--
--   2. The very first field of the data type must be a 'DBKey' to
--      represent the primary key.  Other orders will cause a
--      compilation error.
--
--   3. Every field of the data structure must be an instance of
--      'FromField' and 'ToField'.
--
-- If these three conditions hold and your database naming scheme
-- follows the conventions of 'defaultModelInfo'--namely that the
-- table name is the same as the type name with the first character
-- downcased, and the field names are the same as the column
-- names--then it is reasonable to have a completely empty (default)
-- instance declaration:
--
-- >   data MyType = MyType { myKey :: !DBKey
-- >                        , myName :: !S.ByteString
-- >                        , myCamelCase :: !Int
-- >                        , ...
-- >                        } deriving (Show, Generic)
-- >   instance Model MyType
--
-- The default 'modelInfo' method is called 'defaultModelInfo'.  You
-- may wish to use almost all of the defaults, but tweak a few things.
-- This is easily accomplished by overriding a few fields of the
-- default structure.  For example, suppose your database columns use
-- exactly the same name as your Haskell field names, but the name of
-- your database table is not the same as the name of the Haskell data
-- type.  You can override the database table name (field
-- 'modelTable') as follows:
--
-- >   instance Model MyType where
-- >       modelInfo = defaultModelInfo { modelTable = "my_type" }
--
-- Finally, if you dislike the conventions followed by
-- 'defaultModelInfo', you can simply implement an alternate pattern.
-- An example of this is 'underscoreModelInfo', which strips a prefix
-- off every field name and converts everything from camel-case to
-- underscore notation:
--
-- >   instance Model MyType where
-- >       modelInfo = underscoreModelInfo "my"
--
-- The above code will associate @MyType@ with a database table
-- @my_type@ having column names @key@, @name@, @camel_case@, etc.
--
-- You can implement other patterns like 'underscoreModelInfo' by
-- calling 'defaultModelInfo' and modifying the results.
-- Alternatively, you can directly call the lower-level functions from
-- which 'defaultModelInfo' is built ('defaultModelTable',
-- 'defaultModelColumns', 'defaultModelGetPrimaryKey').
class Model a where
  -- | @modelInfo@ provides information about how the Haskell data
  -- type is stored in the database, in the form of a 'ModelInfo' data
  -- structure.  Among other things, this structure specifies the name
  -- of the database table, the names of the database columns
  -- corresponding to the Haskell data structure fields, and the
  -- position of the primary key in both the database columns and the
  -- Haskell data structure.
  modelInfo :: ModelInfo a
  default modelInfo :: (Generic a, GDatatypeName (Rep a), GColumns (Rep a)
                       , GPrimaryKey0 (Rep a)) => ModelInfo a
  {-# INLINE modelInfo #-}
  modelInfo = defaultModelInfo

  -- | 'modelIdentifiers' contains the table and column names verbatim
  -- as they should be inserted into SQL queries.  For normal models,
  -- these are simply double-quoted (with 'quoteIdent') versions of
  -- the names in 'modelInfo', with the column names qualified by the
  -- double-quoted table name.  However, for special cases such as
  -- join relations (with ':.')  or row aliases (with 'As'),
  -- 'modelIdentifiers' can modify the table name with unquoted SQL
  -- identifiers (such as @JOIN@ and @AS@) and change the qualified
  -- column names appropriately.
  modelIdentifiers :: ModelIdentifiers a
  {-# INLINE modelIdentifiers #-}
  modelIdentifiers = defaultModelIdentifiers modelInfo

  -- | @modelRead@ converts from a database 'query' result to the
  -- Haskell data type of the @Model@, namely @a@.  Note that if type
  -- @a@ is an instance of 'FromRow', a fine definition of @modelRead@
  -- is @modelRead = fromRow@.  The default is to construct a row
  -- parser using the 'Generic' class.  However, it is crucial that
  -- the columns be parsed in the same order they are listed in the
  -- 'modelColumns' field of @a@'s 'ModelInfo' structure, and this
  -- should generally be the same order they are defined in the
  -- Haskell data structure.  Hence @modelRead@ should generally look
  -- like:
  --
  -- @
  --   -- Call 'field' as many times as there are fields in your type
  --   modelRead = Constructor \<$> 'field' \<*> 'field' \<*> 'field'
  -- @
  modelRead :: RowParser a
  default modelRead :: (Generic a, GFromRow (Rep a)) => RowParser a
  {-# INLINE modelRead #-}
  modelRead = defaultFromRow

  -- | Marshal all fields of @a@ /except/ the primary key.  As with
  -- 'modelRead', the fields must be marshalled in the same order the
  -- corresponding columns are listed in 'modelColumns', only with the
  -- primary key (generally column 0) deleted.
  --
  -- Do /not/ define this as 'toRow', even if @a@ is an instance of
  -- 'ToRow', because 'toRow' would include the primary key.
  -- Similarly, do /not/ define this as 'defaultToRow'.  On the other
  -- hand, it is reasonable for @modelWrite@ to return an error for
  -- degenerate models (such as joins) that should never be 'save'd.
  modelWrite :: a -> [Action]
  default modelWrite :: (Generic a, GToRow (Rep a)) => a -> [Action]
  {-# INLINE modelWrite #-}
  modelWrite = defaultModelWrite

  -- | @modelQueries@ provides pre-formatted 'Query' templates for
  -- 'findRow', 'save', and 'destroy'.  The default 'modelQueries'
  -- value is generated from 'modelIdentifiers' and should not be
  -- modified.  However, for degenerate tables (such as joins created
  -- with ':.'), it is reasonable to make 'modelQueries' always throw
  -- an exception, thereby disallowing ordinary queries and requiring
  -- use of more general query functions.
  --
  -- This method should either throw an exception or use the default
  -- implementation.
  modelQueries :: ModelQueries a
  {-# INLINE modelQueries #-}
  modelQueries = defaultModelQueries modelIdentifiers

  -- | Extra constraints, if any, to place in a @CREATE TABLE@
  -- statement.  Only used by "Database.PostgreSQL.ORM.CreateTable".
  modelCreateInfo :: ModelCreateInfo a
  modelCreateInfo = emptyModelCreateInfo

  -- | Perform a validation of the model, returning any errors if
  -- it is invalid.
  modelValid :: a -> ValidationError
  modelValid = const mempty

-- | Degenerate instances of 'Model' for types in the 'ToRow' class
-- are to enable extra 'ToRow' types to be included with ':.' in the
-- result of 'dbSelect' queries.
degen_err :: a
degen_err = error "Attempt to use degenerate ToRow instance as Model"
#define DEGENERATE(ctx,t)             \
instance ctx => Model t where {       \
  modelInfo = degen_err;              \
  modelIdentifiers = degen_err;       \
  modelRead = fromRow;                \
  modelWrite _ = degen_err;           \
  modelCreateInfo = degen_err; }

DEGENERATE(FromField t, (Only t))
DEGENERATE(FromField t, [t])
DEGENERATE((FromField a, FromField b), (a, b))
DEGENERATE((FromField a, FromField b, FromField c), (a, b, c))
DEGENERATE((FromField a, FromField b, FromField c, FromField d), (a, b, c, d))
DEGENERATE((FromField a, FromField b, FromField c, FromField d, FromField e), \
           (a, b, c, d, e))

#undef DEGEN_ERR
#undef DEGENERATE

-- | A degenerate model that lifts any model to a Maybe version. Returns
-- 'Nothing' on a parse failure. Useful, for example, for performing outer
-- joins:
-- @
-- dbJoin modelDBSelect "LEFT OUTER JOIN"
--        (addWhere 'foo = 123' $ modelDBSelect)
--        "USING a.id = b.a_id" :: (A :. Maybe B)
-- @
--
instance forall a. Model a => Model (Maybe a) where
  modelInfo = mi_a { modelGetPrimaryKey = getPrimaryKey }
    where mi_a = modelInfo :: ModelInfo a
          getPrimaryKey Nothing  = NullKey
          getPrimaryKey (Just a) = modelGetPrimaryKey mi_a a

  modelIdentifiers = mi_a { modelQTable = modelQTable mi_a }
    where mi_a = modelIdentifiers :: ModelIdentifiers a

  modelQueries = mi_a { modelLookupQuery = modelLookupQuery mi_a }
    where mi_a = modelQueries :: ModelQueries a

  modelCreateInfo = error
    "Attempt to use degenerate Maybe (Model a) instance for ModelCreateInfo"

  modelValid = maybe mempty modelValid

  modelWrite = maybe [] modelWrite

  modelRead =
    Just `fmap` (modelRead :: RowParser a)
    <|> do
      let n = length $ modelColumns (modelInfo :: ModelInfo a)
      replicateM_ n (field :: RowParser AnyField)
      return Nothing

-- | AnyField parses (simply by consuming) any SQL column.
data AnyField = AnyField

instance FromField AnyField where
  fromField _ _ = pure AnyField

joinModelIdentifiers :: forall a b. (Model a, Model b)
                     => ModelIdentifiers (a :. b)
joinModelIdentifiers = r
  where r = ModelIdentifiers {
              modelQTable = qtable
            , modelQColumns = modelQColumns mia ++ modelQColumns mib
            , modelQWriteColumns = error "attempt to write join relation"
            , modelQPrimaryColumn =
              error "attempt to use primary key of join relation"
            , modelQualifier = Nothing
            , modelOrigTable = Nothing
          }
        qtable | S.null $ modelQTable mib = modelQTable mia
               | S.null $ modelQTable mia = modelQTable mib
               | otherwise = S.concat [modelQTable mia, " CROSS JOIN "
                                      , modelQTable mib]
        mia = modelIdentifiers :: ModelIdentifiers a
        mib = modelIdentifiers :: ModelIdentifiers b

-- | A degenerate instance of model representing a database join.  The
-- ':.' instance does not allow normal model operations such as
-- 'findRow', 'save', and 'destroy'.  Attempts to use such functions
-- will result in an exception.
instance (Model a, Model b) => Model (a :. b) where
  modelInfo = error "attempt to access ModelInfo of join type :."
  modelIdentifiers = joinModelIdentifiers
  modelRead = (:.) <$> modelRead <*> modelRead
  modelWrite _ = error "attempt to write join type :. as a normal Model"
  modelQueries = error "attempt to perform standard query on join type :."


class GUnitType f where
  gUnitTypeName :: f p -> String
instance GUnitType (C1 c U1) where
  gUnitTypeName _ = error "gUnitTypeName"
instance GUnitType V1 where
  gUnitTypeName _ = error "gUnitTypeName"
instance (Datatype c, GUnitType f) => GUnitType (D1 c f) where
  gUnitTypeName = datatypeName

-- | The class of types that can be used as tags in as 'As' alias.
-- Such types should be unit types--in other words, have exactly one
-- constructor where the constructor is nullary (take no arguments).
-- The reason for this class is that the 'Model' instance for 'As'
-- requires a way to extract the name of the row alias without having
-- a concrete instance of the type.  This is provided by the
-- 'rowAliasName' method (which must be non-strict).
class RowAlias a where
  rowAliasName :: g a row -> S.ByteString
  -- ^ Return the SQL identifier for the row alias.  This method must
  -- be non-strict in its argument.  Hence, it should discard the
  -- argument and return the name of the alias.  For example:
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
  -- >
  -- > data My_alias = My_alias
  -- > instance RowAlias My_alias where rowAliasName _ = "my_alias"
  --
  -- Keep in mind that PostgreSQL folds unquoted identifiers to
  -- lower-case.  However, this library quotes row aliases in @SELECT@
  -- statements, thereby preserving case.  Hence, if you want to call
  -- construct a @WHERE@ clause without double-quoting row aliases in
  -- your 'Query', you should avoid capital letters in alias names.
  --
  -- A default implementation of @rowAliasName@ exists for unit types
  -- (as well as empty data declarations) in the 'Generic' class.  The
  -- default converts the first character of the type name to
  -- lower-case, following the logic of 'defaultModelTable'.
  default rowAliasName :: (Generic a, GUnitType (Rep a)) =>
                          g a row -> S.ByteString
  rowAliasName _ = fromString $ caseFold $ gUnitTypeName . from $ a
    where caseFold (h:t) = toLower h:t -- fold first character only
          caseFold []    = []
          a = undefined :: a

-- | The newtype @As@ can be wrapped around an existing type to give
-- it a table name alias in a query.  This is necessary when a model
-- is being joined with itself, to distinguish the two joined
-- instances of the same table.
--
-- For example:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
--
--data X = X
--instance 'RowAlias' X where rowAliasName = const \"x\"
--
-- \  ...
--    r <- 'dbSelect' c $ addWhere_ \"bar.bar_key = x.bar_parent\" modelDBSelect
--         :: IO [Bar :. As X Bar]
-- @
newtype As alias row = As { unAs :: row }
instance (RowAlias alias, Show row) => Show (As alias row) where
  showsPrec d as@(As row) = showParen (d > 10) $ \rest ->
    "As " ++ S8.unpack (rowAliasName as) ++
    " (" ++ showsPrec 11 row (")" ++ rest)


-- | @fromAs@ extracts the @row@ from an @'As' alias row@, but
-- constrains the type of @alias@ to be the same as its first argument
-- (which is non-strict).  This can save you from explicitly
-- specifying types.  For example:
--
-- > data X = X deriving (Generic)
-- > instance RowAlias X where rowAliasName = const "x"
-- >
-- > ...
-- >   r <- map (\(b1 :. b2) -> (b1, fromAs X b2)) <$>
-- >       dbSelect c $ addWhere \"bar.bar_key = x.bar_parent\" modelDBSelect
fromAs :: alias -> As alias row -> row
{-# INLINE fromAs #-}
fromAs _ (As row) = row

-- | A type-restricted wrapper around the 'As' constructor, under the
-- same rationale as 'fromAs'.  Not strict in its first argument.
toAs :: alias -> row -> As alias row
{-# INLINE toAs #-}
toAs _ = As

aliasModelInfo :: forall a alias.
                  (Model a, RowAlias alias) =>
                  ModelInfo a -> ModelInfo (As alias a)
aliasModelInfo mi = r
  where alias = rowAliasName (undefined :: As alias a)
        r = mi { modelTable = alias
               , modelGetPrimaryKey = modelGetPrimaryKey mi . unAs
               }

aliasModelIdentifiers :: forall a alias. (Model a, RowAlias alias)
                      => ModelInfo a -> ModelIdentifiers (As alias a)
aliasModelIdentifiers mi
  | not ok    = error $ "aliasModelIdentifiers: degenerate model " ++
                show (modelQTable ida )
  | otherwise = r
  where r = ModelIdentifiers {
            modelQTable = S.concat [quoteIdent orig, " AS ", alias]
          , modelQColumns = qcols
          , modelQPrimaryColumn = qcols !! pki
          , modelQWriteColumns = deleteAt pki qcols
          , modelQualifier = Just alias
          , modelOrigTable = Just orig
          }
        ida = modelIdentifiers :: ModelIdentifiers a
        ok = Just (modelQTable ida) == modelQualifier ida
             && isJust (modelOrigTable ida)
        Just orig = modelOrigTable ida
        alias = quoteIdent $ rowAliasName (undefined :: As alias a)
        qcol c = S.concat [alias, ".", quoteIdent c]
        qcols = map qcol $ modelColumns mi
        pki = modelPrimaryColumn mi

-- | A degenerate instance of 'Model' that re-names the row with a SQL
-- @AS@ keyword.  This is primarily useful when joining a model with
-- itself.  Hence, standard operations ('findRow', 'save', 'destroy')
-- are not allowed on 'As' models.
instance (Model a, RowAlias as) => Model (As as a) where
  {-# INLINE modelInfo #-}
  modelInfo = aliasModelInfo modelInfo
  {-# INLINE modelRead #-}
  modelRead = As <$> modelRead
  modelWrite = error "attempt to write \"As\" alias as normal Model"
  {-# INLINE modelIdentifiers #-}
  modelIdentifiers = aliasModelIdentifiers modelInfo
  modelQueries = error "attempt to perform standard query on AS table alias"


-- | Lookup the 'modelTable' of a 'Model' (@modelName _ = 'modelTable'
-- ('modelInfo' :: 'ModelInfo' a)@).
modelName :: forall a. (Model a) => a -> S.ByteString
{-# INLINE modelName #-}
modelName _ = modelTable (modelInfo :: ModelInfo a)

-- | Lookup the primary key of a 'Model'.
primaryKey :: (Model a) => a -> DBKey
{-# INLINE primaryKey #-}
primaryKey a = modelGetPrimaryKey modelInfo a

-- | Generate a SQL @SELECT@ statement with no @WHERE@ predicate.  For
-- example, 'defaultModelLookupQuery' consists of
-- @modelSelectFragment@ followed by \"@WHERE@ /primary-key/ = ?\".
modelSelectFragment :: ModelIdentifiers a -> S.ByteString
modelSelectFragment mi = S.concat [
 "SELECT ", S.intercalate ", " $ modelQColumns mi, " FROM ", modelQTable mi ]

-- | A newtype wrapper in the 'FromRow' class, permitting every model
-- to be used as the result of a database query.
newtype LookupRow a = LookupRow { lookupRow :: a } deriving (Show)
instance (Model a) => FromRow (LookupRow a) where
  fromRow = LookupRow <$> modelRead

-- | A newtype wrapper in the 'ToRow' class, which marshalls every
-- field except the primary key.  For use with 'modelInsertQuery'.
newtype InsertRow a = InsertRow a deriving (Show)
instance (Model a) => ToRow (InsertRow a) where
  toRow (InsertRow a) = modelWrite a

-- | A newtype wrapper in the 'ToRow' class, which marshalls every
-- field except the primary key, followed by the primary key.  For use
-- with 'modelUpdateQuery'.
newtype UpdateRow a = UpdateRow a deriving (Show)
instance (Model a) => ToRow (UpdateRow a) where
  toRow (UpdateRow a) = toRow $ InsertRow a :. Only (primaryKey a)

-- | Dump an entire model.  Useful for development and debugging only,
-- as every row will be read into memory before the function returns.
--
-- Note that unlike the other primary model operations, it is OK to
-- call 'findAll' even on degenerate models such as 'As' and ':.'.
findAll :: forall r. (Model r) => Connection -> IO [r]
findAll c = action
  where mi = modelIdentifiers :: ModelIdentifiers r
        q = Query $ modelSelectFragment mi
        action = map lookupRow <$> query_ c q

-- | Follow a 'DBRef' or 'DBRefUnique' and fetch the target row from
-- the database into a 'Model' type @r@.
findRow :: forall r rt. (Model r) => Connection -> GDBRef rt r -> IO (Maybe r)
findRow c k = action
  where qs = modelQueries :: ModelQueries r
        action = do rs <- query c (modelLookupQuery qs) (Only k)
                    case rs of [r] -> return $ Just $ lookupRow $ r
                               _   -> return Nothing

-- | Like 'trySave' but instead of returning an 'Either', throws a
-- 'ValidationError' if the 'Model' is invalid.
save :: (Model r)
     => Connection -> r -> IO r
save c r = do
  eResp <- trySave c r
  case eResp of
    Right resp -> return resp
    Left  errs -> throwIO errs

-- | 'save' but returning '()' instead of the saved model.
save_ :: (Model r)
      => Connection -> r -> IO ()
save_ c r = void $ save c r

-- | Write a 'Model' to the database.  If the primary key is
-- 'NullKey', the item is written with an @INSERT@ query, read back
-- from the database, and returned with its primary key filled in.  If
-- the primary key is not 'NullKey', then the 'Model' is written with
-- an @UPDATE@ query and returned as-is.
--
-- If the 'Model' is invalid (i.e. the return value of 'modelValid' is
-- non-empty), a list of 'InvalidError' is returned instead.
trySave :: forall r. Model r
        => Connection -> r -> IO (Either ValidationError r)
trySave c r | not . H.null $ validationErrors errors = return $ Left errors
            | NullKey <- primaryKey r = do
                  rs <- query c (modelInsertQuery qs) (InsertRow r)
                  case rs of [r'] -> return $ Right $ lookupRow r'
                             _    -> fail "save: database did not return row"
            | otherwise = do
                  rows <- query c (modelUpdateQuery qs) (UpdateRow r)
                  case rows of [r'] -> return $ Right $ lookupRow r'
                               _     -> fail $ "save: database updated "
                                          ++ show (length rows)
                                          ++ " records"
  where qs = modelQueries :: ModelQueries r
        errors = modelValid r

-- | Remove the row corresponding to a particular data structure from
-- the database.  This function only looks at the primary key in the
-- data structure.  It is an error to call this function if the
-- primary key is not set.
destroy :: forall a. (Model a)
  => Connection -> a -> IO (Either ValidationError Bool)
destroy c a =
  case primaryKey a of
    NullKey -> fail "destroy: NullKey"
    DBKey k -> destroyByRef_ "destroy" c (DBRef k :: DBRef a)

-- | Remove a row from the database without fetching it first.
destroyByRef :: forall a rt. (Model a)
  => Connection -> GDBRef rt a -> IO (Either ValidationError Bool)
destroyByRef = destroyByRef_ "destroyByRef"

destroyByRef_ :: forall a rt. (Model a)
  => T.Text -> Connection -> GDBRef rt a -> IO (Either ValidationError Bool)
destroyByRef_ msg c a = action
  where mq     = modelQueries     :: ModelQueries a
        mi     = modelIdentifiers :: ModelIdentifiers a
        pkCol  = modelQPrimaryColumn mi
        action = do
            n <- execute c (modelDeleteQuery mq) (Only a)
            return $ case n of
                0 -> Right False
                1 -> Right True
                _ -> Left $ validationError (T.decodeUtf8 pkCol) $
                    msg <> ": DELETE modified " <> T.pack (show n) <>
                    " rows. This may indicate that your primary key" <>
                    " accessor field is not actually a primary key."

-- | Print to stdout the query statement.
printq :: Query -> IO ()
printq (Query bs) = S8.putStrLn bs
