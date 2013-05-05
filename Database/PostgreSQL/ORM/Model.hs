{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}

module Database.PostgreSQL.ORM.Model (
      -- * The Model class
      Model(..), ModelInfo(..), ModelQueries(..)
      -- * Data types for holding primary keys
    , DBKey(..), isNullKey
    , DBRef, DBRefUnique, GDBRef(..), mkDBRef
    , As(..), fromAs, RowAlias(..)
      -- * Database operations on Models
    , findRow, find, findWhere, findAll
    , save, destroy, destroyByRef
      -- * Functions for accessing and using Models
    , modelToInfo, gmodelToInfo, modelToQueries, gmodelToQueries
    , modelName, primaryKey, modelQKey
    , modelSelectFragment
    , LookupRow(..), UpdateRow(..), InsertRow(..)
      -- * Low-level functions providing manual access to defaults
    , defaultModelInfo
    , defaultModelTable, defaultModelColumns, defaultModelGetPrimaryKey
    , defaultModelRead, defaultModelWrite
    , defaultModelQueries
    , defaultModelQTable, defaultModelQColumns
    , defaultModelLookupQuery, defaultModelUpdateQuery
    , defaultModelInsertQuery, defaultModelDeleteQuery
      -- * Helper functions and miscellaneous internals
    , quoteIdent, NormalRef(..), UniqueRef(..)
    , printq
      -- * Low-level functions for generic FromRow/ToRow
    , GFromRow(..), defaultFromRow, GToRow(..), defaultToRow
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char
import Data.Int
import Data.Monoid
import Data.List hiding (find)
import Data.Typeable
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Data.RequireSelector

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
data DBKey = DBKey !DBKeyType | NullKey deriving (Typeable)

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


-- | Many operations can take either a 'DBRef' or a 'DBURef' (both of
-- which consist internally of a 'DBKeyType').  Hence, these two types
-- are just type aliases to a generalized reference type @GDBRef@,
-- where @GDBRef@'s first type argument, @reftype@, is a phantom type
-- denoting the flavor of reference ('NormalRef' or 'UniqueRef').
newtype GDBRef reftype table = DBRef DBKeyType deriving (Typeable, Eq)

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

-- | See 'GDBRef'.
data NormalRef = NormalRef deriving (Show, Typeable)
-- | @DBRef@ is a type alias of kind @* -> *@.  The type @DBRef T@
-- references an instance of type @T@ by the primary key of its
-- database row.  The type argument @T@ should be an instance of
-- 'Model'.
type DBRef = GDBRef NormalRef

-- | See 'GDBRef'.
data UniqueRef = UniqueRef deriving (Show, Typeable)
-- | A @DBURef T@ is like a @'DBRef' T@, but with an added uniqeuness
-- constraint.  In other words, if type @A@ contains a @DBURef B@,
-- then each @B@ has one (or at most one) @A@ associated with it.  By
-- contrast, if type @A@ contains a @'DBRef' B@, then each @B@ may be
-- associated with many rows of type @A@.
--
-- Functionally, @DBURef@ and @DBRef@ are treated the same by this
-- module.  However, other modules make a distinction.  For instance,
-- the 'HasOne' class requires the child type to point to its parent
-- with a @DBURef@, while the 'HasMany' class requires a 'DBRef'.
-- Moreover, if code creates database tables automatically, the column
-- for a 'DBURef' field should have a @UNIQUE@ constraint.
type DBRefUnique = GDBRef UniqueRef

-- | Create a reference to the primary key of a 'Model', suitable for
-- storing in a different 'Model'.
mkDBRef :: (Model a) => a -> GDBRef rt a
mkDBRef a
  | (DBKey k) <- primaryKey a = DBRef k
  | otherwise = error $ "mkDBRef " ++ S8.unpack (modelName a) ++ ": NullKey"


-- | A @ModelInfo T@ contains all of the information necessary for
-- converting type @T@ to and from database rows.  Each @'Model'@ type
-- has a single @ModelInfo@ associated with it, accessible through the
-- 'modelInfo' method.
data ModelInfo a = ModelInfo {
    modelTable :: !S.ByteString
    -- ^ The name of the database table corresponding to this model.
    -- The default is given by 'defaultModelTable'.
  , modelColumns :: ![S.ByteString]
    -- ^ The names of the database columns corresponding to fields of
    -- this model.  The column names should appear in the order in
    -- which the fields are defined in the haskell data type @a@ (or
    -- at least the order in which 'modelRead' parses them to an @a@
    -- and 'modelWrite' marshalls them).  The default, given by
    -- 'defaultModelColumns', is to use the Haskell field names for
    -- @a@.  This default will fail to compile if @a@ is not defined
    -- using record syntax.
  , modelPrimaryColumn :: !Int
    -- ^ The 0-based index of the primary key column in
    -- 'modelColumns'.  This should be 0 when your data structure's
    -- first field is its 'DBKey' (hihgly recommended, and required by
    -- 'defaultModelGetPrimaryKey').  If you customize this field, you
    -- must also customize 'modelGetPrimaryKey'--no check is made that
    -- the two are consistent.
  , modelGetPrimaryKey :: !(a -> DBKey)
    -- ^ Return the primary key of a particular model instance.  If
    -- you customize this field, you must also customize
    -- 'modelPrimaryColumn'--no check is made that the two are
    -- consistent.
  , modelRead :: !(RowParser a)
    -- ^ Parse a database row corresponding to the model.
  , modelWrite :: !(a -> [Action])
    -- ^ Format all fields except the primary key for writing the
    -- model to the database.
  , modelIsTable :: !Bool
    -- ^ True when the model corresponds to a simple table, as opposed
    -- to a join or an 'As' alias.
  }

instance Show (ModelInfo a) where
  show a = intercalate " " ["Model", show $ modelTable a
                           , show $ modelColumns a, show $ modelPrimaryColumn a
                           , "???", show $ modelIsTable a]

class GDatatypeName f where
  gDatatypeName :: f p -> String
instance (Datatype c) => GDatatypeName (D1 c f) where 
  gDatatypeName a = datatypeName a
-- | The default name of the database table corresponding to a Haskell
-- type.  The default is the same as the type name with the first
-- letter converted to lower-case.  (The rationale is that Haskell
-- requires types to start with a capital letter, but all-lower-case
-- table names are slightly easier to manipulate manually in
-- PostgreSQL because they require less quoting.)
defaultModelTable :: (Generic a, GDatatypeName (Rep a)) => a -> S.ByteString
defaultModelTable = fromString . caseFold. gDatatypeName . from
  where caseFold (h:t) = toLower h:t
        caseFold s     = s
{-
-- | The default name of the database table corresponding to a Haskell
-- type.  The default is the same as the type name, unless the first
-- letter is the only capital letter, in which case the first letter
-- is downcased.  (The rationale is that Haskell requires a
-- capitalized letter, but all-lower-case table names are slightly
-- easier to manipulate manually in PostgreSQL because they require
-- less quoting.)
defaultModelTable :: (Generic a, GDatatypeName (Rep a)) => a -> S.ByteString
defaultModelTable = fromString . maybeFold. gDatatypeName . from
  where maybeFold s | h:t <- s, not (any isUpper t) = toLower h:t
                    | otherwise                     = s
-}

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
-- | Returns the Haskell field names in a data structure.
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

-- | Returns a 'RowParser' that parses each field of a data structure
-- in the order of the Haskell data type definition.  Each field must
-- be an instance of 'FromField'.
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

-- | Returns a series of 'Action's serializing each field of a data
-- structure (in the order of the Haskell datatype definition),
-- /except/ the primary key, since the primary key should never be
-- written to a database.  Every field must be an instance of
-- 'ToField'.

-- The fist argument is the 0-based index of the primary key (which
-- should almost always be 0).  If for some reason you don't want to
-- skip the primary key, use 'defaultToRow' to marshall the full row,
-- but that probably isn't what you want.
defaultModelWrite :: (Generic a, GToRow (Rep a)) =>
                     Int        -- ^ Field number of the primary key
                     -> a       -- ^ Model to write
                     -> [Action] -- ^ Writes all fields except primary key
defaultModelWrite pki = deleteAt pki . defaultToRow

-- | The default definition of 'modelInfo'.  See the documentation at
-- 'Model' for more information.
--
-- Defaults for the individual fields are available in separate
-- functions (e.g., 'defaultModelTable') with fewer class
-- requirements, in case you want to make piecemeal use of defaults.
-- The default for 'modelPrimaryColumn' is 0.  If you overwrite that,
-- you will need to overwrite 'modelGetPrimaryKey' as well (and likely
-- vice versa).
defaultModelInfo :: (Generic a, GToRow (Rep a), GFromRow (Rep a)
                    , GPrimaryKey0 (Rep a), GColumns (Rep a)
                    , GDatatypeName (Rep a)) => ModelInfo a
defaultModelInfo = m
  where m = ModelInfo { modelTable = mname
                      , modelColumns = cols
                      , modelPrimaryColumn = pki
                      , modelGetPrimaryKey = defaultModelGetPrimaryKey
                      , modelRead = defaultModelRead
                      , modelWrite = defaultModelWrite pki
                      , modelIsTable = True
                      }
        unModel :: ModelInfo a -> a
        unModel _ = undefined
        a = unModel m
        mname = defaultModelTable a
        pki = 0
        cols = defaultModelColumns a


data ModelQueries a = ModelQueries {
    modelQTable :: !S.ByteString
    -- ^ SQL quoted identifier for the name of the table.
  , modelQColumns :: ![S.ByteString]
    -- ^ Fully-qualified and quoted SQL identifier names of all the
    -- columns.
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
    -- The query should return the full row as stored in the database
    -- (including the values of fields, such as the primary key, that
    -- have been chosen by the database server).
  , modelDeleteQuery :: !Query
    -- ^ A query template for deleting a 'Model' from the database.
    -- Should have a single query parameter, namely the 'DBKey' of the
    -- row to delete.
  } deriving (Show)

-- | Quote a SQL identifier (such as a column or field name) with
-- double quotes.  Note this has nothing to do with quoting /values/,
-- which must be quoted using single quotes.  (Anyway, all values
-- should be passed as query parameters, meaning @postgresql-simple@
-- will handle quoting them.)
quoteIdent :: S.ByteString -> S.ByteString
quoteIdent iden = fromString $ '"' : (go $ S8.unpack iden)
  where go ('"':cs) = '"':'"':go cs
        go ('\0':_) = error $ "q: illegal NUL character in " ++ show iden
        go (c:cs)   = c:go cs
        go []       = '"':[]

q :: S.ByteString -> S.ByteString
q = quoteIdent

fmtCols :: Bool -> [S.ByteString] -> S.ByteString
fmtCols False cs = S.intercalate ", " (map q cs)
fmtCols True cs = "(" <> fmtCols False cs <> ")"

defaultModelQTable :: ModelInfo a -> S.ByteString
defaultModelQTable mi = q $ modelTable mi

defaultModelQColumns :: ModelInfo a -> [S.ByteString]
defaultModelQColumns mi = map qualify $ modelColumns mi
  where qt = q $ modelTable mi
        qualify c = qt <> "." <> q c

defaultModelLookupQuery :: ModelInfo a -> Query
defaultModelLookupQuery mi = Query $ S.concat [
    "select ", S.intercalate ", " $ defaultModelQColumns mi
  , " from ", defaultModelQTable mi
  , " where ", q (modelColumns mi !! modelPrimaryColumn mi), " = ?"
  ]

defaultModelUpdateQuery :: ModelInfo a -> Query
defaultModelUpdateQuery mi = Query $ S.concat [
    "update ", q (modelTable mi), " set "
    , S.intercalate ", " (map (\c -> q c <> " = ?") $ deleteAt pki cs)
    , " where ", q (cs !! pki), " = ?"
  ]
  where cs = modelColumns mi                        
        pki = modelPrimaryColumn mi

defaultModelInsertQuery :: ModelInfo a -> Query
defaultModelInsertQuery mi = Query $ S.concat $ [
  "insert into ", q (modelTable mi), " ", fmtCols True cs1, " values ("
  , S.intercalate ", " $ map (const "?") cs1
  , ") returning ", fmtCols False cs0
  ]
  where cs0 = modelColumns mi
        cs1 = deleteAt (modelPrimaryColumn mi) cs0

defaultModelDeleteQuery :: ModelInfo a -> Query
defaultModelDeleteQuery mi = Query $ S.concat [
  "delete from ", q (modelTable mi), " where "
  , q (modelColumns mi !! modelPrimaryColumn mi), " = ?"
  ]

-- | The default value of 'modelQueries'.
defaultModelQueries :: ModelInfo a -> ModelQueries a
defaultModelQueries mi = ModelQueries {
    modelQTable = defaultModelQTable mi
  , modelQColumns = defaultModelQColumns mi
  , modelLookupQuery = defaultModelLookupQuery mi
  , modelUpdateQuery = defaultModelUpdateQuery mi
  , modelInsertQuery = defaultModelInsertQuery mi
  , modelDeleteQuery = defaultModelDeleteQuery mi
  }


-- | The class of data types that represent a database table.  This
-- class conveys two important pieces of information necessary to load
-- and save data structures from the database.
--
--   * 'modelInfo' provides information about translating between the
--     Haskell class instance and the database representation, in the
--     form of a 'ModelInfo' data structure.  Among other things, this
--     structure specifies the name of the database table, the names
--     of the database columns corresponding to the Haskell data
--     structure fields, and how to convert the data structure to and
--     from database rows.
--
--   * 'modelQueries' provides pre-formatted 'Query' templates for
--     common operations.  The default 'modelQueries' value is
--     generated from 'modelInfo' and should be suitable for most
--     cases.  Hence, most @Model@ instances should not specify
--     'modelQueries' and should instead perform customization in the
--     definition of 'modelInfo'.
--
-- 'modelInfo' itself provides a reasonable default implementation for
-- types that are members of the 'Generic' class (using GHC's
-- @DeriveGeneric@ extension), provided two conditions hold:
--
--   1. The data type must be defined using record selector syntax.
--
--   2. The very first field of the data type must be a 'DBKey' to
--      represent the primary key.  Other orders will cause a
--      compilation error.
--
-- If both of these conditions hold and your database naming scheme
-- follows the conventions of this module, it is reasonable to have a
-- completely empty (default) instance declaration:
--
-- >   data MyType = MyType { myKey :: !DBKey
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
-- 'defaultModelInfo', you can simply implement an alternate pattern,
-- say @someOtherPattern@, and use it in place of the default:
--
-- >   instance Model MyType where modelInfo = someOtherPattern
--
-- You can implement @someOtherPattern@ in terms of
-- 'defaultModelInfo', or use some of the lower-level functions from
-- which 'defaultModelInfo' is built.  Each component default function
-- is separately exposed (e.g., 'defaultModelTable',
-- 'defaultModelColumns', 'defaultModelGetPrimaryKey', etc.).
--
-- The default queries are simiarly provided by a default function
-- 'defaultModelQueries', whose individual component functions are
-- exposed ('defaultModelLookupQuery', etc.).  However, customizing
-- the queries is not recommended.
class Model a where
  modelInfo :: ModelInfo a
  default modelInfo :: (Generic a, GToRow (Rep a), GFromRow (Rep a)
                       , GPrimaryKey0 (Rep a), GColumns (Rep a)
                       , GDatatypeName (Rep a)) => ModelInfo a
  {-# INLINE modelInfo #-}
  modelInfo = defaultModelInfo
  modelQueries :: ModelQueries a
  modelQueries = defaultModelQueries modelInfo

-- | A degenerate instance of model representing a database join.  The
-- ':.' instance only allows limited queries such as 'findWhere' and
-- 'findAll'.  In particular, there is no primary key and no ability
-- to 'save' or 'destroy' such a join.  Attempts to use such functions
-- (including 'findRow', which requires a primary key) will result in
-- an error.
instance (Model a, Model b) => Model (a :. b) where
  modelInfo = joinModelInfo
  modelQueries = joinModelQueries

joinModelInfo :: (Model a, Model b) => ModelInfo (a :. b)
joinModelInfo = r
  where r = ModelInfo {
            modelTable = jname
          , modelColumns = modelColumns mia ++ modelColumns mib
          , modelPrimaryColumn = -1
          , modelGetPrimaryKey = const err
          , modelRead = (:.) <$> modelRead mia <*> modelRead mib
          , modelWrite = const err
          , modelIsTable = False
          }
        (mia, mib) = (const (modelInfo, modelInfo)
                      :: (Model a, Model b) => ModelInfo (a :. b)
                         -> (ModelInfo a, ModelInfo b)) r
        jname = modelTable mia <> " :. " <> modelTable mib
        err :: a
        err = error $ "illegal use of join relation " ++ S8.unpack jname

joinModelQueries :: (Model a, Model b) => ModelQueries (a :. b)
joinModelQueries = r
  where r = ModelQueries {
            modelQTable = modelQTable qa <> ", " <> modelQTable qb
          , modelQColumns = modelQColumns qa ++ modelQColumns qb
          , modelLookupQuery = badQuery
          , modelUpdateQuery = badQuery
          , modelInsertQuery = badQuery
          , modelDeleteQuery = badQuery
          }
        (qa, qb) = (const (modelQueries, modelQueries)
                    :: (Model a, Model b) => ModelQueries (a :. b)
                       -> (ModelQueries a, ModelQueries b)) r
        badQuery = "select illegal_attempt_to_use_join_result_as_model"

class GUnitType f where
  gUnitTypeCon :: f p -> String
instance (Constructor c) => GUnitType (C1 c U1) where
  gUnitTypeCon m1 = conName m1
instance (GUnitType f) => GUnitType (D1 c f) where
  gUnitTypeCon = gUnitTypeCon . unM1

-- | The class of types that can be used as tags in as 'As' alias.
-- Such types should be unit types--in other words, have exactly one
-- constructor that takes no arguments.  The 'Model' implementation
-- requires a way to extract the name of that constructor without
-- having a concrete instance of the type.  This is provided by the
-- 'rowAliasName' method.
class RowAlias a where
  rowAliasName :: As a row -> S.ByteString
  default rowAliasName :: (Generic a, GUnitType (Rep a)) =>
                             As a row -> S.ByteString
  rowAliasName as = fromString $ caseFold $ gUnitTypeCon . from $ fixtype as
    where fixtype :: As a row -> a
          fixtype _ = undefined
          caseFold (h:t) = toLower h:t
          caseFold s     = s

-- | The newtype @As@ can be wrapped around an existing type to give
-- it a table name alias in a query.  This is necessary when a model
-- is being joined with itself, to distinguish the two instances of
-- the database table.  For example:
--
-- @{-\# LANGUAGE DeriveGeneric, OverloadedStrings #-}
--
--data X = X deriving ('Generic')
--instance 'RowAlias' X
--
-- \  ...
--    r <- 'findWhere' \"bar.bar_key = x.bar_parent\" c () :: IO [Bar :. As X Bar]
-- @
newtype As alias row = As { unAs :: row } deriving (Show, Typeable)

-- | @fromAs@ extracts the @row@ from an @'As' alias row@, but
-- constrains the type of @alias@ to be the same as its first argument
-- (which is non-strict).  This can save you from explicitly
-- specifying types.  For example:
--
-- > data X = X deriving (Generic)
-- > instance RowAlias X
-- >
-- > ...
-- >   r <- map (\(b1 :. b2) -> (b1, fromAs X b2)) <$>
-- >            findWhere "bar.bar_key = X.bar_parent" c ()
fromAs :: alias -> As alias row -> row
fromAs _ (As row) = row

instance (Model a, RowAlias as) => Model (As as a) where
  {-# INLINE modelInfo #-}
  modelInfo
    | not (modelIsTable inner) =
        error "As constructor must be applied to simple tables"
    | otherwise = inner {
        modelGetPrimaryKey = \(As a) -> modelGetPrimaryKey inner a
      , modelRead = As <$> modelRead inner
      , modelWrite = const $ error "illegal attempt to write alias as model"
      , modelIsTable = False
      }
    where inner = modelInfo
  {-# INLINE modelQueries #-}
  modelQueries = qs
    where alias = rowAliasName $ poptycon qs
          mi = gmodelToInfo qs
          qs = (defaultModelQueries mi { modelTable = alias })
               { modelQTable = q (modelTable mi) <> " as " <> q alias }


-- | Lookup the 'ModelInfo' corresponding to a type.  Non-strict in
-- its parameter.
modelToInfo :: (Model a) => a -> ModelInfo a
{-# INLINE modelToInfo #-}
modelToInfo _ = modelInfo

-- | Lookup the 'ModelInfo' corresponding to a type @a@, given some
-- other type parameterized by @a@ (e.g., a @'DBRef' a@).  Non-strict
-- in its parameter.
gmodelToInfo :: (Model a) => g a -> ModelInfo a
{-# INLINE gmodelToInfo #-}
gmodelToInfo _ = modelInfo

-- | Lookup the 'ModelQueries' corresponding to a type.  Non-strict in
-- its parameter.
modelToQueries :: (Model a) => a -> ModelQueries a
{-# INLINE modelToQueries #-}
modelToQueries _ = modelQueries

-- | Lookup the 'ModelQueries' corresponding to a type @a@, given some
-- other type parameterized by @a@ (e.g., a @'DBRef' a@).  Non-strict
-- in its parameter.
gmodelToQueries :: (Model a) => g a -> ModelQueries a
{-# INLINE gmodelToQueries #-}
gmodelToQueries _ = modelQueries

-- | Lookup the 'modelTable' of a 'Model' (@modelName = 'modelTable'
-- . 'modelToInfo'@).
modelName :: (Model a) => a -> S.ByteString
{-# INLINE modelName #-}
modelName = modelTable . modelToInfo

-- | Lookup the primary key of a 'Model'.
primaryKey :: (Model a) => a -> DBKey
{-# INLINE primaryKey #-}
primaryKey a = modelGetPrimaryKey modelInfo a

poptycon :: g a -> a
poptycon _ = undefined

-- | Generate a SQL @SELECT@ statement with no @WHERE@ predicate.  For
-- example, 'defaultModelLookupQuery' consists of
-- @modelSelectFragment@ followed by \"@WHERE@ /primary-key/ = ?\".
modelSelectFragment :: ModelQueries a -> S.ByteString
modelSelectFragment qs = S.concat [
  "select ", S.intercalate ", " $ modelQColumns qs , " from ", modelQTable qs ]

-- | Retreive a quoted and fully-qualified name of the column storing
-- a model's primary keys.
modelQKey :: (Model a) => g a -> S.ByteString
modelQKey ga
  | n < 0 = error $ "modelQKey: " ++ S8.unpack (modelTable mi) ++
            " has no primary key"
  | otherwise = modelQColumns (gmodelToQueries ga) !! n
  where mi = gmodelToInfo ga
        n = modelPrimaryColumn mi

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

-- | Follow a 'DBRef' or 'DBURef' and fetch the target row from the
-- database into a 'Model' type @r@.
findRow :: (Model r) => Connection -> GDBRef rt r -> IO (Maybe r)
findRow c k = action
  where qs = gmodelToQueries $ poptycon action
        action = do rs <- query c (modelLookupQuery qs) (Only k)
                    case rs of [r] -> return $ Just $ lookupRow $ r
                               _   -> return Nothing

-- | An alias for 'findRow'.
find :: (Model r) => Connection -> GDBRef rt r -> IO (Maybe r)
{-# INLINE findRow #-}
find = findRow

-- | Find models matching a SQL @WHERE@ predicate given by a query and
-- parameters.  For example, 'findAll' could have been defined as:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > findAll :: (Model r) => Connection -> IO [r]
-- > findAll c = findWhere "TRUE" c ()
findWhere :: (ToRow parms, Model r) => Query -> Connection -> parms -> IO [r]
{-# INLINE findWhere #-}
findWhere (Query whereClause) c parms = action
  where sel = Query $ modelSelectFragment (gmodelToQueries (poptycon action))
              <> " where " <> whereClause
        action = do rs <- query c sel parms
                    return $ map lookupRow rs

-- | Return an entire database table.
findAll :: (Model r) => Connection -> IO [r]
findAll c = action
  where qs = gmodelToQueries (poptycon action)
        action = do rs <- query_ c (Query $ modelSelectFragment qs)
                    return $ map lookupRow rs


-- | Write a 'Model' to the database.  If the primary key is
-- 'NullKey', the item is written with an @INSERT@ query, read back
-- from the database, and returned with its primary key filled in.  If
-- the primary key is not 'NullKey', then the 'Model' is writen with
-- an @UPDATE@ query and returned as-is.
save :: (Model r) => Connection -> r -> IO r
save c r | NullKey <- primaryKey r = do
               rs <- query c (modelInsertQuery qs) (InsertRow r)
               case rs of [r'] -> return $ lookupRow r'
                          _    -> fail "save: database did not return row"
         | otherwise = do
               n <- execute c (modelUpdateQuery qs) (UpdateRow r)
               case n of 1 -> return r
                         _ -> fail $ "save: database updated " ++ show n
                                     ++ " records"
  where qs = modelToQueries r

-- | Remove the row corresponding to a particular data structure from
-- the database.  This function only looks at the primary key in the
-- data structure.  It is an error to call this function if the
-- primary key is not set.
destroy :: (Model a) => Connection -> a -> IO ()
destroy c a =
  case primaryKey a of
    NullKey -> fail "destroy: NullKey"
    DBKey k -> void $ execute c (modelDeleteQuery $ modelToQueries a) (Only k)

-- | Remove a row from the database without fetching it first.
destroyByRef :: (Model a) => Connection -> GDBRef rt a -> IO ()
destroyByRef c a =
  void $ execute c (modelDeleteQuery $ gmodelToQueries a) (Only a)

printq :: Query -> IO ()
printq (Query bs) = S8.putStrLn bs
