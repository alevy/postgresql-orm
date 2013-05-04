{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.ORM.Relationships (
  -- * Foreign key references
    DBRefInfo(..), DBRefQuery(..)
  , HasMany(..), findMany, HasOne(..), findOne
  , HasParent(..), findParent
  -- ** Foreign key default methods
  , defaultDBRefInfo, defaultParentKey, defaultChildQuery
  -- * Join tables
  , JoinTableInfo(..), JoinTableQueries(..), Joinable(..)
  , findJoin, addJoin, removeJoin
  -- ** Join table construction functions
  , joinDefault, joinReverse, joinThroughModel, joinThroughModelInfo
  -- ** Join table default queries
  , defaultJoinTableQueries
  , defaultjtLookupQuery, defaultjtAddQuery, defaultjtRemoveQuery
  -- * Internal details
  , DummyForRetainingTypes(..)
  ) where

import qualified Data.ByteString as S
import Data.Functor
import Data.Int
import Data.List hiding (find)
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Data.HasField
import Database.PostgreSQL.ORM.Model

-- import GHC.Generics

data DBRefInfo reftype child parent = DBRefInfo {
    dbrefSelector :: !(child -> GDBRef reftype parent)
    -- ^ Field selector returning a reference.
  , dbrefColumn :: !S.ByteString
    -- ^ Name of the database column storing the reference.
  }

instance Show (DBRefInfo rt c p) where
  show ri = "DBRefInfo ? " ++ show (dbrefColumn ri)

newtype DBRefQuery reftype child parent = DBRefQuery {
  dbrefQuery :: Query
  } deriving (Show)

defaultDBRefInfo :: (Model child, HasMaybeField child (GDBRef rt parent)) =>
                    rt -> DBRefInfo rt child parent
defaultDBRefInfo _ = ri
  where c = (undefined :: DBRefInfo rt c p -> c) ri
        rp = (undefined :: DBRefInfo rt c p -> GDBRef rt p) ri
        ri = DBRefInfo {
            dbrefSelector = fromJust . getMaybeFieldVal
          , dbrefColumn = modelColumns (modelToInfo c) !! getMaybeFieldPos c rp
          }

defaultChildQuery :: (Model child) =>
                     DBRefInfo rt child parent -> DBRefQuery rt child parent
defaultChildQuery ri = DBRefQuery $ Query $ S.concat [
  modelSelectFragment cq, " where ", quoteIdent (dbrefColumn ri), " = ?"]
  where cq = (const modelQueries
              :: Model c => DBRefInfo rt c p -> ModelQueries c) ri

class (Model parent, Model child) => HasMany parent child where
  hasManyInfo :: DBRefInfo NormalRef child parent
  default hasManyInfo :: (HasMaybeField child (GDBRef NormalRef parent)) =>
                         DBRefInfo NormalRef child parent
  {-# INLINE hasManyInfo #-}
  hasManyInfo = defaultDBRefInfo NormalRef
  hasManyQuery :: DBRefQuery NormalRef child parent
  {-# INLINE hasManyQuery #-}
  hasManyQuery = defaultChildQuery hasManyInfo

class (Model parent, Model child) => HasOne parent child where
  hasOneInfo :: DBRefInfo UniqueRef child parent
  default hasOneInfo :: (HasMaybeField child (GDBRef UniqueRef parent)) =>
                         DBRefInfo UniqueRef child parent
  {-# INLINE hasOneInfo #-}
  hasOneInfo = defaultDBRefInfo UniqueRef
  hasOneQuery :: DBRefQuery UniqueRef child parent
  {-# INLINE hasOneQuery #-}
  hasOneQuery = defaultChildQuery hasOneInfo

defaultParentKey :: DBRefInfo rt c p -> c -> Reference p
defaultParentKey ri c = case dbrefSelector ri c of DBRef k -> DBRef k

-- | The default only works for 'HasMany' relationships.  For 'HasOne'
-- (meaning the field is of type 'DBURef' instead of 'DBRef'), you
-- will need to say:
--
-- > instance HasParent Child Parent where
-- >     parentKey = defaultParentKey hasOneInfo
class (Model child, Model parent) => HasParent child parent where
  parentKey :: child -> Reference parent
  default parentKey :: (HasMany parent child) => child -> Reference parent
  parentKey = defaultParentKey hasManyInfo

rdChildrenOf :: (Model child, Model parent) =>
                DBRefQuery rt child parent -> Connection -> parent -> IO [child]
rdChildrenOf (DBRefQuery q) conn p =
  map lookupRow <$> query conn q (Only $ primaryKey p)

findOne :: (HasOne parent child) => Connection -> parent -> IO (Maybe child)
findOne c p = do
  rs <- rdChildrenOf hasOneQuery c p
  case rs of [r] -> return $ Just r
             _   -> return Nothing

findMany :: (HasMany parent child) => Connection -> parent -> IO [child]
findMany = rdChildrenOf hasManyQuery

findParent :: (HasParent child parent) =>
              Connection -> child -> IO (Maybe parent)
findParent conn child = find conn (parentKey child)


data DummyForRetainingTypes a b = DummyForRetainingTypes
instance Show (DummyForRetainingTypes a b) where show _ = ""

data JoinTableInfo a b = JoinTableInfo {
    jtTable :: !S.ByteString     -- ^ Name of the join table in the database
  , jtAllowModification :: !Bool
    -- ^ If 'False', disallow generic join table functions that modify
    -- the database.  The default is 'True' for normal join tables,
    -- but 'False' for 'joinThroughModel', since deleting a join
    -- relationsip through a model will destroy other columns of the
    -- join model.
  , jtColumnA :: !S.ByteString   -- ^ Name of ref to A in join table
  , jtKeyA :: !S.ByteString      -- ^ Name of referenced key field in table A
  , jtColumnB :: !S.ByteString
  , jtKeyB :: !S.ByteString
  , jtDummy :: DummyForRetainingTypes a b
    -- ^ Ignore this field.  It exists so that one can modify other
    -- fields of the @JoinTableInfo@ without changing the type
    -- parameters @a@ and @b@.
  } deriving (Show)

data JoinTableQueries a b = JoinTableQueries {
    jtLookupQuery :: !Query
    -- ^ Query takes 2 parameters, the primary keys of @a@ and @b@
    -- respectively.
  , jtAddQuery :: !Query
    -- ^ Query takes 4 parameters, namely two copies of each primary
    -- keys (@a@, @b@, @a@, @b@).  The redundancy is necessary for
    -- avoiding duplicate join table entries.
  , jtRemoveQuery :: !Query
    -- ^ Query takes 2 parameters, the primary keys of @a@ and @b@
    -- respectively.
  } deriving (Show)

defaultJoinTableQueries :: (Model a, Model b) =>
                           JoinTableInfo a b -> JoinTableQueries a b
defaultJoinTableQueries jt = JoinTableQueries {
    jtLookupQuery = defaultjtLookupQuery jt
  , jtAddQuery = if jtAllowModification jt then defaultjtAddQuery False jt
                 else "select non_standard_join_table_cannot_be_modified"
  , jtRemoveQuery = if jtAllowModification jt then defaultjtRemoveQuery jt
                    else "select non_standard_join_table_cannot_be_modified"
  }

defaultjtLookupQuery :: (Model b) => JoinTableInfo a b -> Query
defaultjtLookupQuery jt = Query $ S.concat [
  modelSelectFragment bq, " join ", quoteIdent (jtTable jt)
  , " on ", quoteIdent (jtTable jt), ".", quoteIdent (jtColumnB jt)
  , " = ", quoteIdent (modelTable b), ".", quoteIdent (jtKeyB jt)
  , " where ", quoteIdent (jtTable jt), ".", quoteIdent (jtColumnA jt)
  , " = ?"
  ]
  where (bq, b) = (gmodelToQueries jt, gmodelToInfo jt)

-- | Creates a query for adding a join relationsihp to the table.  If
-- the first argument is 'True', then duplicates will be allowed in
-- the join table.
defaultjtAddQuery :: (Model a, Model b) => Bool -> JoinTableInfo a b -> Query
defaultjtAddQuery allowDups jt = Query $ S.concat [
    "insert into ", quoteIdent (jtTable jt), " ("
  , quoteIdent (jtColumnA jt), ", ", quoteIdent (jtColumnB jt)
  , ") select ?, ? where not exists (select 1 from ", quoteIdent (jtTable jt)
  , " where ", if allowDups then "true or " else ""
  , quoteIdent (jtColumnA jt), " = ? and ", quoteIdent (jtColumnB jt), " = ?)"
  ]

defaultjtRemoveQuery :: JoinTableInfo a b -> Query
defaultjtRemoveQuery jt = Query $ S.concat [
  "delete from ", quoteIdent (jtTable jt), " where "
  , quoteIdent (jtColumnA jt), " = ? and "
  , quoteIdent (jtColumnB jt), " = ?"
  ]

-- | To make two 'Model's an instance of the @Joinable@ class, you
-- must manually specify the 'joinTable'.  There are three pre-defined
-- functions for creating joint tables:  'joinDefault', joinReverse',
-- and 'joinThroughModel'.
--
-- * 'joinDefault' creates a @Joinable@ instance assuming a simple
--    external join table with two columns, one for the primary key of
--    each type being joined.
--
-- * 'joinReverse' assumes a @Joinable@ relation already exists in the
--   other direction, and simply reverses it.  Generally you will use
--   'joinReverse' in conjunction with one of the other two.
--
-- * 'joinThroughModel' assumes that the join table is actually a
--   'Model' in which rows contain other information (such as a
--   primary key).  For this to work, the model used as a join table
--   must include two 'DBRef' fields, one for each of the models being
--   joined.
--
-- An example of declaring a simple join table is:
--
-- @
-- instance Joinable A B where 'joinTable' = 'joinDefault'
-- instance Joinable B A where 'joinTable' = 'joinReverse'
-- @
--
-- An example of using some 'Model' @C@ as the join table (assuming
-- @C@ contains a @'DBRef' A@ and a @'DBRef' B@, or 'Maybe' versions
-- of these types):
--
-- @
-- instance Joinable A B where
--     'joinTable' = 'joinThroughModel' ('undefined' :: C)
-- instance Joinable B A where
--     'joinTable' = 'joinReverse'
-- @
class (Model a, Model b) => Joinable a b where
  joinTable :: JoinTableInfo a b
  joinTableQueries :: JoinTableQueries a b
  {-# INLINE joinTableQueries #-}
  joinTableQueries = defaultJoinTableQueries $ joinTable

joinTableInfoModels :: (Model a, Model b) =>
                       JoinTableInfo a b -> (ModelInfo a, ModelInfo b)
joinTableInfoModels _ = (modelInfo, modelInfo)

joinDefault :: (Model a, Model b) => JoinTableInfo a b
joinDefault = jti
  where (a, b) = joinTableInfoModels jti
        keya = modelColumns a !! modelPrimaryColumn a
        keyb = modelColumns b !! modelPrimaryColumn b
        jti = JoinTableInfo {
            jtTable = S.intercalate "_" $
                          sort [modelTable a, modelTable b]
          , jtAllowModification = True
          , jtColumnA = S.concat [modelTable a, "_", keya]
          , jtKeyA = keya
          , jtColumnB = S.concat [modelTable b, "_", keyb]
          , jtKeyB = keyb
          , jtDummy = DummyForRetainingTypes
          }


flipJoinTableInfo :: JoinTableInfo a b -> JoinTableInfo b a
flipJoinTableInfo jt = JoinTableInfo {
    jtTable = jtTable jt
  , jtAllowModification = jtAllowModification jt
  , jtColumnA = jtColumnB jt
  , jtKeyA = jtKeyB jt
  , jtColumnB = jtColumnA jt
  , jtKeyB = jtKeyA jt
  , jtDummy = DummyForRetainingTypes
  }

joinReverse :: (Joinable a b) => JoinTableInfo b a
joinReverse = flipJoinTableInfo joinTable

joinThroughModelInfo :: (Model jt, Model a, Model b
                , HasMaybeField jt (Reference a)
                , HasMaybeField jt (Reference b)) =>
                ModelInfo jt -> JoinTableInfo a b
joinThroughModelInfo jt = jti
  where dummyRef :: ModelInfo a -> Reference a
        dummyRef _ = undefined
        poptycon :: ModelInfo a -> a
        poptycon _ = undefined
        (a, b) = joinTableInfoModels jti
        jti = JoinTableInfo {
            jtTable = modelTable jt
          , jtAllowModification = False
          , jtColumnA = modelColumns jt !!
                        getMaybeFieldPos (poptycon jt) (dummyRef a)
          , jtKeyA = modelColumns a !! modelPrimaryColumn a
          , jtColumnB = modelColumns jt !!
                        getMaybeFieldPos (poptycon jt) (dummyRef b)
          , jtKeyB = modelColumns b !! modelPrimaryColumn b
          , jtDummy = DummyForRetainingTypes
          }

joinThroughModel :: (Model jt, Model a, Model b
                   , HasMaybeField jt (Reference a)
                   , HasMaybeField jt (Reference b)) =>
                   jt -> JoinTableInfo a b
joinThroughModel = joinThroughModelInfo . modelToInfo

jtJoinOf :: (Model a, Model b) =>
            JoinTableQueries a b -> Connection -> a -> IO [b]
jtJoinOf JoinTableQueries{ jtLookupQuery = q } conn a =
  map lookupRow <$> query conn q (Only $ primaryKey a)

findJoin :: (Joinable a b) => Connection -> a -> IO [b]
findJoin = jtJoinOf joinTableQueries

modelsToJTQ :: (Joinable a b) => a -> b -> JoinTableQueries a b
modelsToJTQ _ _ = joinTableQueries

addJoin :: (Joinable a b) => Connection -> a -> b -> IO Bool
addJoin c a b = do
  (> 0) <$> execute c (jtAddQuery $ modelsToJTQ a b)
    (primaryKey a, primaryKey b, primaryKey a, primaryKey b)

removeJoin :: (Joinable a b) => Connection -> a -> b -> IO Int64
removeJoin c a b = execute c (jtRemoveQuery $ modelsToJTQ a b)
                   (primaryKey a, primaryKey b)


{-
select Post.*, User.* from Post, User, Comment where Comment.postId =
	 Post.id and Comment.userId = User.id

<aalevy> prefix is the table name referenced, suffix is the column
	 name referenced  [15:54]
<aalevy> so, "post_id" for example
<aalevy> or, i guess "postId"
-}

{-
data Foo = Foo {
  foo_key :: !DBKey
  , foo_int :: !Int32
  , parent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)
                                    
instance Model Foo


data Bar = Bar {
  bar_key :: !DBKey
  , bar_none :: !Int32
  , bar_string :: !String
  , bar_parent :: !(Maybe (DBURef Bar))
  } deriving (Show, Generic)
instance Model Bar


data Joiner = Joiner {
    jkey :: !DBKey
  , jcomment :: !String
  , jfoo :: (DBRef Foo)
  , jbar :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)
instance Model Joiner

bar :: Bar
bar = Bar NullKey 77 "hi" (Just $ DBRef 3)

instance Joinable Foo Bar where
  joinTable = (joinThroughModel (undefined :: Joiner)) {
    jtAllowModification = True }
instance Joinable Bar Foo where
  joinTable = joinReverse

instance HasOne Bar Bar
-}
