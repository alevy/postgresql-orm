{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.ORM.Relationships where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Data.HasField
import Database.PostgreSQL.ORM.Model

import GHC.Generics
import Data.Int

data DBRefInfo r child parent = DBRefInfo {
    dbrefColumn :: !Int
  , dbrefSelector :: !(child -> r parent)
  , dbrefQuery :: !Query
  }
instance (Model child) => Show (DBRefInfo r child parent) where
  show rd = "DBRefInfo " ++ show c ++ " " ++
            S8.unpack (modelColumns mi !! c) ++ " " ++
            show (dbrefQuery rd)
    where c = dbrefColumn rd
          getmi :: (Model c) => DBRefInfo r c parent -> ModelInfo c
          getmi _ = modelInfo
          mi = getmi rd

defaultDBRefInfo :: (Model child
                    , HasMaybeField child (r parent)) =>
                    DBRefInfo r child parent
defaultDBRefInfo = rd
  where getTypes :: DBRefInfo r child parent -> (child, r parent)
        getTypes _ = (undefined, undefined)
        (c, rh) = getTypes rd
        cols = modelColumns $ modelToInfo c
        colno = getMaybeFieldPos c rh
        qstr = S.concat [
          "select "
          , S.intercalate ", " (map quoteIdent cols)
          , " from ", quoteIdent (modelName c), " where "
          , quoteIdent (cols !! colno), " = ?"]
        rd = DBRefInfo {
            dbrefColumn = colno
          , dbrefSelector = fromJust . getMaybeFieldVal
          , dbrefQuery = Query qstr
          }

getParentRef :: (HasMaybeField child (GDBRef rt parent)) =>
                rt -> child -> DBRef parent
getParentRef rt c = forcert rt $ fromJust $ getMaybeFieldVal c
  where forcert :: rt -> GDBRef rt parent -> DBRef parent
        forcert _ (GDBRef k) = GDBRef k

class (Model parent, Model child) => HasOne parent child where
  hasOneInfo :: DBRefInfo DBURef child parent
  default hasOneInfo :: (Model child
                        , HasMaybeField child (DBURef parent)) =>
                        DBRefInfo DBURef child parent
  {-# INLINE hasOneInfo #-}
  hasOneInfo = defaultDBRefInfo

class (Model parent, Model child) => HasMany parent child where
  hasManyInfo :: DBRefInfo DBRef child parent
  default hasManyInfo :: (Model child
                         , HasMaybeField child (DBRef parent)) =>
                         DBRefInfo DBRef child parent
  {-# INLINE hasManyInfo #-}
  hasManyInfo = defaultDBRefInfo

-- | The default only works for 'HasMany' relationships.  For 'HasOne'
-- (meaning the field is of type 'DBURef' instead of 'DBRef'), you
-- will need to say:
--
-- > instance HasParent Child Parent where
-- >     parentRef = getParentRef UniqueRef
--
class (Model parent) => HasParent child parent where
  parentRef :: child -> DBRef parent
  default parentRef :: (HasMaybeField child (DBRef parent)) =>
                       child -> DBRef parent
  parentRef = getParentRef NormalRef

rdChildrenOf :: (Model child, Model parent) =>
                DBRefInfo (GDBRef rt) child parent -> Connection -> parent
                -> IO [child]
rdChildrenOf rd conn p =
  map lookupRow <$> query conn (dbrefQuery rd) (Only $ primaryKey p)

findOne :: (HasOne parent child) => Connection -> parent -> IO (Maybe child)
findOne c p = do
  rs <- rdChildrenOf hasOneInfo c p
  case rs of [r] -> return $ Just r
             _   -> return Nothing

findMany :: (HasMany parent child) => Connection -> parent -> IO [child]
findMany = rdChildrenOf hasManyInfo

findParent :: (HasParent child parent) =>
              Connection -> child -> IO (Maybe parent)
findParent conn child = findRef conn (parentRef child)


data DummyForRetainingTypes a b = DummyForRetainingTypes
instance Show (DummyForRetainingTypes a b) where show _ = ""

data JoinTableInfo a b = JoinTableInfo {
    jtTable :: !S.ByteString   -- ^ Name of the join table in the database
  , jtAllowUpdates :: !Bool    -- ^ If False, only allow reads
  , jtColumnA :: !S.ByteString -- ^ Name of ref to A in join table
  , jtKeyA :: !S.ByteString    -- ^ Name of referenced key field in table A
  , jtColumnB :: !S.ByteString
  , jtKeyB :: !S.ByteString
  , jtDummy :: DummyForRetainingTypes a b
    -- ^ Ignore this field.  It exists so that one can modify other
    -- fields of the @JoinTableInfo@ without changing the type
    -- parameters @a@ and @b@.
  } deriving (Show)

data JoinQueryTemplate a b = JoinQueryTemplate !Query deriving (Show)

mkJoinQueryTemplate :: (Model a, Model b) =>
                       JoinTableInfo a b -> JoinQueryTemplate a b
mkJoinQueryTemplate jt = JoinQueryTemplate $ Query $ S.concat [
    "select ", qcols b, " from "
  , quoteIdent (jtTable jt), " join ", quoteIdent (modelTable b)
  , " on ", quoteIdent (jtTable jt), ".", quoteIdent (jtColumnB jt)
  , " = ", quoteIdent (modelTable b), ".", quoteIdent (jtKeyB jt)
  , " where ", quoteIdent (jtTable jt), ".", quoteIdent (jtColumnA jt)
  , " = ?"
  ]
  where (_, b) = joinTableInfoModels jt
        qcols mi = S.intercalate ", " $ map qcol $ modelColumns mi
          where qname = quoteIdent $ modelTable mi
                qcol c = qname <> "." <> quoteIdent c

-- | To make two 'Model's an instance of the @Joinable@ class, you
-- must manually specify the 'joinTable'.  There are three convenient
-- defaults for doing this:
--
-- @instance Joinable A B where joinTable = 'joinDefault'
--
-- instance Joinable A B where joinTable = 'joinReverse'
--
-- instance Joinable A B where
--     joinTable = 'joinThroughModel' (modelInfo :: ModelInfo C)
-- @
class (Model a, Model b) => Joinable a b where
  joinTable :: JoinTableInfo a b
  joinQueryTemplate :: JoinQueryTemplate a b
  {-# INLINE joinQueryTemplate #-}
  joinQueryTemplate = mkJoinQueryTemplate $ joinTable

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
          , jtAllowUpdates = True
          , jtColumnA = S.concat [modelTable a, "_", keya]
          , jtKeyA = keya
          , jtColumnB = S.concat [modelTable b, "_", keyb]
          , jtKeyB = keyb
          , jtDummy = DummyForRetainingTypes
          }


flipJoinTableInfo :: JoinTableInfo a b -> JoinTableInfo b a
flipJoinTableInfo jt = JoinTableInfo { jtTable = jtTable jt
                                     , jtAllowUpdates = jtAllowUpdates jt
                                     , jtColumnA = jtColumnB jt
                                     , jtKeyA = jtKeyB jt
                                     , jtColumnB = jtColumnA jt
                                     , jtKeyB = jtKeyA jt
                                     , jtDummy = DummyForRetainingTypes
                                     }

joinReverse :: (Joinable a b) => JoinTableInfo b a
joinReverse = flipJoinTableInfo joinTable

joinThroughModelInfo :: (Model jt, Model a, Model b
                , HasMaybeField jt (DBRef a)
                , HasMaybeField jt (DBRef b)) =>
                ModelInfo jt -> JoinTableInfo a b
joinThroughModelInfo jt = jti
  where dummyRef :: ModelInfo a -> DBRef a
        dummyRef _ = undefined
        poptycon :: ModelInfo a -> a
        poptycon _ = undefined
        (a, b) = joinTableInfoModels jti
        jti = JoinTableInfo {
            jtTable = modelTable jt
          , jtAllowUpdates = True
          , jtColumnA = modelColumns jt !!
                        getMaybeFieldPos (poptycon jt) (dummyRef a)
          , jtKeyA = modelColumns a !! modelPrimaryColumn a
          , jtColumnB = modelColumns jt !!
                        getMaybeFieldPos (poptycon jt) (dummyRef b)
          , jtKeyB = modelColumns b !! modelPrimaryColumn b
          , jtDummy = DummyForRetainingTypes
          }

joinThroughModel :: (Model jt, Model a, Model b
                   , HasMaybeField jt (DBRef a)
                   , HasMaybeField jt (DBRef b)) =>
                   jt -> JoinTableInfo a b
joinThroughModel = joinThroughModelInfo . modelToInfo

jtJoinOf :: (Model a, Model b) =>
            JoinQueryTemplate a b -> Connection -> a -> IO [b]
jtJoinOf (JoinQueryTemplate q) conn a =
  map lookupRow <$> query conn q (Only $ primaryKey a)

findJoin :: (Joinable a b) => Connection -> a -> IO [b]
findJoin = jtJoinOf joinQueryTemplate




{-
select Post.*, User.* from Post, User, Comment where Comment.postId =
	 Post.id and Comment.userId = User.id

<aalevy> prefix is the table name referenced, suffix is the column
	 name referenced  [15:54]
<aalevy> so, "post_id" for example
<aalevy> or, i guess "postId"
-}

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
bar = Bar NullKey 77 "hi" (Just $ GDBRef 3)

instance Joinable Foo Bar where
  joinTable = (joinThroughModel (undefined :: Joiner)) { jtAllowUpdates = True }

instance HasOne Bar Bar
