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
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Database.PostgreSQL.ORM.HasField
import Database.PostgreSQL.ORM.Model

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


defaultDBRefInfo :: (Model child, Generic child
                    , GHasMaybeField (Rep child) (r parent) TYes) =>
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

class (Model parent, Model child) => HasOne parent child where
  hasOneInfo :: DBRefInfo DBURef child parent
  default hasOneInfo :: (Model child, Generic child
                        , GHasMaybeField (Rep child) (DBURef parent) TYes) =>
                        DBRefInfo DBURef child parent
  {-# INLINE hasOneInfo #-}
  hasOneInfo = defaultDBRefInfo

class (Model parent, Model child) => HasMany parent child where
  hasManyInfo :: DBRefInfo DBRef child parent
  default hasManyInfo :: (Model child, Generic child
                         , GHasMaybeField (Rep child) (DBRef parent) TYes) =>
                         DBRefInfo DBRef child parent
  {-# INLINE hasManyInfo #-}
  hasManyInfo = defaultDBRefInfo


rdParentOf :: (IsDBRef r, Model parent) =>
              DBRefInfo r child parent -> Connection -> child
              -> IO (Maybe parent)
rdParentOf rd conn c = findRef conn $ dbrefSelector rd c

rdChildrenOf :: (Model child, IsDBRef r, Model parent) =>
                DBRefInfo r child parent -> Connection -> parent
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


data JoinInfo a b = JoinInfo {
  joinQuery :: !Query
  } deriving (Show)

data JoinTableInfo a b = JoinTableInfo { jtInfoName :: !S.ByteString
                                       , jtColA :: !S.ByteString
                                       , jtColB :: !S.ByteString
                                       , jtKeyA :: !S.ByteString
                                       , jtKeyB :: !S.ByteString
                                       } deriving (Show)

flipJoinTableInfo :: JoinTableInfo a b -> JoinTableInfo b a
flipJoinTableInfo jt = JoinTableInfo { jtInfoName = jtInfoName jt
                                     , jtColA = jtColB jt
                                     , jtColB = jtColA jt
                                     , jtKeyA = jtKeyB jt
                                     , jtKeyB = jtKeyA jt
                                     }

defaultJoinTableInfo :: (Model a, Model b) => JoinTableInfo a b
defaultJoinTableInfo = jti
  where getmis :: (Model a, Model b) =>
                  JoinTableInfo a b -> (ModelInfo a, ModelInfo b)
        getmis _ = (modelInfo, modelInfo)
        (a, b) = getmis jti
        keya = modelColumns a !! modelPrimaryColumn a
        keyb = modelColumns b !! modelPrimaryColumn b
        jti = JoinTableInfo {
            jtInfoName = S.intercalate "_" $
                         sort [modelInfoName a, modelInfoName b]
          , jtColA = S.concat [modelInfoName a, "_", keya]
          , jtColB = S.concat [modelInfoName b, "_", keyb]
          , jtKeyA = keya
          , jtKeyB = keyb
          }


class (Model a, Model b) => Joins a b where
  joinInfo :: JoinInfo a b

jtJoinOf :: (Model a, Model b) => JoinInfo a b -> Connection -> a -> IO [b]
jtJoinOf jt conn a = do
  map lookupRow <$> query conn (joinQuery jt) (Only $ primaryKey a)

findJoin :: (Joins a b) => Connection -> a -> IO [b]
findJoin = jtJoinOf joinInfo


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

bar :: Bar
bar = Bar NullKey 77 "hi" Nothing

instance HasOne Bar Bar
