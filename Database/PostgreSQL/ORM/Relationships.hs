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
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Database.PostgreSQL.ORM.HasField
import Database.PostgreSQL.ORM.Model

import Data.Int

data RefDescriptor r child parent = RefDescriptor {
    refDescRefColumn :: !Int
  , refDescSelector :: !(child -> r parent)
  , refDescQuery :: !Query
  }
instance (Model child) => Show (RefDescriptor r child parent) where
  show rd = "RefDescriptor " ++ show c ++ " " ++
            S8.unpack (modelColumns mi !! c) ++ " " ++
            show (refDescQuery rd)
    where c = refDescRefColumn rd
          getmi :: (Model c) => RefDescriptor r c parent -> ModelInfo c
          getmi _ = modelInfo
          mi = getmi rd


defaultRefDescriptor :: (Model child, Generic child
                        , GHasMaybeField (Rep child) (r parent) TYes) =>
                        RefDescriptor r child parent
defaultRefDescriptor = rd
  where getTypes :: RefDescriptor r child parent -> (child, r parent)
        getTypes _ = (undefined, undefined)
        (c, rh) = getTypes rd
        cols = modelColumns $ modelToInfo c
        colno = getMaybeFieldPos c rh
        qstr = S.concat [
          "select "
          , S.intercalate ", " (map quoteIdent cols)
          , " from ", quoteIdent (modelName c), " where "
          , quoteIdent (cols !! colno), " = ?"]
        rd = RefDescriptor {
            refDescRefColumn = colno
          , refDescSelector = fromJust . getMaybeFieldVal
          , refDescQuery = Query qstr
          }

class (Model parent, Model child) => HasOne parent child where
  hasOneDesc :: RefDescriptor DBURef child parent
  default hasOneDesc :: (Model child, Generic child
                        , GHasMaybeField (Rep child) (DBURef parent) TYes) =>
                        RefDescriptor DBURef child parent
  {-# INLINE hasOneDesc #-}
  hasOneDesc = defaultRefDescriptor

class (Model parent, Model child) => HasMany parent child where
  hasManyDesc :: RefDescriptor DBRef child parent
  default hasManyDesc :: (Model child, Generic child
                         , GHasMaybeField (Rep child) (DBRef parent) TYes) =>
                         RefDescriptor DBRef child parent
  {-# INLINE hasManyDesc #-}
  hasManyDesc = defaultRefDescriptor


rdParentOf :: (IsDBRef r, Model parent) =>
              RefDescriptor r child parent -> Connection -> child
              -> IO (Maybe parent)
rdParentOf rd conn c = findRef conn $ refDescSelector rd c

rdChildrenOf :: (Model child, IsDBRef r, Model parent) =>
                RefDescriptor r child parent -> Connection -> parent
                -> IO [child]
rdChildrenOf rd conn p =
  map lookupRow <$> query conn (refDescQuery rd) (Only $ primaryKey p)


findOne :: (HasOne parent child) => Connection -> parent -> IO (Maybe child)
findOne c p = do
  rs <- rdChildrenOf hasOneDesc c p
  case rs of [r] -> return $ Just r
             _   -> return Nothing

findMany :: (HasMany parent child) => Connection -> parent -> IO [child]
findMany = rdChildrenOf hasManyDesc


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
