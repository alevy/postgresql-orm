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

data RefDescriptor r c p = RefDescriptor {
    refDescRefColumn :: !Int
  , refDescSelector :: !(c -> r p)
  , refDescQuery :: !Query
  }
instance (Model c) => Show (RefDescriptor r c p) where
  show rd = "RefDescriptor " ++ show c ++ " " ++
            S8.unpack (modelColumns mi !! c) ++ " " ++
            show (refDescQuery rd)
    where c = refDescRefColumn rd
          getmi :: (Model c) => RefDescriptor r c p -> ModelInfo c
          getmi _ = modelInfo
          mi = getmi rd


defaultRefDescriptor :: (Model c, Generic c
                        , GHasMaybeField (Rep c) (r p) TYes) =>
                        RefDescriptor r c p
defaultRefDescriptor = rd
  where getTypes :: RefDescriptor r c p -> (c, r p)
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

class (Model p, Model c) => HasOne p c where
  hasOneDesc :: RefDescriptor DBURef c p
  default hasOneDesc :: (Model c, Generic c
                        , GHasMaybeField (Rep c) (DBURef p) TYes) =>
                        RefDescriptor DBURef c p
  {-# INLINE hasOneDesc #-}
  hasOneDesc = defaultRefDescriptor

class (Model p, Model c) => HasMany p c where
  hasManyDesc :: RefDescriptor DBRef c p
  default hasManyDesc :: (Model c, Generic c
                         , GHasMaybeField (Rep c) (DBRef p) TYes) =>
                         RefDescriptor DBRef c p
  {-# INLINE hasManyDesc #-}
  hasManyDesc = defaultRefDescriptor


rdParentOf :: (IsDBRef r, Model p) =>
              RefDescriptor r c p -> Connection -> c -> IO (Maybe p)
rdParentOf rd conn c = findRef conn $ refDescSelector rd c

rdChildrenOf :: (Model c, IsDBRef r, Model p) =>
                RefDescriptor r c p -> Connection -> p -> IO [c]
rdChildrenOf rd conn p =
  map lookupRow <$> query conn (refDescQuery rd) (Only $ primaryKey p)


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
