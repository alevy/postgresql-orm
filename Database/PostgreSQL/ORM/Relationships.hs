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

data RefDescriptor b r h = RefDescriptor {
    refDescRefColumn :: !Int
  , refDescSelector :: !(b -> r h)
  , refDescQuery :: !Query
  }
instance (Model b) => Show (RefDescriptor b r h) where
  show rd = "RefDescriptor " ++ show c ++ " " ++
            S8.unpack (modelColumns mi !! c) ++ " " ++
            show (refDescQuery rd)
    where c = refDescRefColumn rd
          getmi :: (Model b) => RefDescriptor b r h -> ModelInfo b
          getmi _ = modelInfo
          mi = getmi rd


defaultRefDescriptor :: (Model b, Generic b
                        , GHasMaybeField (Rep b) (r h) TYes) =>
                        RefDescriptor b r h
defaultRefDescriptor = rd
  where getTypes :: RefDescriptor b r h -> (b, r h)
        getTypes _ = (undefined, undefined)
        (b, rh) = getTypes rd
        cols = modelColumns $ modelToInfo b
        colno = getMaybeFieldPos b rh
        qstr = S.concat [
          "select "
          , S.intercalate ", " (map quoteIdent cols)
          , " from ", quoteIdent (modelName b), " where "
          , quoteIdent (cols !! colno), " = ?"]
        rd = RefDescriptor {
            refDescRefColumn = colno
          , refDescSelector = fromJust . getMaybeFieldVal
          , refDescQuery = Query qstr
          }

class HasOne h b where
  hasOneDesc :: RefDescriptor b DBURef h
  default hasOneDesc :: (Model b, Generic b
                        , GHasMaybeField (Rep b) (DBURef h) TYes) =>
                        RefDescriptor b DBURef h
  {-# INLINE hasOneDesc #-}
  hasOneDesc = defaultRefDescriptor

class HasMany h b where
  hasManyDesc :: RefDescriptor b DBRef h
  default hasManyDesc :: (Model b, Generic b
                         , GHasMaybeField (Rep b) (DBRef h) TYes) =>
                         RefDescriptor b DBRef h
  {-# INLINE hasManyDesc #-}
  hasManyDesc = defaultRefDescriptor


rdParentOf :: (IsDBRef r, Model h) =>
              RefDescriptor b r h -> Connection -> b -> IO (Maybe h)
rdParentOf rd c b = findRef c $ refDescSelector rd b

rdChildrenOf :: (Model b, IsDBRef r, Model h) =>
                RefDescriptor b r h -> Connection -> h -> IO [b]
rdChildrenOf rd c h =
  map lookupRow <$> query c (refDescQuery rd) (Only $ primaryKey h)


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
