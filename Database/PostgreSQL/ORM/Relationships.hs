{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.PostgreSQL.ORM.Relationships where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import GHC.Generics

import Database.PostgreSQL.ORM.HasField
import Database.PostgreSQL.ORM.Model

import Data.Int

data RefDescriptor b r = RefDescriptor {
    refDescRefColumn :: !Int
  , refDescSelector :: !(b -> r)
  }
instance (Model b) => Show (RefDescriptor b r) where
  show rd = "RefDescriptor " ++ show c ++ " " ++
            S8.unpack (modelColumns mi !! c)
    where c = refDescRefColumn rd
          getmi :: (Model b) => RefDescriptor b r -> ModelInfo b
          getmi _ = modelInfo
          mi = getmi rd


defaultRefDescriptor :: (Model b, Generic b, GHasMaybeField (Rep b) r TYes) =>
                        RefDescriptor b r
defaultRefDescriptor = rd
  where getTypes :: RefDescriptor b r -> (b, r)
        getTypes _ = (undefined, undefined)
        (b, r) = getTypes rd
        rd = RefDescriptor {
            refDescRefColumn = getMaybeFieldPos b r
          , refDescSelector = fromJust . getMaybeFieldVal
          }

class HasOne h b where
  hasOneDesc :: RefDescriptor b (DBURef h)
  default hasOneDesc :: (Model b, Generic b
                        , GHasMaybeField (Rep b) (DBURef h) TYes) =>
                        RefDescriptor b (DBURef h)
  {-# INLINE hasOneDesc #-}
  hasOneDesc = defaultRefDescriptor

class HasMany h b where
  hasManyDesc :: RefDescriptor b (DBRef h)
  default hasManyDesc :: (Model b, Generic b
                         , GHasMaybeField (Rep b) (DBRef h) TYes) =>
                         RefDescriptor b (DBRef h)
  {-# INLINE hasManyDesc #-}
  hasManyDesc = defaultRefDescriptor




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
