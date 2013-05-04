{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functions to extract a field of a particular type from a
-- 'Generic' data structure, when the data structure contains exactly
-- one field of the given type.  Only works for types with a single
-- constructor.
module Data.HasField (
  HasField(..), HasMaybeField(..)) where

import GHC.Generics

class TypeGCast f g | f -> g where
  typeGCast :: f p -> g p
instance TypeGCast f f where
  typeGCast = id

newtype TYes a = TYes { fromTYes :: a } deriving (Show)
data TNo a = TNo deriving (Show)


class GJustOne a b c | a b -> c where
  gJustOne :: a p -> b p -> c p
  gPruneRight :: a p -> b p -> Int -> Int -> Int
  gPruneRight _ _ a b = a + b
instance GJustOne TYes TNo TYes where
  {-# INLINE gJustOne #-}
  gJustOne j _ = j
  gPruneRight _ _ a _ = a
instance GJustOne TNo TYes TYes where
  {-# INLINE gJustOne #-}
  gJustOne _ j = j
instance GJustOne TNo TNo TNo where
  gJustOne _ _ = TNo

class GHasField t f g | t f -> g where
  gGetField :: t p -> g f
  gFieldCount :: t p -> f -> Int
instance GHasField (K1 i c) c TYes where
  {-# INLINE gGetField #-}
  gGetField (K1 c) = TYes c
  gFieldCount _ _ = 1
instance (TypeGCast TNo g) => GHasField (K1 i c) c' g where
  gGetField _ = typeGCast TNo
  gFieldCount _ _ = 1
instance (GHasField a f ga, GHasField b f gb, GJustOne ga gb g) =>
         GHasField (a :*: b) f g where
           {-# INLINE gGetField #-}
           gGetField (a :*: b) = gJustOne (gGetField a) (gGetField b)
           gFieldCount ~(a :*: b) f =
             gPruneRight (force f $ gGetField a) (force f $ gGetField b)
             (gFieldCount a f) (gFieldCount b f)
             where force :: f -> c f -> c f
                   force _ = id
instance (GHasField x f g) => GHasField (M1 i c x) f g where
  {-# INLINE gGetField #-}
  gGetField (M1 xp) = gGetField xp
  gFieldCount ~(M1 xp) f = gFieldCount xp f

class (Generic a, GHasField (Rep a) f TYes) => HasField a f where
  -- | Extract the 0-based position of a field with a particular type
  -- inside a data structure.  Requires that exactly one record have the
  -- target type.  Not strict in either argument.
  getFieldPos :: (HasField a f) => a -> f -> Int
  -- | Extract the single field of a particular type from a 'Generic'
  -- data structure with exactly one constructor.
  getFieldVal :: (HasField a f) => a -> f
instance (Generic a, GHasField (Rep a) f TYes) => HasField a f where
  getFieldPos a f = gFieldCount (from a) f - 1
  {-# INLINE getFieldVal #-}
  getFieldVal a = fromTYes $ gGetField (from a)


class GHasMaybeField t f g | t f -> g where
  gGetMaybeField :: t p -> g (Maybe f)
  gMaybeFieldCount :: t p -> f -> Int
instance GHasMaybeField (K1 i c) c TYes where
  {-# INLINE gGetMaybeField #-}
  gGetMaybeField (K1 c) = TYes (Just c)
  gMaybeFieldCount _ _ = 1
instance GHasMaybeField (K1 i (Maybe c)) c TYes where
  {-# INLINE gGetMaybeField #-}
  gGetMaybeField (K1 mc) = TYes mc
  gMaybeFieldCount _ _ = 1
instance (TypeGCast TNo g) => GHasMaybeField (K1 i c) c' g where
  gGetMaybeField _ = typeGCast TNo
  gMaybeFieldCount _ _ = 1
instance (GHasMaybeField a f ga, GHasMaybeField b f gb, GJustOne ga gb g) =>
    GHasMaybeField (a :*: b) f g where
      {-# INLINE gGetMaybeField #-}
      gGetMaybeField (a :*: b) = gJustOne (gGetMaybeField a) (gGetMaybeField b)
      gMaybeFieldCount ~(a :*: b) f =
        gPruneRight (force f $ gGetMaybeField a) (force f $ gGetMaybeField b)
        (gMaybeFieldCount a f) (gMaybeFieldCount b f)
        where force :: f -> c (Maybe f) -> c (Maybe f)
              force _ = id
instance (GHasMaybeField x f g) => GHasMaybeField (M1 i c x) f g where
  {-# INLINE gGetMaybeField #-}
  gGetMaybeField (M1 xp) = gGetMaybeField xp
  gMaybeFieldCount ~(M1 xp) f = gMaybeFieldCount xp f

class (Generic a, GHasMaybeField (Rep a) f TYes) => HasMaybeField a f where
  -- | Similar to 'getFieldPos', but looks for a field that is of type
  -- either @f@ or of type @Maybe f@.  (Only one field may have either
  -- of those two types.)
  getMaybeFieldPos :: (HasMaybeField a f) => a -> f -> Int
  -- | Similar to 'getFieldVal', but looks for a field that is of type
  -- either @f@ or of type @Maybe f@.  In the former case, the value
  -- is wrapped with 'Just' to ensure the return type is always @Maybe
  -- f@.
  getMaybeFieldVal :: (HasMaybeField a f) => a -> Maybe f

instance (Generic a, GHasMaybeField (Rep a) f TYes) => HasMaybeField a f where
  getMaybeFieldPos a f = gMaybeFieldCount (from a) f - 1
  {-# INLINE getMaybeFieldVal #-}
  getMaybeFieldVal a = fromTYes $ gGetMaybeField (from a)
