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
module Data.GetField (
  GetField(..), ExtractId(..), ExtractMaybe(..)
  -- * Internals
  , THasOne(..), THasZero(..), THasMany(..), Extractor(..), GGetField(..)
  ) where

import GHC.Generics

import Data.AsTypeOf

-- | Dirty trick to construct "less specific" overlapping instances by
-- making a class argument a simple type variable, but constraining
-- that variable to be a particular type.  E.g., neither of the
-- following two instances is more specific than the other, because
-- @NO@ is not more general than @YES@:
--
-- > instance MyClass a a YES
-- > instance MyClass a b NO
--
-- On the other hand, of the following two instances, the first is
-- more specific than the second:
--
-- > instance MyClass a a YES
-- > instance (TypeGCast NO c) => MyClass a b c
--
-- Note that @YES@ and @NO@ in these examples have kind @* -> *@.
-- Hence the @G@ in @TypeGCast@.  The same trick is equally applicable
-- to types of kind @*@, however.
class TypeGCast f g | f -> g where
  typeGCast :: f p -> g p
instance TypeGCast f f where
  typeGCast = id

-- | Exactly one occurrence of target type found
newtype THasOne a = THasOne { fromTHasOne :: a } deriving (Show)
-- | Zero occurrences of target type found
data THasZero a = THasZero deriving (Show)
-- | More than one occurrences of target type found
newtype THasMany a = THasMany { fromTHasMany :: [a] } deriving (Show)

class GCombine a b c | a b -> c where
  gCombine :: a p -> b p -> c p
instance GCombine THasOne THasZero THasOne where
  {-# INLINE gCombine #-}
  gCombine j _ = j
instance GCombine THasZero THasOne THasOne where
  {-# INLINE gCombine #-}
  gCombine _ j = j
instance GCombine THasZero THasZero THasZero where
  -- Should never be evaluated, so no need to inline it
  gCombine _ _ = THasZero
instance GCombine THasOne THasOne THasMany where
  {-# INLINE gCombine #-}
  gCombine (THasOne a) (THasOne b) = THasMany [a,b]
instance GCombine THasMany THasMany THasMany where
  {-# INLINE gCombine #-}
  gCombine (THasMany as) (THasMany bs) = THasMany (as ++ bs)
instance GCombine THasZero THasMany THasMany where
  {-# INLINE gCombine #-}
  gCombine _ hm = hm
instance GCombine THasMany THasZero THasMany where
  {-# INLINE gCombine #-}
  gCombine hm _ = hm
instance GCombine THasOne THasMany THasMany where
  {-# INLINE gCombine #-}
  gCombine (THasOne a) (THasMany as) = THasMany (a:as)
instance GCombine THasMany THasOne THasMany where
  {-# INLINE gCombine #-}
  gCombine (THasMany as) (THasOne a) = THasMany (as++[a])

class GCount f where gCount :: f p -> (Int, [Int])
instance GCount THasOne where gCount _ = (1, [0])
instance GCount THasMany where gCount _ = (1, [0])
instance GCount THasZero where gCount _ = (1, [])

-- | Class of types used as tag arguments to 'gGetFieldVal' and
-- 'gGetFieldPos'.  @f@ should be a new unit type of kind @* -> *@,
-- used to designate the type of extraction you want.  Then instances
-- should be defined to transform each type @a@ you want to extract to
-- some type @r@, with @g@ set to 'THasOne'.
--
-- For example, 'ExtractMaybe' is a type to convert types @a@ and
-- @Maybe a@ both to type @Maybe a@ (type argument @r@).
--
-- > data ExtractMaybe a = ExtractMaybe
-- > instance Extractor ExtractMaybe a (Maybe a) THasOne where
-- >   extract _ = THasOne . Just
-- > instance Extractor ExtractMaybe (Maybe a) (Maybe a) THasOne where
-- >   extract _ = THasOne
class Extractor f a r g | f a r -> g where
  extract :: f r -> a -> g r
instance (TypeGCast THasZero g) => Extractor f a r g where
  extract _ _ = typeGCast THasZero

-- | Generlized extraction of a field from a 'Generic' data structure.
-- Argument @a@ should generally be the type @'Rep' t@ for some data
-- type @t@ whose fields you want to extract.  @r@ is the type you
-- want to extract from fields of data structure @t@.  @f@ should be
-- defined such that there is an instance @'Extractor' f a r THasOne@
-- exists for each type @a@ you want to extract.
class GGetField f a r g | f a r -> g where
  gGetFieldVal :: f r -> a p -> g r
  -- ^ Returns zero, one, or multiple values of type @f@ wrapped in
  -- 'THasOne', 'THasZero', or 'THasMany' respectively.
  gGetFieldPos :: f r -> a p -> (Int, [Int])
  -- ^ Returns @(total, positions)@ where @total@ is the total number
  -- of fields (matching or not) in the structure and @positions@ is a
  -- list of zero-based field numbers of the fields matching target
  -- type @f r@.
instance (Extractor f c r g, GCount g) => GGetField f (K1 i c) r g where
  {-# INLINE gGetFieldVal #-}
  gGetFieldVal f (K1 c) = extract f c
  gGetFieldPos f k = gCount (gGetFieldVal f k)
instance (GGetField f a1 r g1, GGetField f a2 r g2, GCombine g1 g2 g) =>
         GGetField f (a1 :*: a2) r g where
           {-# INLINE gGetFieldVal #-}
           gGetFieldVal f (a1 :*: a2) =
             gCombine (gGetFieldVal f a1) (gGetFieldVal f a2)
           gGetFieldPos f ~(a1 :*: a2) = (n1 + n2, p1 ++ map (n1 +) p2)
             where (n1, p1) = gGetFieldPos f a1
                   (n2, p2) = gGetFieldPos f a2
instance (GGetField f a r g) => GGetField f (M1 i c a) r g where
  {-# INLINE gGetFieldVal #-}
  gGetFieldVal f (M1 a) = gGetFieldVal f a
  gGetFieldPos f ~(M1 a) = gGetFieldPos f a


class (Generic a, GGetField f (Rep a) r THasOne) => GetField f a r where
  -- | Extract the single field matching 'Extractor' @f r@ from a
  -- 'Generic' data structure @a@ where @a@ has exactly one
  -- constructor.
  getFieldVal :: f r -> a -> r
  -- | Extract the 0-based position of the single field matching
  -- 'Extractor' @f r@ within 'Generic' data structure @a@.
  -- Non-strict in both arguments.
  getFieldPos :: f r -> a -> Int
instance (Generic a, GGetField f (Rep a) r THasOne) => GetField f a r where
  getFieldVal f a = fromTHasOne $ gGetFieldVal f (from a)
  getFieldPos f a = head $ snd $ gGetFieldPos f (from a)

-- | An extractor that matches an exact field type.
data ExtractId r = ExtractId deriving (Show)
instance Extractor ExtractId a a THasOne where
  {-# INLINE extract #-}
  extract _ = THasOne

-- | An extractor that matches either type @r@ or type @Maybe r@, and,
-- in the former case, wraps @Just@ around the value so as to always
-- return type @Maybe r@.
data ExtractMaybe r = ExtractMaybe
instance Extractor ExtractMaybe a (Maybe a) THasOne where
  {-# INLINE extract #-}
  extract _ = THasOne . Just
instance Extractor ExtractMaybe (Maybe a) (Maybe a) THasOne where
  {-# INLINE extract #-}
  extract _ = THasOne

