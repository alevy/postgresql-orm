{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functions to extract a field of a particular type from a
-- 'Generic' data structure, when the data structure contains exactly
-- one field of the given type.  Only works for types with exactly one
-- constructor (not variant types).
--
-- An example of usage:
--
-- > data MyType = MyType { myString :: String             -- position 0
-- >                      , myInt :: Int                   -- position 1
-- >                      , myBool :: Bool                 -- position 2
-- >                      , myMaybeChar :: Maybe Char      -- position 3
-- >                      , myMaybeString :: Maybe String  -- position 4
-- >                      } deriving (Show, Generic)
-- > 
-- > myType :: MyType
-- > myType = MyType "my type" 21 True Nothing (Just "maybe string")
--
-- >>> getFieldVal ExtractId myType :: String
-- "my type"
-- >>> getFieldVal ExtractId myType :: Int
-- 21
-- >>> getFieldVal ExtractMaybe myType :: Maybe Char
-- Nothing
-- >>> getFieldVal ExtractMaybe myType :: Maybe Int
-- Just 21
-- >>> getFieldVal ExtractMaybe myType :: Maybe String  -- ambiguous
-- <interactive>:5:1: Couldn't match type `THasMany' with `THasOne'
-- >>> getFieldPos' ExtractId (undefined :: MyType) (undefined :: Bool)
-- 2
-- >>> getFieldPos' ExtractMaybe (undefined :: MyType) (undefined :: Maybe Bool)
-- 2
-- >>> getFieldPos' ExtractMaybe myType ()  -- No field has type ()
-- <interactive>:8:1: Couldn't match type `THasNone' with `THasOne'
module Data.GetField (
  GetField(..), ExtractId(..), ExtractMaybe(..), getFieldPos'
  -- * Internals
  , THasOne(..), THasNone(..), THasMany(..), Extractor(..), GGetField(..)
  ) where

import GHC.Generics

-- | Dirty trick to construct "less specific" overlapping instances by
-- making a class argument a simple type variable, but constraining
-- that variable to be a particular type.  E.g., neither of the
-- following two instances is more specific than the other, because
-- @NO@ is not more general than @YES@:
--
-- > class MyClass a b c | a b -> c where myClass :: a -> b -> c ()
-- > instance MyClass a a YES       where myClass _ _ = YES ()
-- > instance MyClass a b NO        where myClass _ _ = NO ()
--
-- Hence, attempting to use the first instance will generate a
-- compilation error rather than inferring the type of c as YES.  On
-- the other hand, of the following two instances, the first is more
-- specific than the second:
--
-- > instance MyClass a a YES where
-- >     myClass _ _ = YES ()
-- > instance (TypeGCast NO c) => MyClass a b c where
-- >     myClass _ _ = typeGCast $ NO ()
--
-- That's because @c@ is more general than @YES@.  The key to this
-- working is that an instance context--i.e., @(TypeGCast NO c)@--is
-- never consulted during instance selection, only to validate an
-- already-selected most-specific instance.
--
-- Note that @YES@ and @NO@ in these examples have kind @* -> *@.
-- Hence the @G@ in @TypeGCast@.  The same trick is equally applicable
-- to types of kind @*@, we just don't happen to need that in this
-- module.
class TypeGCast f g | f -> g where
  typeGCast :: f p -> g p
instance TypeGCast f f where
  typeGCast = id

-- | Exactly one matching field has been found.
newtype THasOne a = THasOne { fromTHasOne :: a } deriving (Show)
-- | Zero matching fields have been found.
data THasNone a = THasNone deriving (Show)
-- | More than one matching field has been found.
newtype THasMany a = THasMany { fromTHasMany :: [a] } deriving (Show)

class GCombine a b c | a b -> c where
  gCombine :: a p -> b p -> c p
instance GCombine THasOne THasNone THasOne where
  {-# INLINE gCombine #-}
  gCombine j _ = j
instance GCombine THasNone THasOne THasOne where
  {-# INLINE gCombine #-}
  gCombine _ j = j
instance GCombine THasNone THasNone THasNone where
  -- Should never be evaluated, so no need to inline it
  gCombine _ _ = THasNone
instance GCombine THasOne THasOne THasMany where
  {-# INLINE gCombine #-}
  gCombine (THasOne a) (THasOne b) = THasMany [a,b]
instance GCombine THasMany THasMany THasMany where
  {-# INLINE gCombine #-}
  gCombine (THasMany as) (THasMany bs) = THasMany (as ++ bs)
instance GCombine THasNone THasMany THasMany where
  {-# INLINE gCombine #-}
  gCombine _ hm = hm
instance GCombine THasMany THasNone THasMany where
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
instance GCount THasNone where gCount _ = (1, [])

-- | Class of types used as tag arguments to 'gGetFieldVal' and
-- 'gGetFieldPos'.  @f@ should be a new unit type of kind @* -> *@,
-- used to designate the type of extraction you want.  Then instances
-- should be defined to transform each type @a@ you want to extract to
-- some type @r@, with @g@ set to 'THasOne'.
--
-- For example, 'ExtractMaybe' is a type to convert types @a@ and
-- @Maybe a@ both to type @Maybe a@ (i.e., type argument @r@ is @Maybe
-- a@).
--
-- > data ExtractMaybe a = ExtractMaybe
-- > instance Extractor ExtractMaybe a (Maybe a) THasOne where
-- >   extract _ = THasOne . Just
-- > instance Extractor ExtractMaybe (Maybe a) (Maybe a) THasOne where
-- >   extract _ = THasOne
--
-- Note that there is already a default general instance returning
-- 'THasNone'.  Hence, you do not need to define one.  Otherwise, you
-- would have to define an overlapping instance such as:
--
-- > instance Extractor ExtractMaybe a b THasZero where  -- Incorrect
-- >   extract _ = THasNone
--
-- (Except the above wouldn't quite work anyway given the rules for
-- overlapping instances.)  So just assume that any instance you don't
-- explicitly define for your 'Extractor' will automatically fall back
-- to 'THasNone'.
class Extractor f a r g | f a r -> g where
  extract :: f r -> a -> g r
  extractCount :: f r -> a -> (Int, [Int])
  default extractCount :: (GCount g) => f r -> a -> (Int, [Int])
  extractCount fr a = gCount (extract fr a)
instance (TypeGCast THasNone g) => Extractor f a r g where
  extract _ _ = typeGCast THasNone
  extractCount _ _ = gCount THasNone

-- | Generlized extraction of a field from a 'Generic' data structure.
-- Argument @rep@ should generally be the type @'Rep' t@ for some data
-- type @t@ whose fields you want to extract.  @r@ is the result type
-- you want back from the extraction.  @f@ should be defined such that
-- there is an instance of @'Extractor' f a r THasOne@ for each type
-- @a@ you want to convert to @r@ and extract.
class GGetField f rep r g | f rep r -> g where
  gGetFieldVal :: f r -> rep p -> g r
  -- ^ Returns zero, one, or multiple values of type @f@ wrapped in
  -- 'THasOne', 'THasNone', or 'THasMany' respectively.
  gGetFieldPos :: f r -> rep p -> (Int, [Int])
  -- ^ Returns @(total, positions)@ where @total@ is the total number
  -- of fields (matching or not) in the structure and @positions@ is a
  -- list of zero-based field numbers of the fields matching target
  -- type @f r@.
instance (Extractor f c r g) => GGetField f (K1 i c) r g where
  {-# INLINE gGetFieldVal #-}
  gGetFieldVal f (K1 c) = extract f c
  gGetFieldPos f (K1 c) = extractCount f c
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
  -- 'Generic' data structure @a@ with exactly one constructor.
  getFieldVal :: f r -> a -> r
  -- | Extract the 0-based position of the single field matching
  -- 'Extractor' @f r@ within 'Generic' data structure @a@.
  -- Non-strict in both arguments.
  getFieldPos :: f r -> a -> Int
instance (Generic a, GGetField f (Rep a) r THasOne) => GetField f a r where
  {-# INLINE getFieldVal #-}
  getFieldVal f a = fromTHasOne $ gGetFieldVal f (from a)
  getFieldPos f a = head $ snd $ gGetFieldPos f (from a)

-- | A variant of 'getFieldPos' in which the type of the field is
-- supplied as a non-strict argument.  This may be easier than
-- typecasting the extractor argument.  For example, to extract the
-- 'Int' from a structure with a single 'Int' field:
--
-- @
--       getFieldPos' 'ExtractId' myStruct ('undefined' :: 'Int')
-- @
getFieldPos' :: (Generic a, GGetField f (Rep a) r THasOne) =>
                (f ()) -> a -> r -> Int
getFieldPos' f a r = getFieldPos (fixType f r) a
  where fixType :: f () -> r -> f r
        fixType _ _ = undefined

-- | An extractor that matches an exact field type.
data ExtractId r = ExtractId deriving (Show)
instance Extractor ExtractId a a THasOne where
  {-# INLINE extract #-}
  extract _ = THasOne

-- | An extractor that matches either type @r@ or type @Maybe r@, and,
-- in the former case, wraps @Just@ around the value so as always to
-- return type @Maybe r@.
data ExtractMaybe r = ExtractMaybe
instance Extractor ExtractMaybe a (Maybe a) THasOne where
  {-# INLINE extract #-}
  extract _ = THasOne . Just
instance Extractor ExtractMaybe (Maybe a) (Maybe a) THasOne where
  {-# INLINE extract #-}
  extract _ = THasOne

