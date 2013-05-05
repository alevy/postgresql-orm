{-# LANGUAGE CPP #-}

-- | Helper routines for creating 'undefined' values or
-- type-constricted versions of 'const' based on sub-components of
-- another type.  The mnemonic for remembering these names is:
--
--   * @undef/N/@ returns an 'undefined' value of the same type as the
--   @/N/@th type argument of its input type, counting from the right.
--   So @/N/@ is the number of type arguments in the input type, i.e.,
--   @a@, @b@, and @c@ in the following:
--
--   >    undef1 :: g a -> a
--   >    undef2 :: g a b -> a
--   >    undef3 :: g a b c -> a
--
--   * @undef/M/_/N/@ is equivalent to @undef/N/ . undef/M/@ -- i.e.,
--   the @/M/@th type argument of the @/N/@th type argument.  For
--   example:
--
--   >    undef1_2 :: g1 (g a b) -> a
--   >    undef2_1 :: g1 (g a) b1 -> a
--
--   * @asTypeOf/N/@ is equivalent to @\\a t -> a `asTypeOf` undef/N/ t@.
--
--   * @gAsTypeOf/N/@ is like @asTypeOf/N/@, but constraints a type
--   argument rather than a type.  This is analogous to 'gcast'
--   ('gcast1', etc.) in "Data.Typeable".  For example:
--
--   >    asTypeOf2  ::   a -> g a b ->   a
--   >    gAsTypeOf2 :: m a -> g a b -> m a
module Data.AsTypeOf (asTypeOf, module Data.AsTypeOf) where

gAsTypeOf :: m a -> a -> m a
gAsTypeOf = const

undef :: a -> a
{-# INLINE undef #-}
undef _ = undefined

-- The CPP language extension seems to use a pre-ansi C preprocessor.
-- If that ever changes, then /**/ will have to change to ##.
#define U(n, t)                           \
  asTypeOf/**/n :: a -> t -> a;           \
  {-# INLINE asTypeOf/**/n #-};           \
  asTypeOf/**/n = const;                  \
  gAsTypeOf/**/n/**/ :: m a -> t -> m a;  \
  {-# INLINE gAsTypeOf/**/n/**/ #-};      \
  gAsTypeOf/**/n/**/ = const;             \
  undef/**/n :: t -> a;                   \
  {-# INLINE undef/**/n #-};              \
  undef/**/n _ = undefined;

U (1, g a)
U (2, g a b)
U (3, g a b c)
U (4, g a b c d)
U (5, g a b c d e)

U (1_1, g1 (g a))
U (1_2, g1 (g a b))
U (1_3, g1 (g a b c))
U (1_4, g1 (g a b c d))
U (1_5, g1 (g a b c d e))

U (2_1, g1 (g a) b1)
U (2_2, g1 (g a b) b1)
U (2_3, g1 (g a b c) b1)
U (2_4, g1 (g a b c d) b1)
U (2_5, g1 (g a b c d e) b1)

#undef U
