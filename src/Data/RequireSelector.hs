{-# LANGUAGE FlexibleInstances, UndecidableInstances, CPP #-}

module Data.RequireSelector (RequireSelector) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 800)
import GHC.Generics

-- | There are intentionally no members of this class, so that placing
-- it in a context will always cause an error.
class IntentionallyCauseError a
#endif

-- | The point of this class is to ensure that you are using data
-- types defined with record selectors (i.e., @data Foo = Foo { unFoo
-- :: Int }@ as opposed to @data Foo = Foo Int@).
--
-- Unfortunately, "GHC.Generics" makes the 'NoSelector' type a member
-- of the 'Selector' class.  Hence, if you want to ensure a type @a@
-- is /not/ 'NoSelector', use the context @(RequireSelector a) =>@.
--
-- If you see a compilation error involving @RequireSelector@ or
-- @IntentionallyCauseError@, it means you failed to define one of
-- your datatypes using record selector syntax.
class RequireSelector a
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 800)
instance (IntentionallyCauseError NoSelector) => RequireSelector NoSelector
#endif
instance RequireSelector a
