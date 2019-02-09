{-# LANGUAGE CPP, FlexibleContexts, DeriveDataTypeable, OverloadedStrings #-}
module Database.PostgreSQL.ORM.Validations where

import Control.Exception
import Data.Aeson
import qualified Data.HashMap.Strict as H
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import qualified Data.Text as T
import Data.Typeable
#if __GLASGOW_HASKELL >= 806
import Data.Semigroup
#endif

newtype ValidationError = ValidationError
  { validationErrors :: H.HashMap T.Text [T.Text] } deriving (Show, Typeable)

instance Exception ValidationError

#if __GLASGOW_HASKELL__ >= 806
instance Semigroup ValidationError where
  ein <> zwei = ValidationError $!
    H.unionWith mappend (validationErrors ein) (validationErrors zwei)
#endif

instance Monoid ValidationError where
  mempty = ValidationError mempty
#if __GLASGOW_HASKELL < 806
  mappend ein zwei = ValidationError $!
    H.unionWith mappend (validationErrors ein) (validationErrors zwei)
#endif

instance ToJSON ValidationError where
  toJSON = toJSON . validationErrors

instance FromJSON ValidationError where
  parseJSON val = ValidationError `fmap` parseJSON val

type ValidationFunc a = a -> ValidationError

validationError :: T.Text -> T.Text -> ValidationError
validationError columnName description =
    ValidationError $ H.singleton columnName [description]

validate :: (a -> Bool)
         -> T.Text -- ^ Column name
         -> T.Text -- ^ Error description
         -> ValidationFunc a
validate validator columnName desc = \a ->
  if validator a
    then mempty
    else validationError columnName desc

validateNotEmpty :: (a -> T.Text)
                 -> T.Text
                 -> T.Text
                 -> ValidationFunc a
validateNotEmpty accessor = validate (not . T.null . accessor)

