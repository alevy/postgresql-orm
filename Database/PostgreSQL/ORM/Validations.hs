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

newtype ValidationError = ValidationError
  { validationErrors :: H.HashMap T.Text [T.Text] } deriving (Show, Typeable)

instance Exception ValidationError

instance Monoid ValidationError where
  mempty = ValidationError mempty
  mappend ein zwei = ValidationError $!
    H.unionWith mappend (validationErrors ein) (validationErrors zwei)

instance ToJSON ValidationError where
  toJSON = toJSON . validationErrors

instance FromJSON ValidationError where
  parseJSON val = ValidationError `fmap` parseJSON val

type ValidationFunc a = a -> ValidationError

validate :: (a -> Bool)
         -> T.Text -- ^ Column name
         -> T.Text -- ^ Error description
         -> ValidationFunc a
validate validator columnName desc = \a ->
  if validator a then
    ValidationError H.empty
    else ValidationError $ H.singleton columnName [desc]

validateNotEmpty :: (a -> T.Text)
                 -> T.Text
                 -> T.Text
                 -> ValidationFunc a
validateNotEmpty accessor = validate (not . T.null . accessor)

