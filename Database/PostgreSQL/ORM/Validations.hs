{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, OverloadedStrings #-}
module Database.PostgreSQL.ORM.Validations where

import Control.Exception
import Data.Aeson
import qualified Data.Text as T
import Data.Typeable

data InvalidError = InvalidError
  { invalidColumn :: !T.Text
  , invalidError  :: !T.Text } deriving (Show)

instance ToJSON InvalidError where
  toJSON ie = object [ "column" .= invalidColumn ie
                     , "error" .= invalidError ie]

newtype ValidationError = ValidationError [InvalidError]
  deriving (Show, Typeable)

instance Exception ValidationError

type ValidationFunc a = a -> [InvalidError]

validate :: (a -> Bool)
         -> T.Text -- ^ Column name
         -> T.Text -- ^ Error description
         -> ValidationFunc a
validate validator columnName desc = \a ->
  if validator a then
    []
    else [InvalidError columnName desc]

validateNotEmpty :: (a -> T.Text)
                 -> T.Text
                 -> T.Text
                 -> ValidationFunc a
validateNotEmpty accessor = validate (not . T.null . accessor)

