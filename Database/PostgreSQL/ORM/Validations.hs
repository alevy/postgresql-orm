{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, OverloadedStrings #-}
module Database.PostgreSQL.ORM.Validations where

import Control.Exception
import Data.Aeson
import qualified Data.Text as T
import Data.Typeable
import qualified Data.ByteString.Char8 as S8

data InvalidError = InvalidError
  { invalidColumn :: !S8.ByteString
  , invalidError  :: !S8.ByteString } deriving (Show)

instance ToJSON InvalidError where
  toJSON ie = object ["column" .= invalidColumn ie, "error" .= invalidError ie]

newtype ValidationError = ValidationError [InvalidError]
  deriving (Show, Typeable)

instance Exception ValidationError

type ValidationFunc a = a -> [InvalidError]

validate :: (a -> Bool)
         -> S8.ByteString -- ^ Column name
         -> S8.ByteString -- ^ Error description
         -> ValidationFunc a
validate validator columnName desc = \a ->
  if validator a then
    []
    else [InvalidError columnName desc]

validateNotEmpty :: (a -> T.Text)
                 -> S8.ByteString
                 -> S8.ByteString
                 -> ValidationFunc a
validateNotEmpty accessor = validate (not . T.null . accessor)

