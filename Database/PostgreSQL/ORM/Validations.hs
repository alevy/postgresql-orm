{-# LANGUAGE FlexibleContexts#-}
module Database.PostgreSQL.ORM.Validations where

import qualified Data.ListLike as LL
--import Text.Regex.Posix

data InvalidError = InvalidError
  { invalidColumn :: String
  , invalidError  :: String } deriving (Show)

type ValidationFunc a = a -> [InvalidError]

validate :: (a -> Bool)
         -> String -- ^ Column name
         -> String -- ^ Error description
         -> ValidationFunc a
validate validator columnName desc = \a ->
  if validator a then
    []
    else [InvalidError columnName desc]

validateLength :: LL.ListLike lst elm
               => (a -> lst)
               -> Int
               -> String
               -> String
               -> ValidationFunc a
validateLength accessor len =
  validate (\a -> LL.length (accessor a) > len)

validateNotEmpty :: LL.ListLike lst elm
                 => (a -> lst)
                 -> String
                 -> String
                 -> ValidationFunc a
validateNotEmpty accessor = validate (not . LL.null . accessor)

--validateFormat :: (RegexMaker Regex CompOption ExecOption format,
--                  RegexLike Regex str)
--               => (a -> str)
--               -> format
--               -> String
--               -> String
--               -> ValidationFunc a
--validateFormat accessor format = validate (\a -> accessor a =~ format)
