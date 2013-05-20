{-# LANGUAGE OverloadedStrings #-}

-- | Utility function for describing a table in the database.
module Database.PostgreSQL.Describe (
  ColumnInfo(..), describeTable
  ) where

import Control.Monad
import qualified Data.ByteString as S
import Data.Int
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as PG
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.TypeInfo
import Database.PostgreSQL.Simple.Types

data ColumnInfo = ColumnInfo {
    colNum :: !Int16
    -- ^ Internal column number used by PostgreSQL.  Generally these
    -- will be consecutive starting from 1, but this may not be the
    -- case if you have altered a table to delete columns.
  , colName :: S.ByteString
    -- ^ Name of the column
  , colType :: !TypeInfo
    -- ^ Type of the column
  , colNotNull :: !Bool
    -- ^ If 'True', the database cannot contain null.  (This
    -- constraint should always be accurate.)
  , colPrimary :: !Bool
    -- ^ 'True' if this column (and only this column) constitutes the
    -- primary key of the table.  Always 'False' if the primary key
    -- comprises multiple columns (even if this is one of those
    -- columns).
  , colUnique :: !Bool
    -- ^ 'True' if there is a uniqueness constraint on this column.
    -- Not 'True' if this column is part of a uniqueness constraint
    -- involving multiple columns.  (Such multi-column uniqueness
    -- consraints are not reported by this interface.)
  , colReferences :: !(Maybe S.ByteString)
    -- ^ If this there is a foreign key constraint on this column (and
    -- the constraint does not span multiple columns), report the
    -- table referenced by this column.
  } deriving (Show)

defColInfo :: ColumnInfo
defColInfo = ColumnInfo {
    colNum = 0
  , colName = S.empty
  , colType = PG.void
  , colNotNull = False
  , colPrimary = False
  , colUnique = False
  , colReferences = Nothing
  }

-- | Returns a list of 'ColumnInfo' structures for a particular table.
-- Not all information about a table is returned.  In particular,
-- constraints that span columns are ignored.
describeTable :: Connection -> S.ByteString -> IO [ColumnInfo]
describeTable cn t = do
  [(Only tbloid)] <- query cn "select oid from pg_class where relname = ?"
                     (Only t)
  cs0 <- query cn "select attnum, attname, atttypid, attnotnull\
                  \ from pg_attribute\
                  \ where attrelid = ? and attisdropped = 'f' and attnum > 0\
                  \ order by attnum"
                  (Only (tbloid :: Oid))
  cs1 <- forM cs0 $ \ (num, name, typ, notnull) -> do
    ti <- getTypeInfo cn typ
    return defColInfo {
      colNum = num, colName = name, colType = ti, colNotNull = notnull
    }
  constraints <- query cn "select contype, conkey, relname\
                          \ from pg_constraint left join pg_class\
                          \ on confrelid = pg_class.oid\
                          \ where conrelid = ?"
                          (Only tbloid)
  let _ = constraints :: [(String, V.Vector Int16, Maybe S.ByteString)]
  return $ map (\c -> foldl appConstr c constraints) cs1
    where appConstr ci (ct, ck, mn)
            | V.length ck == 1, colNum ci == ck V.! 0 = appConstr1 ci ct mn
            | otherwise                           = ci
          appConstr1 ci "p" _ = ci { colPrimary = True }
          appConstr1 ci "u" _ = ci { colUnique = True }
          appConstr1 ci "f" n@(Just _) = ci { colReferences = n }
          appConstr1 ci _ _ = ci
