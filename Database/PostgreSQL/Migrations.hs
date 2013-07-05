{-# LANGUAGE OverloadedStrings #-}

{- |
Example migration:

> import Database.PostgreSQL.Migrations
> 
> up = migrate $ do
>       create_table "posts"
>         [ column "title" "VARCHAR(255) NOT NULL"
>         , column "author_id" "integer references authors(id)"]
> 
> down = migrate $ drop_table "posts"
> 
> main = defaultMain up down

-}
module Database.PostgreSQL.Migrations (
    -- * Utilities
    defaultMain
  , connectEnv
  , runSqlFile
    -- * DSL
  , Migration, migrate
  , column
    -- ** Adding
  , create_table
  , add_column
    -- ** Removing
  , drop_table
  , drop_column
    -- ** Modifying
  , rename_column
  , change_column
    -- ** Statements
  , create_table_stmt, add_column_stmt, drop_table_stmt, drop_column_stmt
  , rename_column_stmt, change_column_stmt
  ) where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Int
import Data.Maybe
import Database.PostgreSQL.Simple hiding (connect)
import Database.PostgreSQL.Simple.Internal (exec)
import Database.PostgreSQL.Simple.Types
import System.Environment
import System.Exit

-- | Creates a PostgreSQL 'Connection' using the /DATABASE_URL/ environment
-- variable, if it exists. If it does, it should match the format:
--
-- @
--   postgresql:\/\/[[USERNAME\@PASSWORD]HOSTNAME[:PORT]]/[DBNAME]
-- @
--
-- If it is not present, the environment variables /PG_DBNAME/ /PG_HOST/ etc,
-- are used.
connectEnv :: IO Connection
connectEnv = do
  psqlStr <- getEnvironment >>= return . (fromMaybe "") . (lookup "DATABASE_URL")
  connectPostgreSQL $ S8.pack psqlStr

--
-- Migration Monad
--

type Migration = ReaderT Connection IO

migrate :: Migration a -> Connection -> IO ()
migrate = (void .) . runReaderT

executeQuery_ :: Query -> Migration Int64
executeQuery_ q = ask >>= \conn -> liftIO $ execute_ conn q

-- | Runs the SQL file at the given path, relative to the current working
-- directory.
runSqlFile :: FilePath -> Migration ()
runSqlFile sqlFile = void $ do
    conn <- ask
    liftIO $ do
      rawSql <- S.readFile sqlFile
      exec conn rawSql

-- | Returns a column defition by quoting the given name
column :: S8.ByteString -- ^ name
       -> S8.ByteString -- ^ type, definition, constraints
       -> S8.ByteString
column name def = S8.concat ["\"", name, "\" ", def]

-- | Creates a table. See 'column' for constructing the column list.
create_table :: S8.ByteString
             -- ^ Table name
             -> [S8.ByteString]
             -- ^ Column definitions
             -> Migration Int64
create_table = (executeQuery_ .) . create_table_stmt

-- | Returns a 'Query' that creates a table, for example:
--
-- @
--   create_table \"posts\"
--     [ column \"title\" \"VARCHAR(255) NOT NULL\"
--     , column \"body\"  \"text\"]
-- @
create_table_stmt :: S8.ByteString
                  -- ^ Table name
                  -> [S8.ByteString]
                  -- ^ Column definitions
                  -> Query
create_table_stmt tableName colDefs = Query $ S8.concat $
  [ "create table \""
  , tableName
  , "\" ("] ++ (S8.intercalate ", " colDefs):([");"])

-- | Drops a table
drop_table :: S8.ByteString -> Migration Int64
drop_table = executeQuery_ . drop_table_stmt

-- | Returns a 'Query' that drops a table
drop_table_stmt :: S8.ByteString -> Query
drop_table_stmt tableName = Query $ S8.concat
  [ "drop table \"", tableName, "\";"]

-- | Adds a column to the given table. For example,
--
-- @
--   add_column \"posts\" \"title\" \"VARCHAR(255)\"
-- @
--
-- adds a varchar column called \"title\" to the table \"posts\".
--
add_column :: S8.ByteString
           -- ^ Table name
           -> S8.ByteString
           -- ^ Column name
           -> S8.ByteString
           -- ^ Column definition
           -> Migration Int64
add_column  = ((executeQuery_ .) .) . add_column_stmt

-- | Returns a 'Query' that adds a column to the given table. For example,
--
-- @
--   add_column \"posts\" \"title\" \"VARCHAR(255)\"
-- @
--
-- Returns the query
--
-- @
--   ALTER TABLE \"posts\" add \"title\" VARCHAR(255);
-- @
add_column_stmt :: S8.ByteString
                -- ^ Table name
                -> S8.ByteString
                -- ^ Column name
                -> S8.ByteString
                -- ^ Column definition
                -> Query
add_column_stmt tableName colName colDef = Query $ S8.concat
  [ "alter table \"" , tableName , "\" add ", column colName colDef, ";"]

-- | Drops a column from the given table. For example,
--
-- @
--   drop_column \"posts\" \"title\"
-- @
--
-- drops the column \"title\" from the \"posts\" table.
drop_column :: S8.ByteString
            -- ^ Table name
            -> S8.ByteString
            -- ^ Column name
            -> Migration Int64
drop_column = (executeQuery_ .) . drop_column_stmt

-- | Returns a 'Query' that drops a column from the given table. For example,
--
-- @
--   drop_column \"posts\" \"title\"
-- @
--
-- Returns the query
--
-- @
--   ALTER TABLE \"posts\" add \"title\";
-- @
drop_column_stmt :: S8.ByteString
                 -- ^ Table name
                 -> S8.ByteString
                 -- ^ Column name
                 -> Query
drop_column_stmt tableName colName = Query $ S8.concat
  ["alter table \"", tableName, "\" drop \"", colName, "\";"]

-- | Renames a column in the given table. For example,
--
-- @
--   rename_column \"posts\" \"title\" \"name\"
-- @
--
-- renames the column \"title\" in the \"posts\" table to \"name\".
rename_column :: S8.ByteString
              -- ^ Table name
              -> S8.ByteString
              -- ^ Old column name
              -> S8.ByteString
              -- ^ New column name
              -> Migration Int64
rename_column = ((executeQuery_ .) .) . rename_column_stmt

-- | Returns a 'Query' that renames a column in the given table. For example,
--
-- @
--   rename_column \"posts\" \"title\" \"name\"
-- @
--
-- Returns the query
--
-- @
--   ALTER TABLE \"posts\" RENAME \"title\" TO \"name\";
-- @
rename_column_stmt :: S8.ByteString
                   -- ^ Table name
                   -> S8.ByteString
                   -- ^ Old column name
                   -> S8.ByteString
                   -- ^ New column name
                   -> Query
rename_column_stmt tableName colName colNameNew = Query $ S8.concat
  [ "alter table \"", tableName, "\" rename \""
  , colName, "\" to \"", colNameNew, "\";"]

-- | Alters a column in the given table. For example,
--
-- @
--   change_column \"posts\" \"title\" \"DROP DEFAULT\"
-- @
--
-- drops the default constraint for the \"title\" column in the \"posts\"
-- table.
change_column :: S8.ByteString
              -- ^ Table name
              -> S8.ByteString
              -- ^ Column name
              -> S8.ByteString
              -- ^ Action
              -> Migration Int64
change_column = ((executeQuery_ .) .) . change_column_stmt

-- | Returns a 'Query' that alters a column in the given table. For example,
--
-- @
--   change_column \"posts\" \"title\" \"DROP DEFAULT\"
-- @
--
-- Returns the query
--
-- @
--   ALTER TABLE \"posts\" ALTER \"title\" DROP DEFAULT;
-- @
change_column_stmt :: S8.ByteString
                   -- ^ Table name
                   -> S8.ByteString
                   -- ^ Column name
                   -> S8.ByteString
                   -- ^ Action
                   -> Query
change_column_stmt tableName colName action = Query $ S8.concat
  [ "alter table \"", tableName, "\" alter \""
  , colName, "\" ", action, ";"]

data CmdArgs = CmdArgs { cmd :: String
                       , cmdVersion :: String
                       , cmdCommit :: Bool }

parseCmdArgs :: [String] -> Maybe CmdArgs
parseCmdArgs args = do
  mycmd <- listToMaybe args
  let args0 = tail args
  myversion <- listToMaybe args0
  return $ go (CmdArgs mycmd myversion False) $ tail args0
  where go res [] = res
        go res (arg:as) =
          let newRes = case arg of
                        "--with-db-commit" -> res { cmdCommit = True }
                        _ -> res
          in go newRes as

defaultMain :: (Connection -> IO ()) -- ^ Migration function
            -> (Connection -> IO ()) -- ^ Rollback function
            -> IO ()
defaultMain up down = do
  (Just cmdArgs) <- getArgs >>= return . parseCmdArgs
  case cmd cmdArgs of
    "up" -> do
      conn <- connectEnv
      res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
      let currentVersion = case res of
                      [] -> ""
                      (Only v):_ -> v
      let version = cmdVersion cmdArgs
      if currentVersion < version then do
          begin conn
          up conn
          void $ execute conn "insert into schema_migrations values(?)"
                              (Only version)
          if cmdCommit cmdArgs then
            commit conn
            else rollback conn
        else exitWith $ ExitFailure 1
    "down" -> do
      conn <- connectEnv
      res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
      let currentVersion = case res of
                      [] -> ""
                      (Only v):_ -> v
      let version = cmdVersion cmdArgs
      if currentVersion == version then do
          begin conn
          down conn
          void $ execute conn "delete from schema_migrations where version = ?"
              (Only version)
          if cmdCommit cmdArgs then
            commit conn
            else rollback conn
        else
          exitWith $ ExitFailure 1
    _ -> exitWith $ ExitFailure 1

