{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module CompilerUtils (
  fromList, Migration(..), MigrationMap, compiledMain
) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM_, void)
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Database.PostgreSQL.Simple
  (Connection, Only(..), execute, query_, begin, commit)
import Database.PostgreSQL.Migrations (connectEnv)
import Database.PostgreSQL.Migrate (initializeDb)
import System.Environment (getArgs, getProgName)

type Version = String

data Migration = Migration { migName :: String
                           , migUp :: Connection -> IO ()
                           , migDown :: Connection -> IO ()
                           }

type MigrationMap = Map Version Migration

compiledMain :: MigrationMap -> IO ()
compiledMain migrations = do
  args <- getArgs
  case args of
    "init":[] -> initializeDb
    "list":[] -> listMigrations migrations
    "migrate":[] -> runMigrations migrations
    "rollback":[] -> runRollback migrations
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " migrate|rollback"
      putStrLn $ "       " ++ progName ++ " list"
      putStrLn $ "       " ++ progName ++ " init"

listMigrations :: MigrationMap -> IO ()
listMigrations migrations =
  forM_ (M.toAscList migrations) $ \(_, Migration name _ _) -> putStrLn name

-- | Runs all new migrations in a given directory and dumps the
-- resulting schema to a file \"schema.sql\" in the migrations
-- directory.
--
-- Determining which migrations to run is done by querying the database for the
-- largest version in the /schema_migrations/ table, and choosing all
-- migrations in the given directory with higher versions.
runMigrations :: MigrationMap -> IO ()
runMigrations migrationsIn = do
  conn <- connectEnv
  res <- query_ conn
         "select version from schema_migrations order by version desc limit 1"
  let latestVersion = case res of
                        [] -> ""
                        (Only latest):_ -> latest
  let migrations = M.toAscList $
        M.filterWithKey (\k _ -> k > latestVersion) migrationsIn
  forM_ migrations (doone conn)
  where doone conn (version, Migration name up down) = do
          putStrLn $ "=== Running Migration " ++ name
          ok <- catch
            (do
                begin conn
                void $ execute conn "insert into schema_migrations values(?)"
                  (Only version)
                up conn
                commit conn
                return True)
            (\(e :: SomeException) -> return False)
          if ok
            then putStrLn "=== Success"
            else putStrLn "=== Migration Failed!"

runRollback :: MigrationMap -> IO ()
runRollback migrations = do
  conn <- connectEnv
  res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
  case res of
    [] -> putStrLn "=== DB Fully Rolled Back!"
    (Only latest):_ -> do
      let (Migration name _ down) = migrations M.! latest
      putStrLn $ "=== Running Rollback " ++ name
      ok <- catch
        (do
            begin conn
            down conn
            void $ execute conn "delete from schema_migrations where version = ?"
              (Only latest)
            commit conn
            return True)
        (\(e :: SomeException) -> return False)
      if ok
        then putStrLn "=== Success"
        else putStrLn "=== Migration Failed!"
