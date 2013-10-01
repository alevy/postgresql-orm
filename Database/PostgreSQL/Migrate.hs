{-# LANGUAGE OverloadedStrings #-}

-- | Functions for creating and running database migrations. You should
-- probably be using the `pg_migrate` executable to run migrations, however
-- these functions are exposed for developers that want to integrate migrations
-- more tightly into their applications or utilities.

module Database.PostgreSQL.Migrate
  ( initializeDb
  , runMigrationsForDir
  , runRollbackForDir
  , dumpDb
  , defaultMigrationsDir
  , MigrationDetails(..)
  ) where

import Control.Monad
import Data.List
import Database.PostgreSQL.Simple hiding (connect)
import qualified Data.ByteString.Char8 as S8
import Database.PostgreSQL.Migrations
import System.Exit 
import GHC.IO.Handle
import System.Cmd
import System.Process
import System.Directory
import System.FilePath
import System.Environment
import System.IO

-- | The default relative path containing migrations: @\"db\/migrations\"@
defaultMigrationsDir :: FilePath
defaultMigrationsDir = "db" </> "migrations"

-- | Dumps the database schema to the given file handle.
--
-- This is a wrapper around the utility /pg_dump/ that comes with postgresql.
-- Therefore, /pg_dump/ must be installed on the system.
dumpDb :: Handle -> IO ExitCode
dumpDb outputFile = do
  dbUrl <- getEnvironment >>= return . maybe "" id . lookup "DATABASE_URL"
  (_, out, err, ph) <- runInteractiveProcess "pg_dump"
                        [dbUrl, "--schema-only", "-O", "-x"] Nothing Nothing
  exitCode <- waitForProcess ph
  if exitCode /= ExitSuccess then do
    S8.hGetContents err >>= S8.hPut stderr
    else do
      raw <- S8.hGetContents out
      let clean = S8.concat $ intersperse "\n" $
                    filter ((/= "--") . (S8.take 2)) $
                    S8.lines raw
      S8.hPut outputFile clean
  return exitCode

-- | Initializes the database by creating a \"schema-migrations\" table.
-- This table must exist before running any migrations.
initializeDb :: IO ()
initializeDb = do
  conn <- connectEnv
  void $ execute_ conn "create table schema_migrations (version VARCHAR(28))"

-- | Runs all new migrations in a given directory and dumps the
-- resulting schema to a file \"schema.sql\" in the migrations
-- directory.
--
-- Determining which migrations to run is done by querying the database for the
-- largest version in the /schema_migrations/ table, and choosing all
-- migrations in the given directory with higher versions.
runMigrationsForDir :: Handle -- ^ Log output (probably stdout)
                    -> FilePath -- ^ Path to directory containing migrations
                    -> IO ExitCode
runMigrationsForDir logOut dir = do
  conn <- connectEnv
  res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
  let latestVersion = case res of
                        [] -> ""
                        (Only latest):_ -> latest
  migrations <- getDirectoryMigrations dir >>=
                    return . (dropWhile (isVersion (<= latestVersion)))
  go migrations
  where go [] = withFile (dir </> ".." </> "schema.sql") WriteMode dumpDb
        go (mig@(MigrationDetails _ _ name):fs) = do
              hPutStrLn logOut $ "=== Running Migration " ++ name
              exitCode <- runMigration mig
              if exitCode == ExitSuccess then do
                hPutStrLn logOut "=== Success"
                go fs
                else do
                  hPutStrLn logOut "=== Migration Failed!"
                  return exitCode

-- | Run a migration. The returned exit code denotes the success or failure of
-- the migration.
runMigration :: MigrationDetails -> IO ExitCode
runMigration (MigrationDetails file version _) = do
  rawSystem "runghc"
    [file, "up", version, "--with-db-commit"]

runRollbackForDir :: FilePath -> IO ExitCode
runRollbackForDir dir = do
  conn <- connectEnv
  res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
  case res of
    [] -> do
      putStrLn "=== DB Fully Rolled Back!"
      return ExitSuccess
    (Only latest):_ -> do
      (Just (mig@(MigrationDetails _ _ name))) <-
                  getDirectoryMigrations dir >>=
                    return . (find (isVersion (== latest)))
      putStrLn $ "=== Running Rollback " ++ name
      exitCode <- runRollback mig
      if exitCode == ExitSuccess then do
        putStrLn "=== Success"
        withFile (dir </> ".." </> "schema.sql") WriteMode dumpDb
        else do
          putStrLn "=== Migration Failed!"
          return exitCode

-- | Run a migration. The returned exit code denotes the success or failure of
-- the migration.
runRollback :: MigrationDetails -> IO ExitCode
runRollback (MigrationDetails file version _) = do
  rawSystem "runghc"
    [file, "down", version, "--with-db-commit"]

data MigrationDetails = MigrationDetails { migrationPath :: FilePath
                                         , migrationVersion :: String
                                         , migrationName :: String }
                                         deriving (Show)

getDirectoryMigrations :: FilePath -> IO [MigrationDetails]
getDirectoryMigrations dir = do
  files0 <- getDirectoryContents dir
  let files = filter (('.' /=) . head) $ sort files0
  return $ map (splitFileVersionName dir) files

splitFileVersionName :: FilePath -> FilePath -> MigrationDetails
splitFileVersionName dir file = 
  let fileName = takeBaseName file
      parts    = foldr (\chr (hd:result) ->
                          if chr == '_' then
                            "":hd:result
                            else ((chr:hd):result))
                       [""] fileName
      version  = head parts
      name     = concat $ intersperse "_" $ tail parts
  in MigrationDetails (dir </> file) version name

isVersion :: (String -> Bool) -> MigrationDetails -> Bool
isVersion cond (MigrationDetails _ v _) = cond v

