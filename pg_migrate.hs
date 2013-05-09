{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.List
import Database.PostgreSQL.Simple hiding (connect)
import qualified Data.ByteString.Char8 as S8
import Database.PostgreSQL.Migration
import System.Exit 
import GHC.IO.Handle
import System.Cmd
import System.Process
import System.Directory
import System.FilePath
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  ec <- case args of
    "init":[] -> initializeDb
    "dump":[] -> dumpDb stdout
    "migrate":[] -> runMigrationsForDir defaultMigrationDir
    "migrate":dir:[] -> runMigrationsForDir dir
    "rollback":[] -> runRollbackForDir defaultMigrationDir
    "rollback":dir:[] -> runRollbackForDir dir
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " migrate|rollback [DIRECTORY]"
      putStrLn $ "       " ++ progName ++ " init"
      putStrLn $ "       " ++ progName ++ " dump"
      return $ ExitFailure 1
  if ec == ExitSuccess then
    return ()
    else exitWith ec


dumpDb :: Handle -> IO ExitCode
dumpDb outputFile = do
  (_, out, err, ph) <- runInteractiveProcess "pg_dump"
                        ["--schema-only"] Nothing Nothing
  exitCode <- waitForProcess ph
  if exitCode /= ExitSuccess then do
    S8.hGetContents err >>= S8.hPut outputFile
    else do
      S8.hGetContents out >>= S8.hPut stderr
  return exitCode

initializeDb :: IO ExitCode
initializeDb = do
  conn <- connectEnv
  void $ execute_ conn "create table schema_migrations (version VARCHAR(14))"
  return ExitSuccess


runMigrationsForDir :: FilePath -> IO ExitCode
runMigrationsForDir dir = do
  conn <- connectEnv
  res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
  let latestVersion = case res of
                        [] -> ""
                        (Only latest):_ -> latest
  migrations <- getDirectoryMigrations dir >>=
                    return . (dropWhile (isVersion (<= latestVersion)))
  go migrations
  where go [] = return ExitSuccess
        go (MigrationDetails file version name:fs) = do
              putStrLn $ "=== Running Migration " ++ name
              exitCode <- rawSystem "runghc"
                            [ "-XOverloadedStrings", file, "up", version
                            , "--with-db-commit"]
              if exitCode == ExitSuccess then do
                putStrLn "=== Success"
                go fs
                else do
                  putStrLn "=== Migration Failed!"
                  return exitCode

runRollbackForDir :: FilePath -> IO ExitCode
runRollbackForDir dir = do
  conn <- connectEnv
  res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
  let latestVersion = case res of
                        [] -> ""
                        (Only latest):_ -> latest
  (Just (MigrationDetails file version name)) <-
              getDirectoryMigrations dir >>=
                return . (find (isVersion (== latestVersion)))
  putStrLn $ "=== Running Rollback " ++ name
  exitCode <- rawSystem "runghc"
                [ "-XOverloadedStrings", file, "down", version
                , "--with-db-commit"]
  if exitCode == ExitSuccess then do
    putStrLn "=== Success"
    return $ ExitSuccess
    else do
      putStrLn "=== Migration Failed!"
      return exitCode

data MigrationDetails = MigrationDetails FilePath String String

getDirectoryMigrations :: FilePath -> IO [MigrationDetails]
getDirectoryMigrations dir = do
  files0 <- getDirectoryContents dir
  let files = filter (('.' /=) . head) files0
  let pairs = zip (map (dir </>) files) $ map splitFileVersionName files
  return $ map fromPairs pairs
  where fromPairs (path, (version, name)) = MigrationDetails path version name

splitFileVersionName :: FilePath -> (String, String)
splitFileVersionName file = 
  let fileName = takeBaseName file
      version  = takeWhile (/= '_') fileName
      name     = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '_') fileName
  in (version, name)

isVersion :: (String -> Bool) -> MigrationDetails -> Bool
isVersion cond (MigrationDetails _ v _) = cond v

