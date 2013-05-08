{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Maybe
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Types (Query)
import qualified Data.ByteString.Char8 as S8
import Database.PostgreSQL.Migration
import GHC.IO.Exception
import System.Cmd
import System.Directory
import System.FilePath
import System.Environment

main = do
  args <- getArgs
  case args of
    "init":[] -> initializeDb
    "migrate":[] -> runMigrationsForDir defaultMigrationDir
    "migrate":dir:[] -> runMigrationsForDir dir
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " migrate|rollback [FILES]"

initializeDb :: IO ()
initializeDb = do
  (Connection conn) <- connect
  initializeSchemaMigrations conn

runMigrationsForDir :: FilePath -> IO ()
runMigrationsForDir dir = do
  (Connection conn) <- connect
  res <- PS.query_ conn
          "select version from schema_migrations order by version desc limit 1"
  let latestVersion = case res of
                        [] -> ""
                        (Only latest):_ -> latest
  fileNames <- getDirectoryContents dir
  let files = map (\fn -> dir </> fn) fileNames
  let filteredPairs = dropWhile (\(_, (version, _)) ->
                                    version <= latestVersion) $
                                zip files $ map versionName fileNames
  go filteredPairs
  where go [] = return ()
        go ((file, (version, name)):fs) = do
              putStrLn $ "=== Running Migration " ++ name
              exitCode <- rawSystem "runghc"
                            [file, "up", "--with-db-commit", "--force"]
              if exitCode == ExitSuccess then do
                putStrLn "=== Success"
                go fs
                else do
                  putStrLn "=== Migration Failed!"
        versionName fileName = 
              ( takeWhile (/= '_') fileName
              , takeWhile (/= '.') $ drop 1 $
                                              dropWhile (/= '_') fileName)

