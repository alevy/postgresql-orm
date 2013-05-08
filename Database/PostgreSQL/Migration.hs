{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Migration
  ( module Database.PostgreSQL.Simple
  , defaultMigrationDir
  , Connection(..)
  , initializeSchemaMigrations
  , connect
  ) where

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.List
import Data.Maybe
import Data.Int
import Database.PostgreSQL.Simple hiding (execute, execute_, query, query_, Connection, connect)
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Internal as PS
import qualified Database.PostgreSQL.Simple.Types as PS
import qualified Database.PostgreSQL.LibPQ as PQ
import GHC.IO.Handle
import GHC.IO.Handle.FD
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Posix.String ()
import Text.Regex.Posix.Wrap ((=~))
import Language.Haskell.Interpreter

connect :: IO Connection
connect = do
  psqlStr <- getEnvironment >>= return . (fromMaybe "") . (lookup "DATABASE_URL")
  fmap Connection $ connectPostgreSQL $ S8.pack psqlStr

query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ (Connection conn) sql@(PS.Query rawSql) = do
  S8.putStrLn rawSql
  PS.query_ conn sql

query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
query (Connection conn) sql fields = do
  rawSql <- formatQuery conn sql fields
  S8.putStrLn rawSql
  PS.query_ conn (PS.Query rawSql)

execute_ :: Connection -> Query -> IO Int64
execute_ (Connection conn) sql@(PS.Query rawSql) = do
  S8.putStrLn rawSql
  PS.execute_ conn sql

execute :: ToRow q => Connection -> Query -> q -> IO Int64
execute (Connection conn) sql fields = do
  rawSql <- formatQuery conn sql fields
  S8.putStrLn rawSql
  PS.execute_ conn (PS.Query rawSql)

exec :: Connection -> S8.ByteString -> IO PQ.Result
exec (Connection conn) sql = do
  S8.putStrLn sql
  PS.exec conn sql

type Migration = Connection -> IO ()

newtype Connection = Connection PS.Connection

runSqlFile :: Connection -> FilePath -> IO ()
runSqlFile conn sqlFile = void $ do
    rawSql <- S.readFile sqlFile
    exec conn rawSql

initializeSchemaMigrations :: PS.Connection -> IO ()
initializeSchemaMigrations conn = void $ PS.execute_ conn
    "create table schema_migrations (version VARCHAR(14) )"

rollbackFiles :: Connection -> [FilePath] -> IO ()
rollbackFiles conn files = do
  res <- query_ conn
          "select version from schema_migrations order by version desc"
  let versions = map (\(Only v) -> v) res
  go (sort files) versions
  where go [] _ = return ()
        go _ [] = return ()
        go (file:files) (latestVersion:versions) = do
          let version = takeWhile (/= '_') file
          let name = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '_') file
          case () of
            () | version == latestVersion -> do
                        putStrLn $ "=== Rolling back " ++ name
                        undefined version file
                        go files versions
               | version < latestVersion -> fail $
                                "Missing rollback " ++ latestVersion
               | otherwise -> return ()

migrateFiles :: Connection -> [FilePath] -> IO ()
migrateFiles conn files = do
  res <- query_ conn
          "select version from schema_migrations order by version desc limit 1"
  let latestVersion = case res of
                        [] -> ""
                        (Only latest):_ -> latest
  go (sort files) latestVersion
  where go [] latestVersion = return ()
        go (file:files) latestVersion = do
          let version = takeWhile (/= '_') file
          let name = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '_') file
          if version > latestVersion then do
            putStrLn $ "=== Running Migration " ++ name
            undefined version file
            putStrLn $ "=== Finished " ++ name
            go files version
            else
              return ()

defaultMigrationDir :: FilePath
defaultMigrationDir = "db" </> "migrations"

runAllMigrations :: FilePath -> IO ()
runAllMigrations dir = do
  dirs <- getDirectoryContents dir
  print $ filter (isMigrationDir) dirs
  return ()
  where isMigrationDir d = length ((d =~ pattern) :: String)  > 0
        pattern :: String
        pattern = "[0-9]+_[^\\.]+"

