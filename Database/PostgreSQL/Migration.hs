{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Migration where

import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import Database.PostgreSQL.Simple hiding (connect)
import Database.PostgreSQL.Simple.Internal (exec)
import Database.PostgreSQL.Simple.Types
import System.Environment
import System.Exit
import System.FilePath

connectEnv :: IO Connection
connectEnv = do
  psqlStr <- getEnvironment >>= return . (fromMaybe "") . (lookup "DATABASE_URL")
  connectPostgreSQL $ S8.pack psqlStr

type Migration = Connection -> IO ()

runSqlFile :: Connection -> FilePath -> IO ()
runSqlFile conn sqlFile = void $ do
    rawSql <- S.readFile sqlFile
    exec conn rawSql

defaultMigrationDir :: FilePath
defaultMigrationDir = "db" </> "migrations"

create_table :: S8.ByteString -> [S8.ByteString] -> Query
create_table tableName colDefs = Query $ S8.concat $
  [ "create table "
  , tableName
  , " ("] ++ (S8.intercalate ", " colDefs):([");"])

drop_table :: S8.ByteString -> Query
drop_table tableName = Query $ S8.concat
  [ "drop table ", tableName, ";"]

add_column :: S8.ByteString -> S8.ByteString -> Query
add_column tableName colDef = Query $ S8.concat
  [ "alter table " , tableName , " add " , colDef, ";"]

drop_column :: S8.ByteString -> S8.ByteString -> Query
drop_column tableName colName = Query $ S8.concat
  ["alter table ", tableName, " drop ", colName, ";"]

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

defaultMain :: Migration -> Migration -> IO ()
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

