{-# LANGUAGE FlexibleContexts #-}
module Database.PostgreSQL.Connection where

import Data.Pool
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as S8
import Database.PostgreSQL.Simple
import System.Environment

createConnectionPool :: Maybe S8.ByteString -> IO (Pool Connection)
createConnectionPool mconnectStr = do
  createPool (createConnection mconnectStr)
             (\c -> rollback c >> close c)
             1
             (fromInteger 60)
             20

createConnection :: Maybe S8.ByteString -> IO Connection
createConnection mconnectStr = do
  dbUrl <- case mconnectStr of
    Nothing -> do
      env <- getEnvironment
      return $ maybe S8.empty S8.pack $ lookup "DATABASE_URL" env
    Just str -> return str
  connectPostgreSQL dbUrl

withConnection :: (MonadIO m, MonadBaseControl IO m)
               => Pool Connection
               -> (Connection -> m b) -> m b
withConnection pool func = withResource pool $ \conn -> do
  liftIO $ begin conn
  res <- func conn
  liftIO $ commit conn
  return res

