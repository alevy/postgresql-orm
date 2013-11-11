{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.PostgreSQL.LIO.Connection
  ( Connection
  , query, queryP, query_, queryP_
  , execute, executeP, execute_, executeP_
  , module Database.PostgreSQL.Simple) where

import Data.Int
import qualified Data.ByteString.Char8 as S8
import Database.PostgreSQL.Simple hiding
  (query, query_, execute, execute_, Connection, ConnectInfo)
import qualified Database.PostgreSQL.Simple as P
import LIO
import LIO.DCLabel
import LIO.TCB
import LIO.TCB.LObj

type Connection = LObj DCLabel P.Connection

type ConnectInfo = Principal

toConnectInfo :: ConnectInfo -> DC P.ConnectInfo
toConnectInfo ci = return $ P.ConnectInfo
  { P.connectHost = "127.0.0.1"
  , P.connectPort = 5432
  , P.connectUser = ""
  , P.connectPassword = ""
  , P.connectDatabase = S8.unpack $ principalName ci }

connectP :: DCPriv -> ConnectInfo -> DC Connection
connectP p ci = do
  guardAllocP p $ (ci %% ci)
  pci <- toConnectInfo ci
  conn <- ioTCB $ P.connect pci
  return $ LObjTCB (ci %% ci) conn

query :: (FromRow r, ToRow q) => Connection -> Query -> q -> DC [r]
query = blessTCB "query" P.query

queryP :: (FromRow r, ToRow q) => DCPriv -> Connection -> Query -> q -> DC [r]
queryP = blessPTCB "query" P.query

query_ :: FromRow r => Connection -> Query -> DC [r]
query_ = blessTCB "query_" P.query_

queryP_ :: FromRow r => DCPriv -> Connection -> Query -> DC [r]
queryP_ = blessPTCB "query_" P.query_

execute :: ToRow q => Connection -> Query -> q -> DC Int64
execute = blessTCB "execute" P.execute

executeP :: ToRow q => DCPriv -> Connection -> Query -> q -> DC Int64
executeP = blessPTCB "execute" P.execute

execute_ :: Connection -> Query -> DC Int64
execute_ = blessTCB "execute_" P.execute_

executeP_ :: DCPriv -> Connection -> Query -> DC Int64
executeP_ = blessPTCB "execute_" P.execute_

