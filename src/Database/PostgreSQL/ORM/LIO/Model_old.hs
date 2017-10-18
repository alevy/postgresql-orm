{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Database.PostgreSQL.ORM.LIO.Model where

import qualified Data.ByteString as S
import qualified Database.PostgreSQL.Simple as M
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.ORM.Model (Model, GDBRef)
import qualified Database.PostgreSQL.ORM.Model as M
import qualified Database.PostgreSQL.ORM.DBSelect as M
import qualified Database.PostgreSQL.ORM as M
import Data.Typeable
import LIO
import LIO.DCLabel
import LIO.TCB

import GHC.Generics
import Database.PostgreSQL.ORM.CreateTable
import Data.Vector (Vector, toList)

findAllP :: (Model r, ModelPolicy c r m) => Connection c -> DC [DCLabeled m]
findAllP (ConnectionTCB c dcc) = do
  rows <- ioTCB $ M.dbSelect c selectModel
  mapM (labelModel dcc) rows

findRow :: (Model r, ModelPolicy c r m)
     => Connection c -> GDBRef rt r -> DC (Maybe (DCLabeled m))
findRow (ConnectionTCB c dcc) k = do
  mrow <- ioTCB $ M.findRow c k
  case mrow of
    Nothing -> return Nothing
    Just row -> labelModel dcc row >>= \lr -> return $ Just lr

data Connection c = ConnectionTCB M.Connection c

class DCConnection c => ModelPolicy c a b | a -> b, b -> c, b -> a where
  labelModel :: c -> a -> DC (DCLabeled b)

  selectModel :: M.DBSelect a
  default selectModel :: (Model a) => M.DBSelect a
  selectModel = M.modelDBSelect

  lookupModel :: M.DBSelect a
  default lookupModel :: Model a => M.DBSelect a
  lookupModel =
    let primKey = M.modelQPrimaryColumn (M.modelIdentifiers :: M.ModelIdentifiers a)
    in M.addWhere_ (Query $ S.concat [primKey, " = ?"]) $ M.modelDBSelect

class Typeable c => DCConnection c where
  newConnection :: DCPriv -> c

connect :: forall c. DCConnection c => DC (Connection c)
connect = do
  let tc = typeRepTyCon $ typeOf (undefined :: c)
      pd = concat
            [ tyConPackage tc
            , ":"
            , tyConModule tc
            , "."
            , tyConName tc ]
      cpriv = PrivTCB $ toCNF $ principal pd
  conn <- ioTCB $ M.connectPostgreSQL "" -- M.defaultConnectInfo --{ M.connectDatabase = pd }
  return $ ConnectionTCB conn $ newConnection cpriv

--- EXAMPLE

data MyConn = MyConnTCB DCPriv deriving (Typeable)

instance DCConnection MyConn where
  newConnection = MyConnTCB

data Owner = Owner { ownerId :: M.DBKey
                   , ownerPrincipal :: String } deriving (Generic, Show)

data Region = Region { regionId :: M.DBKey
                     , regionName :: String
                     , regionOwner :: M.DBRef Owner } deriving (Generic, Show)

instance Model Region where
  modelInfo = M.underscoreModelInfo "region"

instance Model Owner where
  modelInfo = M.underscoreModelInfo "region"

instance ModelPolicy MyConn (Region M.:. Owner) Region where
  selectModel = M.addExpression "" $ M.modelDBSelect
  labelModel (MyConnTCB mypriv) (region M.:. owner) = do
    labelP mypriv (ownerPrincipal owner \/ mypriv %% ownerPrincipal owner \/ mypriv) region

instance ModelPolicy MyConn Owner Owner where
  labelModel (MyConnTCB mypriv) owner =
    labelP mypriv (True %% mypriv) owner

data Owners = Owners { ownersId :: M.DBKey, owners :: Vector String } deriving (Generic, Typeable)

instance Model Owners where

