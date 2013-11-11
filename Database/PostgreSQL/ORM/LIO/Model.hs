{-# LANGUAGE Rank2Types #-}
module Database.PostgreSQL.ORM.LIO.Model where

import Control.Applicative
import Database.PostgreSQL.LIO.Connection
import Database.PostgreSQL.ORM.Model (Model, lookupRow, GDBRef)
import Database.PostgreSQL.ORM.DBSelect
import LIO
import LIO.DCLabel

dbSelectP :: Model a => DCPriv -> Connection -> DBSelect a -> DC [a]
{-# INLINE dbSelectP #-}
dbSelectP p conn dbs = map lookupRow <$> queryP_ p conn q
  where q = renderDBSelect dbs

dbSelectParamsP :: (ToRow r, Model a)
                => DCPriv -> Connection -> DBSelect a -> r -> DC [a]
{-# INLINE dbSelectParamsP #-}
dbSelectParamsP p conn dbs r = map lookupRow <$> queryP p conn q r
  where q = renderDBSelect dbs

data MPQueries = MPQueries
  { mpqFindAll :: Model a => Priv p -> Connection -> DC [a]
  , mpqFindRow :: Model a => Connection -> GDBRef rt a -> DC (Maybe a)
  , mpqSave :: Model a => Connection -> a -> DC a }
