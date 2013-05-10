{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.ORM.DSL where

import Data.AsTypeOf
import qualified Data.ByteString as S
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.ORM.Model

findBy :: (Model r) => Connection -> Clause -> IO [r]
findBy conn clause = action
  where sel = do conds <- runClause conn clause
                 return $ Query $ modelSelectFragment
                            (modelIdentifiers `gAsTypeOf1_1` action)
                            <> " " <> conds
        action = do rs <- sel >>= query_ conn
                    return $ map lookupRow rs

newtype Clause = Clause (Connection -> IO S.ByteString)

runClause :: Connection -> Clause -> IO S.ByteString
runClause conn (Clause f) = f conn

rawClause :: ToRow r => Query -> r -> Clause
rawClause q row = Clause $ \conn -> formatQuery conn q row

rawClause_ :: Query -> Clause
rawClause_ (Query q) = Clause $ \_ -> return q

instance Monoid Clause where
  mempty = Clause . const $ return mempty
  mappend (Clause f1) (Clause f2) = Clause $ \conn -> do
    q1 <- f1 conn
    q2 <- f2 conn
    return $ q1 <> " " <> q2

whereClause :: ToRow r => Query -> r -> Clause
whereClause q row = Clause $ \conn ->
  formatQuery conn ("where " <> q) row

limit :: Integer -> Clause
limit i = Clause $ \conn -> formatQuery conn "limit ?" (Only i)

offset :: Integer -> Clause
offset i = Clause $ \conn -> formatQuery conn "offset ?" (Only i)

