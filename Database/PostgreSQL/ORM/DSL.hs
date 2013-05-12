{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.ORM.DSL where

import Data.AsTypeOf
import qualified Data.ByteString as S
import Data.Maybe
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.ORM.Model

data DBQuery = DBQuery { qWhere   :: Maybe Clause
                       , qLimit   :: Maybe Clause
                       , qOffset  :: Maybe Clause
                       , qOrderBy :: Maybe Clause
                       }

instance Monoid DBQuery where
  mempty = DBQuery Nothing Nothing Nothing Nothing
  mappend db1 db2 = DBQuery (whereConcat (qWhere db1) (qWhere db2))
                            (oneOf qLimit)
                            (oneOf qOffset)
                            (oneOf qOrderBy)
    where oneOf comb = case comb db2 of
                        Nothing -> comb db1
                        _ -> comb db2
          whereConcat (Just first) (Just second) = Just $ first
                                                        <> (rawClause_ "and")
                                                        <> second
          whereConcat o1 o2 = mappend o1 o2

runDBQuery :: DBQuery -> Clause
runDBQuery dbQuery =
  let wClause = case qWhere dbQuery of
                  Nothing -> Nothing
                  Just q -> Just $ (rawClause_ "where") <> q
      mres    = wClause <> (mconcat $ map (\f -> f dbQuery)
                                         [qOrderBy, qLimit, qOffset])
  in fromMaybe mempty mres

findBy :: (Model r) => Connection -> DBQuery -> IO [r]
findBy conn dbQuery = action
  where sel = do conds <- runClause conn $ runDBQuery dbQuery
                 return $ Query $ modelSelectFragment
                            (modelIdentifiers `gAsTypeOf1_1` action)
                            <> " " <> conds
        action = do sel >>= print
                    rs <- sel >>= query_ conn
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

whereClause :: ToRow r => Query -> r -> DBQuery
whereClause q row = mempty { qWhere = Just $ rawClause q row }

limit :: Integer -> DBQuery
limit i = mempty { qLimit = Just $ rawClause "limit ?" (Only i) }

offset :: Integer -> DBQuery
offset i = mempty { qOffset = Just $ rawClause "offset ?" (Only i) }

orderBy :: Query -> DBQuery
orderBy ordering = mempty { qOrderBy = Just $ rawClause_ $
                                                "order by " <> ordering }

