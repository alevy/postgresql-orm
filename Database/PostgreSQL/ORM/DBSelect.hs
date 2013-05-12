{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.ORM.DBSelect (
    Clause(..), emptyClause, mkClause, appendClause
  , DBSelect(..), emptyDBSelect, renderDBSelect
  , dbSelect
  ) where

import qualified Data.ByteString as S
import Data.Monoid
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Generics

data Clause = Clause { clQuery :: !S.ByteString
                     , clParam :: ![Action]
                     } deriving (Show)

emptyClause :: Clause
emptyClause = Clause S.empty []

nullClause :: Clause -> Bool
nullClause (Clause q []) = S.null q
nullClause _             = False

mkClause :: (ToRow p) => Query -> p -> Clause
mkClause (Query q) p = Clause q (toRow p)

appendClause :: Query -> Clause -> Clause -> Clause
appendClause _ a b | nullClause a = b
                   | nullClause b = a
appendClause (Query delim) (Clause qa pa) (Clause qb pb) =
  Clause (S.concat [qa, delim, qb]) (pa ++ pb)

data DBSelect a = DBSelect {
    selWith :: !Clause
  , selSelect :: !Query
    -- ^ Normally \"SELECT\", but could also be, e.g., \"SELECT DISTINCT\"
  , selFields :: !Query
  , selFrom :: !Clause
  , selWhere :: !Clause
  , selGroupBy :: !Query
  , selHaving :: !Clause
    -- below here, should appear outside any union
  , selOrderBy :: !Query
  , selLimit :: !Clause
  , selOffset :: !Clause
  } deriving (Show, Generic)

class GDBS f where
  gdbsDefault :: f p
  gdbsQuery :: f p -> Endo [S.ByteString]
  gdbsParam :: f p -> Endo [Action]
instance GDBS (K1 i Query) where
  gdbsDefault = K1 (Query S.empty)
  gdbsQuery (K1 (Query bs)) = Endo (bs :)
  gdbsParam (K1 (Query bs)) = Endo id
instance GDBS (K1 i Clause) where
  gdbsDefault = K1 emptyClause
  gdbsQuery (K1 cl) = Endo ((clQuery cl) :)
  gdbsParam (K1 cl) = Endo ((clParam cl) ++)
instance (GDBS a, GDBS b) => GDBS (a :*: b) where
  gdbsDefault = gdbsDefault :*: gdbsDefault
  gdbsQuery (a :*: b) = gdbsQuery a <> gdbsQuery b
  gdbsParam (a :*: b) = gdbsParam a <> gdbsParam b
instance (GDBS f) => GDBS (M1 i c f) where
  gdbsDefault = M1 gdbsDefault
  gdbsQuery = gdbsQuery . unM1
  gdbsParam = gdbsParam . unM1

emptyDBSelect :: (DBSelect a)
emptyDBSelect = (to gdbsDefault) { selSelect = fromString "SELECT" }

renderDBSelect :: (DBSelect a) -> Query
renderDBSelect dbs = Query $ S.intercalate " " $ filter (not . S.null) $
                     appEndo (gdbsQuery $ from dbs) []

instance ToRow (DBSelect a) where
  toRow dbs = appEndo (gdbsParam $ from dbs) []

dbSelect :: (FromRow a) => Connection -> DBSelect a -> IO [a]
dbSelect c dbs = query c (renderDBSelect dbs) dbs
