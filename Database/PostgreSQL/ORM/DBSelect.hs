{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.ORM.DBSelect (
    Clause(..), emptyClause, mkClause, appendClause
  , DBSelect(..), emptyDBSelect, renderDBSelect
  , setWhere, addWhere
  , setOrderBy, setLimit, setOffset
  , dbSelect
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
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
    -- ^ By default @\"SELECT\"@, but might usefully be set to
    -- something else such as @\"SELECT DISTINCT\"@ in some
    -- situations.
  , selFields :: !Query
  , selFromKeyword :: !Query
    -- ^ By default @\"FROM\"@, but could set it to empty for
    -- selecting simple values (such as internal database functions).
  , selFrom :: !Query
  , selJoins :: ![Query]
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
  gdbsQuery :: f p -> Builder
  gdbsParam :: f p -> Endo [Action]
instance GDBS (K1 i Query) where
  gdbsDefault = K1 (Query S.empty)
  gdbsQuery (K1 (Query bs)) | S.null bs = mempty
                            | otherwise = fromByteString bs <> fromChar ' '
  gdbsParam _ = mempty
instance GDBS (K1 i [Query]) where
  gdbsDefault = K1 []
  gdbsQuery (K1 qs) = mconcat $ map doq qs
    where doq q = fromByteString (fromQuery q) <> fromChar ' '
  gdbsParam _ = mempty
instance GDBS (K1 i Clause) where
  gdbsDefault = K1 emptyClause
  gdbsQuery (K1 cl) | S.null (clQuery cl) = mempty
                    | otherwise = fromByteString (clQuery cl) <> fromChar ' '
  gdbsParam (K1 cl) = Endo ((clParam cl) ++)
instance (GDBS a, GDBS b) => GDBS (a :*: b) where
  gdbsDefault = gdbsDefault :*: gdbsDefault
  gdbsQuery (a :*: b) = gdbsQuery a <> gdbsQuery b
  gdbsParam (a :*: b) = gdbsParam a <> gdbsParam b
instance (GDBS f) => GDBS (M1 i c f) where
  gdbsDefault = M1 gdbsDefault
  gdbsQuery = gdbsQuery . unM1
  gdbsParam = gdbsParam . unM1

emptyDBSelect :: DBSelect a
emptyDBSelect = (to gdbsDefault) { selSelect = fromString "SELECT"
                                 , selFromKeyword = fromString "FROM" }

renderDBSelect :: DBSelect a -> Query
renderDBSelect dbs = Query $ toByteString $ gdbsQuery $ from dbs

instance ToRow (DBSelect a) where
  toRow dbs = appEndo (gdbsParam $ from dbs) []

setWhere :: (ToRow p) => DBSelect a -> Query -> p -> DBSelect a
setWhere dbs q p = dbs { selWhere = mkClause q p }

addWhere :: (ToRow p) => DBSelect a -> Query -> p -> DBSelect a
addWhere dbs@DBSelect{ selWhere = wh } q@(Query q') p = dbs { selWhere = newwh }
  where newwh | nullClause wh = mkClause (Query $ "WHERE " <> q') p 
              | otherwise     = appendClause " AND " wh $ mkClause q p

setOrderBy :: DBSelect a -> Query -> DBSelect a
setOrderBy dbs ob = dbs { selOrderBy = "ORDER BY " <> ob }

setLimit :: DBSelect a -> Int -> DBSelect a
setLimit dbs i = dbs { selLimit = mkClause "LIMIT ?" (Only i) }

setOffset :: DBSelect a -> Int -> DBSelect a
setOffset dbs i = dbs { selOffset = mkClause "OFFSET ?" (Only i) }

newtype RenderedToRow = RenderedToRow [Action] deriving (Show)
instance ToRow RenderedToRow where toRow (RenderedToRow as) = as

dbSelect :: (FromRow a) => Connection -> DBSelect a -> IO [a]
dbSelect c dbs =
  case toRow dbs of [] -> query_ c (renderDBSelect dbs)
                    as -> query c (renderDBSelect dbs) (RenderedToRow as)
