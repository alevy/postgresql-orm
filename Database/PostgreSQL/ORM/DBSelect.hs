{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.ORM.DBSelect (
    Clause(..), Join(..)
  , DBSelect(..), emptyDBSelect, renderDBSelect
  , setWhere, addWhere
  , setOrderBy, setLimit, setOffset
  , dbSelect
    -- * Internal
  , emptyClause, mkClause, appendClause
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

mkClause :: (ToRow p) => S.ByteString -> p -> Clause
mkClause q p = Clause q (toRow p)

appendClause :: S.ByteString -> Clause -> Clause -> Clause
appendClause _ a b | nullClause a = b
                   | nullClause b = a
appendClause delim (Clause qa pa) (Clause qb pb) =
  Clause (S.concat [qa, delim, qb]) (pa ++ pb)

data Join = Join {
    joinKeyword :: !S.ByteString
    -- ^ @\"JOIN\"@, @\"CROSS JOIN\"@, etc.
  , joinRHS :: !S.ByteString
    -- ^ Right-hand side of the join relation (i.e., @table2@ in
    -- \"@SELECT table1 JOIN table2 ON ...@\")
  , joinOn :: !S.ByteString
    -- ^ @\"ON ...\"@ or @\"USING ...\"@ clause.
  } deriving (Show)

data DBSelect a = DBSelect {
    selWith :: !Clause
  , selSelectKeyword :: !S.ByteString
    -- ^ By default @\"SELECT\"@, but might usefully be set to
    -- something else such as @\"SELECT DISTINCT\"@ in some
    -- situations.
  , selFields :: !S.ByteString
  , selFromKeyword :: !S.ByteString
    -- ^ By default @\"FROM\"@, but could set it to empty for
    -- selecting simple values (such as internal database functions).
  , selFrom :: !S.ByteString
  , selJoins :: ![Join]
  , selWhere :: !Clause
  , selGroupBy :: !S.ByteString
  , selHaving :: !Clause
    -- below here, should appear outside any union
  , selOrderBy :: !S.ByteString
  , selLimit :: !Clause
  , selOffset :: !Clause
  } deriving (Show, Generic)

space :: Builder
space = fromChar ' '

class GDBS f where
  gdbsDefault :: f p
  gdbsQuery :: f p -> Builder
  gdbsParam :: f p -> Endo [Action]
instance GDBS (K1 i S.ByteString) where
  gdbsDefault = K1 S.empty
  gdbsQuery (K1 bs) | S.null bs = mempty
                    | otherwise = space <> fromByteString bs
  gdbsParam _ = mempty
instance GDBS (K1 i [Join]) where
  gdbsDefault = K1 []
  gdbsQuery (K1 js) = mconcat $ map doj js
    where doj (Join kw tb on) = space <> fromByteString kw <> space <>
                                fromByteString tb <> space <> fromByteString on
  gdbsParam _ = mempty
instance GDBS (K1 i Clause) where
  gdbsDefault = K1 emptyClause
  gdbsQuery (K1 cl) | S.null (clQuery cl) = mempty
                    | otherwise = space <> fromByteString (clQuery cl)
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
emptyDBSelect = (to gdbsDefault) { selSelectKeyword = fromString "SELECT"
                                 , selFromKeyword = fromString "FROM" }

renderDBSelect :: DBSelect a -> Query
renderDBSelect dbs = Query $ if S.null q0 then q0 else S.tail q0
  where q0 = toByteString $ gdbsQuery $ from dbs

instance ToRow (DBSelect a) where
  toRow dbs = appEndo (gdbsParam $ from dbs) []

parenthesize :: S.ByteString -> S.ByteString
parenthesize bs =
  toByteString $ fromChar '(' <> fromByteString bs <> fromChar ')'

setWhere :: (ToRow p) => Query -> p -> DBSelect a -> DBSelect a
setWhere (Query q) p dbs = dbs {
    selWhere = mkClause ("WHERE " <> parenthesize q) p
  }

addWhere :: (ToRow p) => Query -> p -> DBSelect a -> DBSelect a
addWhere q@(Query bs) p dbs@DBSelect{ selWhere = wh } 
  | nullClause wh = setWhere q p dbs
  | otherwise = dbs {
        selWhere = appendClause " AND " wh $ mkClause (parenthesize bs) p
      }

setOrderBy :: Query -> DBSelect a -> DBSelect a
setOrderBy (Query ob) dbs = dbs { selOrderBy = "ORDER BY " <> ob }

setLimit :: Int -> DBSelect a -> DBSelect a
setLimit i dbs = dbs { selLimit = mkClause "LIMIT ?" (Only i) }

setOffset :: DBSelect a -> Int -> DBSelect a
setOffset dbs i = dbs { selOffset = mkClause "OFFSET ?" (Only i) }

dbSelect :: (FromRow a) => Connection -> DBSelect a -> IO [a]
dbSelect c dbs = query c (renderDBSelect dbs) dbs

