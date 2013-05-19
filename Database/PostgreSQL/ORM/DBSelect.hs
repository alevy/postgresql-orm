{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.ORM.DBSelect (
    -- * The DBSelect structure
    Join(..), DBSelect(..)
    -- * Executing DBSelects
  , dbSelectParams, dbSelect
  , renderDBSelect, buildDBSelect
    -- * Creating DBSelects
  , emptyDBSelect
  , modelDBSelect
  , dbJoin, dbJoinModels
  , dbProject
    -- * Altering DBSelects
  , addWhere, setOrderBy, setLimit, setOffset
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import qualified Data.ByteString as S
import Data.Functor
import Data.Monoid
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Data.AsTypeOf
import Database.PostgreSQL.Escape
import Database.PostgreSQL.ORM.Model

data Join = Join {
    joinKeyword :: !Query
    -- ^ @\"JOIN\"@, @\"CROSS JOIN\"@, etc.
  , joinRHS :: !Query
    -- ^ Right-hand side of the join relation (i.e., @table2@ in
    -- \"@SELECT table1 JOIN table2 ON ...@\")
  , joinOn :: !Query
    -- ^ @\"ON ...\"@ or @\"USING ...\"@ clause, including the @ON@ or
    -- @USING@ keyword..
  } deriving (Show)

-- | A deconstructed SQL select statement.
data DBSelect a = DBSelect {
    selWith :: !Query
  , selSelectKeyword :: !Query
    -- ^ By default @\"SELECT\"@, but might usefully be set to
    -- something else such as @\"SELECT DISTINCT\"@ in some
    -- situations.
  , selFields :: !Query
  , selFromKeyword :: !Query
    -- ^ By default @\"FROM\"@, but could be set to empty for
    -- selecting simple values (such as internal database functions).
  , selFrom :: !Query
  , selJoins :: ![Join]
  , selWhereKeyword :: !Query
    -- ^ Empty by default, but set to @\"WHERE\"@ if any @WHERE@
    -- clauses are added to the 'selWhere' field.
  , selWhere :: !Query
  , selGroupBy :: !Query
  , selHaving :: !Query
    -- below here, should appear outside any union
  , selOrderBy :: !Query
  , selLimit :: !Query
  , selOffset :: !Query
  } deriving (Show, Generic)

space :: Builder
space = fromChar ' '

qNull :: Query -> Bool
qNull = S.null . fromQuery

qBuilder :: Query -> Builder
qBuilder = fromByteString . fromQuery

toQuery :: Builder -> Query
toQuery = Query . toByteString

class GDBS f where
  gdbsDefault :: f p
  gdbsQuery :: f p -> Builder
instance GDBS (K1 i Query) where
  gdbsDefault = K1 (Query S.empty)
  gdbsQuery (K1 q) | qNull q = mempty
                   | otherwise = space <> qBuilder q
instance GDBS (K1 i [Join]) where
  gdbsDefault = K1 []
  gdbsQuery (K1 js) = mconcat $ map doj js
    where doj (Join kw tb on) =
            space <> qBuilder kw <> space <> qBuilder tb <> space <> qBuilder on
instance (GDBS a, GDBS b) => GDBS (a :*: b) where
  gdbsDefault = gdbsDefault :*: gdbsDefault
  gdbsQuery (a :*: b) = gdbsQuery a <> gdbsQuery b
instance (GDBS f) => GDBS (M1 i c f) where
  gdbsDefault = M1 gdbsDefault
  gdbsQuery = gdbsQuery . unM1

-- | A 'DBSelect' structure with keywords @\"SELECT\"@ and @\"FROM\"@
-- and everything else empty.
emptyDBSelect :: DBSelect a
emptyDBSelect = (to gdbsDefault) { selSelectKeyword = fromString "SELECT"
                                 , selFromKeyword = fromString "FROM" }

buildDBSelect :: DBSelect a -> Builder
buildDBSelect dbs = gdbsQuery $ from dbs

renderDBSelect :: DBSelect a -> Query
renderDBSelect = Query . toByteString . buildDBSelect

addWhere :: (ToRow p) => Query -> p -> DBSelect a -> DBSelect a
addWhere q p dbs
  | qNull q = dbs
  | otherwise = dbs {
    selWhereKeyword = "WHERE"
  , selWhere = if qNull $ selWhere dbs
               then toQuery clause
               else toQuery $ qBuilder (selWhere dbs) <>
                    fromByteString " AND " <> clause
  }
  where clause = mconcat [fromChar '(', buildSql q p, fromChar ')']

setOrderBy :: Query -> DBSelect a -> DBSelect a
setOrderBy (Query ob) dbs = dbs { selOrderBy = Query $ "ORDER BY " <> ob }

setLimit :: Int -> DBSelect a -> DBSelect a
setLimit i dbs = dbs { selLimit = fmtSql "LIMIT ?" (Only i) }

setOffset :: DBSelect a -> Int -> DBSelect a
setOffset dbs i = dbs { selOffset = fmtSql "OFFSET ?" (Only i) }

-- | A 'DBSelect' that returns all rows of a model.
modelDBSelect :: (Model a) => DBSelect a
modelDBSelect = r
  where mi = modelIdentifiers `gAsTypeOf1` r
        r = emptyDBSelect {
          selFields = Query $ S.intercalate ", " $ modelQColumns mi
          , selFrom = Query $ modelQTable mi
          }

-- | Run a 'DBSelect' query on parameters.  There number of \'?\'
-- character embedeed in various fields of the 'DBSelect' must exactly
-- match the number of fields in parameter type @p@.  Note the order
-- of arguments is such that the 'DBSelect' can be pre-rendered and
-- the parameter supplied later.  Hence, you should use this version
-- when the 'DBSelect' is static.  For dynamically modified 'DBSelect'
-- structures, you may prefer 'dbSelect'.
dbSelectParams :: (Model a, ToRow p) => DBSelect a -> Connection -> p -> IO [a]
{-# INLINE dbSelectParams #-}
dbSelectParams dbs = \c p -> map lookupRow <$> query c q p
  where q = renderDBSelect dbs

-- | Run a 'DBSelect' query and return the resulting models.
dbSelect :: (Model a) => Connection -> DBSelect a -> IO [a]
{-# INLINE dbSelect #-}
dbSelect c dbs = map lookupRow <$> query_ c q
  where q = renderDBSelect dbs

-- | Create a join of the 'selFields', 'selFrom', 'selJoins', and
-- 'selWhere' clauses of two 'DBSelect' queries.  Other fields are
-- simply taken from the first 'DBSelect', meaning the values in the
-- second table are ignored.
dbJoin :: DBSelect a      -- ^ First table
          -> Query        -- ^ Join keyword (@\"JOIN\"@, @\"LEFT JOIN\"@, etc.)
          -> DBSelect b   -- ^ Second table
          -> Query  -- ^ Predicate (if any) including @ON@ or @USING@ keyword
          -> DBSelect (a :. b)
dbJoin left joinOp right onClause = left {
    selFields = Query $ S.concat [fromQuery $ selFields left, ", ",
                                  fromQuery $ selFields right]
  , selJoins = selJoins left ++
               Join joinOp (selFrom right) onClause :
               selJoins right
  , selWhereKeyword = Query $ if S.null whereClause then S.empty else "WHERE"
  , selWhere = Query whereClause
  }
  where wl = fromQuery $ selWhere left
        wr = fromQuery $ selWhere right
        whereClause | S.null wl = wr
                    | S.null wr = wl
                    | otherwise = S.concat [wl, " AND ", wr]

-- | A version of 'dbJoin' that uses 'modelDBSelect' for the joined
-- tables.
dbJoinModels :: (Model a, Model b) =>
                Query           -- ^ Join keyword
                -> Query        -- ^ @ON@ or @USING@ predicate
                -> DBSelect (a :. b)
dbJoinModels kw on = dbJoin modelDBSelect kw modelDBSelect on

-- | Restrict the fields returned by a DBSelect to be those of a
-- single 'Model' @a@.  It only makes sense to do this if all the
-- fields of @a@ are part of @join@, but no static check is performed.
-- If you @dbProject@ a type that doesn't make sense, you will get a
-- runtime error from a failed database query.
dbProject :: (Model a) => DBSelect join -> DBSelect a
dbProject dbs = r
  where sela = modelDBSelect `gAsTypeOf1` r
        r = dbs { selFields = selFields sela }
