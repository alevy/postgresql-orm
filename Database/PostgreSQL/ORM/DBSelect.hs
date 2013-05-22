{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.ORM.DBSelect (
    -- * The DBSelect structure
    DBSelect(..), FromClause(..)
    -- * Executing DBSelects
  , dbSelectParams, dbSelect
  , renderDBSelect, buildDBSelect
    -- * Creating DBSelects
  , emptyDBSelect
  , modelDBSelect
  , dbJoin, dbJoinModels
  , dbProject, dbProject'
  , dbNest, dbChain
    -- * Altering DBSelects
  , addWhere_, addWhere, setOrderBy, setLimit, setOffset
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Functor
import Data.Monoid
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Data.AsTypeOf
import Database.PostgreSQL.Escape
import Database.PostgreSQL.ORM.Model

-- | As it's name would suggest, a @FromClause@ is the part of a query
-- between the @FROM@ keyword and the @WHERE@ keyword.  It can
-- consists of simple table names, @JOIN@ operations, and
-- parenthesized subqueries.
--
-- From clauses are represented in a more structured way than the
-- other fields so as to allow the possibility of collapsing join
-- relations.  For instance, given a @'DBSelect' (A :. B)@ and a
-- @'DBSelect' (B :. C)@, it is desirable to be able to generate a
-- @'DBSelect' (A :. B :. C)@ in which each pair of terms involving
-- @B@ in the three-way relation is constrained according to the
-- original two queries.  This functionality is provided by 'dbNest'
-- and 'dbChain', but it requires the ability to locate and replace
-- the instance of type @B@ in one 'DBSelect' with the @FromClause@ of
-- the other 'DBSelect'.
--
-- The 'fcCanonical' field is a canonical name of each type, which by
-- convention is the quoted and fully-qualified table name.  Comparing
-- 'fcCanonical' is somewhat of a hack, and happens entirely at
-- runtime.  It would be nicer to do this at compile time, but doing
-- so would require language extensions such as @GADTs@ of
-- @FunctionalDependencies@.
data FromClause = FromModel {
    fcVerbatim :: !Query -- ^ Verbatim SQL for a table, table @AS@
                         -- alias, or parenthesized subquery.
  , fcCanonical :: !S.ByteString
    -- ^ Canonical name of the table or join relation represented by
    -- this term.  For @JOIN@ terms, this is always the @CROSS JOIN@
    -- of the canonical names of 'fcLeft' and 'fcRight'.  This means
    -- one can locate a join given only it's type (e.g., the canonical
    -- name for @A :. B@ is always @\"a CROSS JOIN b\"@), but it does
    -- mean you have to be careful not accidentally to merge two
    -- different joins on the same types.  For this reason it may be
    -- safest always to have type @b@ be a single table in 'dbNest'
    -- and 'dbChain'.
  }
  | FromJoin {
    fcLeft :: !FromClause
  , fcJoinOp :: !Query -- ^ Usually @\"JOIN\"@
  , fcRight :: !FromClause
  , fcOnClause :: !Query -- ^ @ON@ or @USING@ clause (or empty)
  , fcCanonical :: !S.ByteString
  }
  deriving Show

-- | A deconstructed SQL select statement that allows easier
-- manipulation of individual terms.  Several functions are provided
-- to combine the 'selFields', 'selFrom', and 'selWhere' clauses of
-- muliple @DBSelect@ structures.  Other clauses may be discarded when
-- combining queries with join operations.  Hence it is advisable to
-- set the other clauses at the end (or, if you set these fields, to
-- collapse your 'DBSelect' structure into a subquery using
-- `dbProject'`).
data DBSelect a = DBSelect {
    selWith :: !Query
  , selSelectKeyword :: !Query
    -- ^ By default @\"SELECT\"@, but might usefully be set to
    -- something else such as @\"SELECT DISTINCT\"@ in some
    -- situations.
  , selFields :: Query
  , selFrom :: !FromClause
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
  } deriving (Generic)

instance Show (DBSelect a) where
  show = S8.unpack . fromQuery . renderDBSelect

space :: Builder
space = fromChar ' '

qNull :: Query -> Bool
qNull = S.null . fromQuery

qBuilder :: Query -> Builder
qBuilder = fromByteString . fromQuery

toQuery :: Builder -> Query
toQuery = Query . toByteString

buildFromClause :: FromClause -> Builder
buildFromClause (FromModel q _) | qNull q = mempty
buildFromClause cl0 = fromByteString " FROM " <> go cl0
  where go (FromModel q _) = qBuilder q
        go (FromJoin left joinkw right onClause _) = mconcat [
            fromChar '(', go left, space, qBuilder joinkw, space, go right
          , if qNull onClause then mempty else space <> qBuilder onClause
          , fromChar ')' ]

class GDBS f where
  gdbsDefault :: f p
  gdbsQuery :: f p -> Builder
instance GDBS (K1 i Query) where
  gdbsDefault = K1 (Query S.empty)
  gdbsQuery (K1 q) | qNull q = mempty
                   | otherwise = space <> qBuilder q
instance GDBS (K1 i FromClause) where
  gdbsDefault = K1 (FromModel "" "")
  gdbsQuery (K1 fc) = buildFromClause fc
instance (GDBS a, GDBS b) => GDBS (a :*: b) where
  gdbsDefault = gdbsDefault :*: gdbsDefault
  gdbsQuery (a :*: b) = gdbsQuery a <> gdbsQuery b
instance (GDBS f) => GDBS (M1 i c f) where
  gdbsDefault = M1 gdbsDefault
  gdbsQuery = gdbsQuery . unM1

-- | A 'DBSelect' structure with keyword @\"SELECT\"@ and everything
-- else empty.
emptyDBSelect :: DBSelect a
emptyDBSelect = (to gdbsDefault) { selSelectKeyword = fromString "SELECT" }

-- | Create a 'Builder' for a rendered version of a 'DBSelect'.  This
-- can save one string copy if you want to embed one query inside
-- another as a subquery, as done by `dbProject'`, and thus need to
-- parenthesize it.  Hoever, the function is probably not a useful for
-- end users.
buildDBSelect :: DBSelect a -> Builder
buildDBSelect dbs = gdbsQuery $ from dbs

-- | Turn a 'DBSelect' into a 'Query' suitable for the 'query' or
-- 'query_' functions.
renderDBSelect :: DBSelect a -> Query
renderDBSelect = Query . S.tail . toByteString . buildDBSelect
-- S.tail is because the rendering inserts an extra space at the beginning

catQueries :: Query -> Query -> Query -> Query
catQueries left delim right
  | qNull left  = right
  | qNull right = left
  | otherwise   = Query $ S.concat $ map fromQuery [left, delim, right]

-- | Add a where clause verbatim to a 'DBSelect'.  The clause must
-- /not/ contain the @WHERE@ keyword (which is added automatically by
-- @addWhere_@ if needed).  If the @DBSelect@ has existing @WHERE@
-- clauses, the new clause is appended with @AND@.  If the query
-- contains any @\'?\'@ characters, they will be rendered into the
-- query and matching parameters will later have to be filled in via a
-- call to 'dbSelectParams'.
addWhere_ :: Query -> DBSelect a -> DBSelect a
addWhere_ q dbs
  | qNull q = dbs
  | otherwise = dbs { selWhereKeyword = "WHERE"
                    , selWhere = catQueries (selWhere dbs) " AND " q }

-- | Add a where clause, and pre-render parameters directly into the
-- clause.  The argument @p@ must have exactly as many fields as there
-- are @\'?\'@ characters in the 'Query'.  Example:
--
-- > bars <- dbSelect c $ addWhere "bar_id = ?" (Only target_id) $
-- >                      (modelDBSelect :: DBSelect Bar)
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

-- | Set the @ORDER BY@ clause of a 'DBSelect'.  Example:
--
-- > dbSelect c $ setOrderBy "\"employeeName\" DESC NULLS FIRST" $
--                  modelDBSelect
setOrderBy :: Query -> DBSelect a -> DBSelect a
setOrderBy (Query ob) dbs = dbs { selOrderBy = Query $ "ORDER BY " <> ob }

-- | Set the @LIMIT@ clause of a 'DBSelect'.
setLimit :: Int -> DBSelect a -> DBSelect a
setLimit i dbs = dbs { selLimit = fmtSql "LIMIT ?" (Only i) }

-- | Set the @OFFSET@ clause of a 'DBSelect'.
setOffset :: DBSelect a -> Int -> DBSelect a
setOffset dbs i = dbs { selOffset = fmtSql "OFFSET ?" (Only i) }

-- | A 'DBSelect' that returns all rows of a model.
modelDBSelect :: (Model a) => DBSelect a
modelDBSelect = r
  where mi = modelIdentifiers `gAsTypeOf1` r
        r = emptyDBSelect {
          selFields = Query $ S.intercalate ", " $ modelQColumns mi
          , selFrom = FromModel (Query $ modelQTable mi) (modelQTable mi)
          }

-- | Run a 'DBSelect' query on parameters.  The number of @\'?\'@
-- characters embedeed in various fields of the 'DBSelect' must
-- exactly match the number of fields in parameter type @p@.  Note the
-- order of arguments is such that the 'DBSelect' can be pre-rendered
-- and the parameters supplied later.  Hence, you should use this
-- version when the 'DBSelect' is static.  For dynamically modified
-- 'DBSelect' structures, you may prefer 'dbSelect'.
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
-- simply taken from the second 'DBSelect', meaning fields such as
-- 'selWith', 'selGroupBy', and 'selOrderBy' in the in the first
-- 'DBSelect' are entirely ignored.
dbJoin :: (Model a, Model b) =>
          DBSelect a      -- ^ First table
          -> Query        -- ^ Join keyword (@\"JOIN\"@, @\"LEFT JOIN\"@, etc.)
          -> DBSelect b   -- ^ Second table
          -> Query  -- ^ Predicate (if any) including @ON@ or @USING@ keyword
          -> DBSelect (a :. b)
dbJoin left joinOp right onClause = addWhere_ (selWhere left) right {
    selFields = Query $ S.concat [fromQuery $ selFields left, ", ",
                                  fromQuery $ selFields right]
  , selFrom = FromJoin (selFrom left) joinOp (selFrom right) onClause
              (modelQTable idab)
  }
  where idab = modelIdentifiers `gAsTypeOf`
               (undefined :: g a -> g b -> a :. b) left right

-- | A version of 'dbJoin' that uses 'modelDBSelect' for the joined
-- tables.
dbJoinModels :: (Model a, Model b) =>
                Query           -- ^ Join keyword
                -> Query        -- ^ @ON@ or @USING@ predicate
                -> DBSelect (a :. b)
dbJoinModels kw on = dbJoin modelDBSelect kw modelDBSelect on

-- | Restrict the fields returned by a DBSelect to be those of a
-- single 'Model' @a@.  It only makes sense to do this if @a@ is part
-- of @something_containing_a@, but no static check is performed that
-- this is the case.  If you @dbProject@ a type that doesn't make
-- sense, you will get a runtime error from a failed database query.
dbProject :: (Model a) => DBSelect something_containing_a -> DBSelect a
{-# INLINE dbProject #-}
dbProject dbs = r
  where sela = modelDBSelect `gAsTypeOf1` r
        r = dbs { selFields = selFields sela }

-- | Like 'dbProject', but renders the entire input 'DBSelect' as a
-- subquery.  Hence, you can no longer mention fields of models other
-- than @a@ that might be involved in joins.  The two advantages of
-- this approach are 1) that you can once again join to tables that
-- were part of the original query without worrying about row aliases,
-- and 2) that all terms of the 'DBSelect' will be faithrully rendered
-- into the subquery (whereas otherwise they could get dropped by join
-- operations).  Generally you will still want to use 'dbProject', but
-- @dbProject'@ is available when needed.
dbProject' :: (Model a) => DBSelect something_containing_a -> DBSelect a
dbProject' dbs = r
  where sela = modelDBSelect `gAsTypeOf1` r
        ida = modelIdentifiers `gAsTypeOf1` r
        Just mq = modelQualifier ida
        q = toQuery $ fromChar '(' <>
            buildDBSelect dbs { selFields = selFields sela } <>
            fromByteString ") AS " <> fromByteString mq
        r = sela { selFrom = FromModel q $ modelQTable ida }

mergeFromClauses :: S.ByteString -> FromClause -> FromClause -> FromClause
mergeFromClauses canon left right =
  case go left of
    (fc, 1) -> fc
    (_, 0)  -> error $ "mergeFromClauses could not find " ++ show canon
    (_, _)  -> error $ "mergeFromClauses found duplicate " ++ show canon
  where go fc | fcCanonical fc == canon = (right, 1 :: Int)
        go (FromJoin l op r on ffc) =
          case (go l, go r) of
            ((lfc, ln), (rfc, rn)) -> (FromJoin lfc op rfc on ffc, ln + rn)
        go fc = (fc, 0)

-- | Nest two type-compatible @JOIN@ queries.  As with 'dbJoin',
-- fields of the first @JOIN@ (the @'DBSelect' (a :. b)@) other than
-- 'selFields', 'selFrom', and 'selWhere' are entirely ignored.
dbNest :: (Model a, Model b) =>
          DBSelect (a :. b) -> DBSelect (b :. c) -> DBSelect (a :. b :. c)
dbNest left right = addWhere_ (selWhere left) right {
    selFields = fields
  , selFrom = mergeFromClauses nameb (selFrom left) (selFrom right)
  }
  where nameb = modelQTable $ modelIdentifiers `gAsTypeOf1_1` left
        acols = modelQColumns $ modelIdentifiers `gAsTypeOf1_2` left
        colcomma c r = fromByteString c <> fromByteString ", " <> r
        fields = toQuery $ foldr colcomma (qBuilder $ selFields right)
                 acols

-- | Like 'dbNest', but projects away the middle type @b@.
dbChain :: (Model a, Model b, Model c) =>
           DBSelect (a :. b) -> DBSelect (b :. c) -> DBSelect (a :. c)
dbChain left right = dbProject $ dbNest left right
