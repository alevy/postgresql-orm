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

data FromClause = FromModel { fcVerbatim :: !Query
                            , fcCanonical :: !S.ByteString }
                | FromJoin { fcLeft :: !FromClause
                           , fcJoinOp :: !Query
                           , fcRight :: !FromClause
                           , fcOnClause :: !Query
                           , fcCanonical :: !S.ByteString }
                  deriving Show

-- | A deconstructed SQL select statement.
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

buildDBSelect :: DBSelect a -> Builder
buildDBSelect dbs = gdbsQuery $ from dbs

renderDBSelect :: DBSelect a -> Query
renderDBSelect = Query . S.tail . toByteString . buildDBSelect
-- S.tail is because the rendering inserts an extra space at the beginning

catQueries :: Query -> Query -> Query -> Query
catQueries left delim right
  | qNull left  = right
  | qNull right = left
  | otherwise   = Query $ S.concat $ map fromQuery [left, delim, right]

addWhere_ :: Query -> DBSelect a -> DBSelect a
addWhere_ q dbs
  | qNull q = dbs
  | otherwise = dbs { selWhereKeyword = "WHERE"
                    , selWhere = catQueries (selWhere dbs) " AND " q }

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
          , selFrom = FromModel (Query $ modelQTable mi) (modelQTable mi)
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
dbJoin :: (Model a, Model b) =>
          DBSelect a      -- ^ First table
          -> Query        -- ^ Join keyword (@\"JOIN\"@, @\"LEFT JOIN\"@, etc.)
          -> DBSelect b   -- ^ Second table
          -> Query  -- ^ Predicate (if any) including @ON@ or @USING@ keyword
          -> DBSelect (a :. b)
dbJoin left joinOp right onClause = addWhere_ (selWhere right) left {
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
-- than @a@ that might be involved in joins.
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

dbNest :: (Model a, Model b) =>
          DBSelect (a :. b) -> DBSelect (b :. c) -> DBSelect (a :. b :. c)
dbNest left right = addWhere_ (selWhere right) left {
    selFields = fields
  , selFrom = mergeFromClauses nameb (selFrom left) (selFrom right)
  }
  where nameb = modelQTable $ modelIdentifiers `gAsTypeOf1_1` left
        acols = modelQColumns $ modelIdentifiers `gAsTypeOf1_2` left
        colcomma c r = fromByteString c <> fromByteString ", " <> r
        fields = toQuery $ foldr colcomma (qBuilder $ selFields right)
                 acols

dbChain :: (Model a, Model b, Model c) =>
           DBSelect (a :. b) -> DBSelect (b :. c) -> DBSelect (a :. c)
dbChain left right = dbProject $ dbNest left right
