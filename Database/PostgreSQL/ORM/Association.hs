{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.ORM.Association (
    Association(..), assocProject, assocWhere, findAssoc
    -- * Associations based on parent-child relationships
  , GDBRefInfo(..), DBRefInfo, dbrefAssocs-- ,has, belongsTo, defaultDBRefInfo
    -- * Join table Associations
  , JoinTable(..), defaultJoinTable, jtAssocs, joinTable
    -- ** Operations on join tables
  , jtAdd, jtRemove, jtRemoveByRef
    -- ** Semi-internal join table functions
  , jtAddStatement, jtRemoveStatement, jtParam
  , jtFlip, jtAssoc
    -- * Nested and chained associations
  , nestAssoc, chainAssoc
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.List
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import Data.GetField
import Database.PostgreSQL.Escape
import Database.PostgreSQL.ORM.DBSelect
import Database.PostgreSQL.ORM.Model


-- | A data structure representing a relationship between a model @a@
-- and a model @b@.  At a high level, an @Association a b@ tells you
-- how to find rows of type @b@ given rows of type @a@.  More
-- concretely, this boils down to being able to make two types of
-- query.
--
--  * You want to look up a bunch of @(a ':.' b)@s, filtering using
--  predicates on both @a@ and @b@ (e.g., get a list of recent posts
--  and their authors).  For this purpose, you can use 'assocSelect',
--  which allows you to 'addWhere' predicates mentioning columns in
--  both @a@ and @b@.
--
--  * You already have an instance of type @a@, and want to find all
--  the @b@s associated with it.  For that you use either 'assocWhere'
--  or 'findAssoc' (which internally access fields 'assocSelectOnlyB',
--  'assocWhereQuery', and 'assocWhereParam').  This type of query is
--  strictly less general than the first one, but can be formulated in
--  a more efficient way by extracting values directly from a concrete
--  instance of @a@ without needing to touch table @a@ in the
--  database.
--  
-- Note that an @Association@ is asymmetric.  It tells you how to get
-- @b@s from @a@s, but not vice versa.  In practice, there will almost
-- always be an association in the other direction, too.  Functions
-- such as 'dbrefAssocs' and 'jtAssocs' therefore create an
-- @Association@ and its inverse simultaneously, returning them as a
-- pair.
data Association a b = Association {
    assocSelect :: !(DBSelect (a :. b))
    -- ^ General select returning all instances of @a@ and @b@ that
    -- match according to the association.
  , assocSelectOnlyB :: !(DBSelect b)
    -- ^ The right-hand side of the 'assocSelect' query.  This query
    -- makes no mention of type @a@ (but can be combined with the next
    -- two fields to form an optimized query).  You probably never
    -- want to use this directly, and should instead use either
    -- 'findAssoc' or 'assocWhere'.  Also note this is not useful for
    -- selecting all the @b@s in the relation; for that you should use
    -- 'assocProject'.
  , assocWhereQuery :: !Query
    -- ^ A @WHERE@ clause to find all the 'b's associated with a
    -- particular @a@.  This can often be done more efficiently than
    -- through 'assocSelect'.  The clause contains @\'?\'@ characters
    -- which should be filled in by 'assocWhereParam'.
  , assocWhereParam :: !(a -> [Action])
    -- ^ The query parameters for the query returned by
    -- 'assocWhereQuery'.
  }

instance Show (Association a b) where
  show assoc =
    "Association { assocSelect = " ++ show (assocSelect assoc) ++
    ", assocSelectOnlyB = " ++ show (assocSelectOnlyB assoc) ++
    ", assocWhereQuery = " ++ S8.unpack (fromQuery $ assocWhereQuery assoc) ++
    " }"

-- | A projection of 'assocSelect', extracting only the fields of
-- model @b@.  Note that this query touches table @a@ even if it does
-- not return results from @a@.  Hence, you can use 'addWhere' to add
-- predicates on both @a@ and @b@.  (Note the contrast to
-- 'assocSelectOnlyB', which does not touch table @a@ at all, and
-- hence in the case of an @INNER JOIN@ might return rows of @b@ that
-- should not be part of the association.  'assocSelectOnlyB' is
-- intended for use only in conjunction with 'assocWhereQuery'.)
assocProject :: (Model b k) => Association a b -> DBSelect b
assocProject = dbProject . assocSelect

-- | Returns a 'DBSelect' for all @b@s associated with a particular
-- @a@.
assocWhere :: (Model b k) => Association a b -> a -> DBSelect b
assocWhere ab a = addWhere (assocWhereQuery ab) (assocWhereParam ab a)
                  (assocSelectOnlyB ab)

-- | Follow an association to return all of the @b@s associated
-- with a particular @a@.  The behavior is similar to:
--
-- > findAssoc' ab c a = dbSelect c $ assocWhere ab a
--
-- But if the first argument is a static association, this function
-- may be marginally faster because it pre-renders most of the query.
findAssoc :: (Model b k) => Association a b -> Connection -> a -> IO [b]
{-# INLINE findAssoc #-}
findAssoc assoc = \c a ->
  map lookupRow <$> query c q (assocWhereParam assoc a)
  where {-# NOINLINE q #-}
        q = renderDBSelect $
            addWhere_ (assocWhereQuery assoc) $ assocSelectOnlyB assoc

-- | Combine two associations into one.
nestAssoc :: (Model a k1, Model b k2) =>
             Association a b -> Association b c -> Association a (b :. c)
nestAssoc ab bc = ab { assocSelect = dbNest (assocSelect ab) (assocSelect bc)
                     , assocSelectOnlyB = assocSelect bc }

-- | Combine two associations into one, and project away the middle
-- type.  (The middle type can still be mentioned in @WHERE@ clauses.)
--
-- An example:
--
-- > data Author = Author {
-- >     authorId :: DBKey
-- >   } deriving (Show, Generic)
-- > instance Model Author where modelInfo = underscoreModelInfo "author"
-- > 
-- > data Post = Post {
-- >     postId :: DBKey
-- >   , postAuthorId :: DBRef Author
-- >   } deriving (Show, Generic)
-- > instance Model Post where modelInfo = underscoreModelInfo "post"
-- > 
-- > data Comment = Comment {
-- >     commentId :: DBKey
-- >   , commentPostId :: DBRef Post
-- >   } deriving (Show, Generic)
-- > instance Model Comment where modelInfo = underscoreModelInfo "comment"
-- > 
-- > author_posts :: Association Author Post
-- > post_author :: Association Post Author
-- > (post_author, author_posts) = dbrefAssocs defaultDBRefInfo
-- > 
-- > -- Could equally well use dbrefAssocs as above
-- > post_comments :: Association Post Comment
-- > post_comments = has
-- >
-- > comment_post :: Association Comment Post
-- > comment_post = belongsTo
-- > 
-- > comment_author :: Association Comment Author
-- > comment_author = chainAssoc comment_post post_author
-- > 
-- > author_comments :: Association Author Comment
-- > author_comments =  chainAssoc author_posts post_comments
chainAssoc :: (Model a k1, Model b k2, Model c k3) =>
              Association a b -> Association b c -> Association a c
chainAssoc ab bc = ab { assocSelect = dbChain (assocSelect ab) (assocSelect bc)
                      , assocSelectOnlyB = dbProject $ assocSelect bc }


-- | A common type of association is when one model contains a 'DBRef'
-- or 'DBRefUnique' pointing to another model.  In this case, the
-- model containing the 'DBRef' is known as the /child/, and the
-- referenced model is known as the /parent/.
--
-- Two pieces of information are required to describe a parent-child
-- relationship:  First, the field selector that extracts the Haskell
-- 'DBRef' from the haskell type @child@, and second the name of the
-- database column that stores this 'DBRef' field.
--
-- For example, consider the following:
--
-- > data Author = Author {
-- >     authorId :: DBKey
-- >   } deriving (Show, Generic)
-- > instance Model Author
-- > 
-- > data Post = Post {
-- >     postId :: DBKey
-- >   , postAuthorId :: DBRef Author
-- >   } deriving (Show, Generic)
-- > instance Model Post
-- >
-- > post_author_refinfo :: DBRefInfo Post Author
-- > post_author_refinfo = DBRefInfo {
-- >     dbrefSelector = postAuthorId
-- >   , dbrefQColumn = "\"post\".\"postAuthorId\""
-- >   }
--
-- Note that the parent-child relationship described by a @GDBRefInfo@
-- is asymmetric, but bidirectional.  When a @'DBRefInfo' child
-- parent@ exists, the schema should generally /not/ permit the
-- existence of a valid @'DBRefInfo' parent child@ structure.
-- However, the 'dbrefAssocs' function generates 'Association's in
-- both directions from a single 'DBRefInfo'.
--
-- Constructing such parent-child 'Association's requires knowing how
-- to extract primary keys from the @parent@ type as well as the name
-- of the column storing primary keys in @parent@.  Fortunately, this
-- information is already available from the 'Model' class, and thus
-- does not need to be in the @GDBRefInfo@.  (Most functions on
-- @GDBRefInfo@s require @parent@ and @child@ to be instances of
-- 'Model'.)
--
-- When your 'Model's are instances of 'Generic' (which will usually
-- be the case), a 'DBRefInfo' structure can be computed automatically
-- by 'defaultDBRefInfo'.  This is the recommended way to produce a
-- @GDBRefInfo@.  (Alternatively, see 'has' and 'belongsTo' to make
-- use of an entirely implicit @DBRefInfo@.)
data GDBRefInfo reftype child parent parent_key = DBRefInfo {
    dbrefSelector :: !(child -> GDBRef reftype parent parent_key)
    -- ^ Field selector returning a reference.
  , dbrefQColumn :: !S.ByteString
    -- ^ Literal SQL for the database column storing the reference.
    -- This should be double-quoted and table-qualified, in case the
    -- column name is a reserved keyword, contains capital letters, or
    -- conflicts with the name of a column in the joined table.  An
    -- example would be:
    --
    -- > dbrefQColumn = "\"table_name\".\"column_name\""
  }

instance Show (GDBRefInfo rt c p k) where
  show ri = "DBRefInfo ? " ++ show (dbrefQColumn ri)

-- | @DBRefInfo@ is a type alias for the common case that the
-- reference in a 'GDBRefInfo' is a 'DBRef' (as opposed to a
-- 'DBRefUnique').  The functions in this library do not care what
-- type of reference is used.  The type is generalized to 'GDBRefInfo'
-- just to make it easier to assign a selector to 'dbrefSelector' when
-- the selector returns a 'DBRefUnique'.  Note, however, that
-- 'defaultDBRefInfo' returns a 'DBRefInfo' regardless of the flavor
-- of reference actually encountered.
type DBRefInfo = GDBRefInfo NormalRef

data ExtractRef a = ExtractRef deriving (Show)
instance Extractor ExtractRef (GDBRef rt a k) (DBRef a k) THasOne where
  extract _ (DBRef k) = THasOne $ DBRef k
instance Extractor ExtractRef (GDBRef rt a k) (DBRef (As alias a) k) THasOne where
  extract _ (DBRef k) = THasOne $ DBRef k
instance Extractor ExtractRef (Maybe (GDBRef rt a k)) (DBRef a k) THasOne where
  extract _ (Just (DBRef k)) = THasOne $ DBRef k
  extract _ _                = error "Maybe DBRef is Nothing"
instance Extractor ExtractRef (Maybe (GDBRef rt a k)) (DBRef (As alias a) k)
         THasOne where
  extract _ (Just (DBRef k)) = THasOne $ DBRef k
  extract _ _                = error "Maybe DBRef is Nothing"

-- | Creates a 'DBRefInfo' from a model @child@ that references
-- @parent@.  For this to work, the @child@ type must be an instance
-- of 'Generic' and must contain exactly one field of the any of the
-- following types:
--
--   1. @'GDBRef' rt parent@, which matches both @'DBRef' parent@ and
--   @'DBRefUnique' parent@.
--
--   2. @Maybe ('GDBRef' rt parent)@, for cases where the reference
--   might be @NULL@.  Note, however, that an exception will be thrown
--   if you call 'findAssoc' on a child whose reference is 'Nothing'.
--
-- A special case arises when a Model contains a 'DBRef' to itself.
-- If you just wish to find parents and children given an existing
-- structure (i.e., 'findAssoc'), it is okay to declare an
-- @'Association' MyType MyType@.  However, in this case attempts to
-- use 'assocSelect' will then fail.  To work around this problem, the
-- parent must use a row alias.
--
-- Note that currently /aliasing the child will not work/, since the
-- 'As' data structure will not contain a 'DBRef' field, only the
-- contents of the 'As' data structure.  An example of doing this
-- correctly (using 'has' and 'belongsTo', both of which wrap
-- @defaultDBRefInfo@):
--
-- > data Bar = Bar {
-- >     barId :: !DBKey
-- >   , barName :: !String
-- >   , barParent :: !(Maybe (DBRef Bar))
-- >   } deriving (Show, Generic)
-- > instance Model Bar where modelInfo = underscoreModelInfo "bar"
-- > 
-- > data ParentBar = ParentBar
-- > instance RowAlias ParentBar where rowAliasName _ = "parent_bar"
-- > 
-- > toParent :: Association Bar (As ParentBar Bar)
-- > toParent = belongsTo
-- > 
-- > toChild :: Association (As ParentBar Bar) Bar
-- > toChild = has
-- TODO
-- defaultDBRefInfo :: forall child kc parent kp.
--                     (Model child kc, Model parent kp
--                     , GetField ExtractRef child (DBRef parent kp)) =>
--                     DBRefInfo child parent kp
-- defaultDBRefInfo = ri
--   where extractor = (const ExtractRef :: g kp -> ExtractRef (DBRef p kp)) ri
--         child = undefined :: child
--         childids = modelIdentifiers :: ModelIdentifiers child
--         ri :: DBRefInfo child parent kp
--         ri = DBRefInfo {
--             dbrefSelector = getFieldVal extractor
--           , dbrefQColumn = modelQColumns childids !! getFieldPos extractor child
--           }

-- | Generate both the child-parent and parent-child 'Association's
-- implied by a 'GDBRefInfo'.
dbrefAssocs :: forall child kc parent kp rt.
               (ToField kp, Model child kc, Model parent kp) =>
               GDBRefInfo rt child parent kp
               -> (Association child parent, Association parent child)
dbrefAssocs ri = (c_p, p_c)
  where idp = modelIdentifiers :: ModelIdentifiers parent
        on = Query $ "ON " <> modelQPrimaryColumn idp
             <> " = " <> dbrefQColumn ri
        c_p = Association {
            assocSelect = dbJoinModels "JOIN" on
          , assocSelectOnlyB = modelDBSelect
          , assocWhereQuery = Query $ modelQPrimaryColumn idp <> " = ?"
          , assocWhereParam = \child -> [toField $ dbrefSelector ri child]
          }
        p_c = Association {
            assocSelect = dbJoinModels "JOIN" on
          , assocSelectOnlyB = modelDBSelect
          , assocWhereQuery = Query $ dbrefQColumn ri <> " = ?"
          , assocWhereParam = \parent -> [toField $ primaryKey parent]
          }


-- | Short for
--
-- > snd $ dbrefAssocs defaultDBRefInfo
--
-- Note the inverse 'Association' is given by 'belongsTo'.  For
-- example, given the @Author@ and @Post@ models described in the
-- documentation for 'GDBRefInfo', in which each @Post@ references an
-- @Author@, you might say:
--
-- > author_post :: Association Author Post
-- > author_post = has
-- >
-- > post_author :: Association Post Author
-- > post_author = belongsTo
-- TODO
-- has :: (ToField kp, Model child kc, Model parent kp,
--         GetField ExtractRef child (DBRef parent kp))
--     => Association parent child
-- has = snd $ dbrefAssocs defaultDBRefInfo
-- 
-- | The inverse of 'has'.  Short for
--
-- > fst $ dbrefAssocs defaultDBRefInfo
--
-- See an example at 'has'.
-- TODO
-- belongsTo :: (ToField kp, Model child kc, Model parent kp
--              , GetField ExtractRef child (DBRef parent kp)) =>
--              Association child parent
-- belongsTo = fst $ dbrefAssocs defaultDBRefInfo

-- | A data structure representing a dedicated join table in the
-- database.  A join table differs from a model in that rows do not
-- have primary keys.  Hence, model operations do not apply.
-- Nonetheless a join table conveys information about a relationship
-- between models.
--
-- Note that all names in a @JoinTable@ should be unquoted.
data JoinTable a b = JoinTable {
    jtTable :: !S.ByteString
    -- ^ Name of the join table in the database.  (Not quoted.)
  , jtColumnA :: !S.ByteString
    -- ^ Name of the column in table 'jtTable' that contains a 'DBRef'
    -- to model @a@.  (Not quoted or table-qualified.)
  , jtColumnB :: !S.ByteString
    -- ^ Like 'jtColumnA' for model @b@.
  } deriving (Show)

-- | The default join table has the following fields:
--
-- * 'jtName' is the name of the two models (in alphabetical order),
-- separated by an @\'_\'@ character.
--
-- * 'jtColumnA' is the name of model @a@, an @\'_\'@ character, and
-- the name of the primary key column in table @a@.
--
-- * 'jtColumnB' is the name of model @b@, an @\'_\'@ character, and
-- the name of the primary key column in table @b@.
--
-- Note that 'defaultJoinTable' cannot create a default join table for
-- joining a model to itself, as following these rules the two columns
-- would have the same name.  If you wish to join a table to itself,
-- you have two options:  First, you can define the join table and
-- assign the column names manually.  This will permit you to call
-- 'findAssoc', but you still will not be able to use 'assocSelect'
-- for more complex queries, since SQL does not permit joins between
-- two tables with the same name.  The second option is to give one of
-- the sides of the join table a row alias with 'As'.  For example:
--
-- > data ParentBar = ParentBar
-- > instance RowAlias ParentBar where rowAliasName _ = "parent_bar"
-- > 
-- > selfJoinTable :: JoinTable Bar (As ParentBar Bar)
-- > selfJoinTable = defaultJoinTable
-- > 
-- > selfJoin :: Association Bar (As ParentBar Bar)
-- > otherSelfJoin :: Association (As ParentBar Bar) Bar
-- > (selfJoin, otherSelfJoin) = jtAssocs selfJoinTable
defaultJoinTable :: forall a k1 b k2. (Model a k1, Model b k2) => JoinTable a b
defaultJoinTable
  | colA == colB = error "defaultJoinTable has default for self joins"
  | otherwise = jti
  where a = modelInfo :: ModelInfo a k1
        b = modelInfo :: ModelInfo b k2
        colA = S.intercalate "_"
               [modelTable a, modelColumns a !! modelPrimaryColumn a]
        colB = S.intercalate "_"
               [modelTable b, modelColumns b !! modelPrimaryColumn b]
        jti = JoinTable {
            jtTable = S.intercalate "_" $ sort [modelTable a, modelTable b]
          , jtColumnA = colA
          , jtColumnB = colB
          }

jtQTable :: JoinTable a b -> S.ByteString
jtQTable = quoteIdent . jtTable

jtQColumnA :: JoinTable a b -> S.ByteString
jtQColumnA jt = S.concat [ jtQTable jt, ".", quoteIdent $ jtColumnA jt]

jtQColumnB :: JoinTable a b -> S.ByteString
jtQColumnB jt = S.concat [ jtQTable jt, ".", quoteIdent $ jtColumnB jt]

-- | Flip a join table.  This doesn't change the name of the table
-- (since the same join table is used in both directions, and the
-- default join table name glues together the two model names in
-- alphabetical order anyway).
jtFlip :: JoinTable a b -> JoinTable b a
jtFlip jt = jt { jtColumnA = jtColumnB jt , jtColumnB = jtColumnA jt }

-- | A SQL statement suitable for adding a pair to a join table.  Note
-- that the statement takes two parameters (i.e., contains two @\'?\'@
-- characters) corresponding to the primary keys of the two models
-- being associated.  These parameters can be supplied by 'jtParam'.
jtAddStatement :: JoinTable a b -> Query
jtAddStatement jt = Query $ S.concat [
    "INSERT INTO ", jtQTable jt, " ("
  , quoteIdent $ jtColumnA jt, ", ", quoteIdent $ jtColumnB jt
  , ") VALUES (?, ?) EXCEPT SELECT "
  , jtQColumnA jt, ", ", jtQColumnB jt, " FROM ", quoteIdent $ jtTable jt
  ]

-- | Add an association between two models to a join table.  Returns
-- 'True' if the association was not already there.
jtAdd :: (ToField k1, ToField k2, Model a k1, Model b k2)
      => JoinTable a b -> Connection -> a -> b -> IO Bool
{-# INLINE jtAdd #-}
jtAdd jt = \c a b -> (/= 0) <$> execute c q (jtParam jt a b)
  where {-# NOINLINE q #-}
        q = jtAddStatement jt

-- | A SQL statement for removing a pair from a join table.  Like
-- 'jtAddStatement', the query is parameterized by two primary keys.
jtRemoveStatement :: JoinTable a b -> Query
jtRemoveStatement jt = Query $ S.concat [
    "DELETE FROM ", quoteIdent $ jtTable jt, " WHERE "
  , jtQColumnA jt, " = ? AND ", jtQColumnB jt, " = ?"
  ]

-- | Remove an association from a join table.  Returns 'True' if the
-- association was previously there.
jtRemove :: (ToField k1, ToField k2, Model a k1, Model b k2) =>
            JoinTable a b -> Connection -> a -> b -> IO Bool
{-# INLINE jtRemove #-}
jtRemove jt = \c a b -> (/= 0) <$> execute c q (jtParam jt a b)
  where {-# NOINLINE q #-}
        q = jtRemoveStatement jt

-- | Remove an assocation from a join table when you don't have the
-- target instances of the two models handy, but do have references.
jtRemoveByRef :: (ToField k1, ToField k2, Model a k1, Model b k2)
              => JoinTable a b -> Connection -> GDBRef rt a k1
              -> GDBRef rt b k2 -> IO Bool
{-# INLINE jtRemoveByRef #-}
jtRemoveByRef jt = \c a b -> (/= 0) <$> execute c q (a, b)
  where {-# NOINLINE q #-}
        q = jtRemoveStatement jt

-- | Generate parameters for 'jtAddStatement' and 'jtRemoveStatement'.
-- The returned list is suitable for use as a 'ToRow' instance.  For
-- example:
--
-- > execute conn (jtAddStatement my_join_table) (jtParam a b)
jtParam :: (ToField k1, ToField k2, Model a k1, Model b k2)
        => JoinTable a b -> a -> b -> [Action]
jtParam _ a b = [toField $ primaryKey a, toField $ primaryKey b]

-- | Generate a one-way association from a 'JoinTable'.  Use
-- 'jtAssocs' instead.
jtAssoc :: forall a k1 b k2. (ToField k1, Model a k1, Model b k2)
        => JoinTable a b -> Association a b
jtAssoc jt = Association {
    assocSelect = dbJoin modelDBSelect "JOIN" onlyB $ Query $ S.concat [
       "ON ", priA, " = ", jtQColumnA jt]
  , assocSelectOnlyB = onlyB
  , assocWhereQuery = Query $ jtQColumnA jt <> " = ?"
  , assocWhereParam = \a -> [toField $ primaryKey a]
  }
  where priA = modelQPrimaryColumn (modelIdentifiers :: ModelIdentifiers a)
        priB = modelQPrimaryColumn (modelIdentifiers :: ModelIdentifiers b)
        selB = modelDBSelect :: DBSelect b
        fromB = FromJoin
                (FromModel (Query $ jtQTable jt) (jtQTable jt))
                "JOIN" (selFrom selB)
                (Query $ S.concat ["ON ", jtQColumnB jt, " = ", priB])
                (jtQTable jt <> "->B")
        onlyB = selB { selFrom = fromB }

-- | Generate the two associations implied by a 'JoinTable'.
jtAssocs :: (ToField k1, ToField k2, Model a k1, Model b k2) =>
            JoinTable a b -> (Association a b, Association b a)
jtAssocs jt = (jtAssoc jt, jtAssoc $ jtFlip jt)

-- | Generate a one-way association based on the default join table
-- naming scheme described at 'defaultJoinTable'.  Defined as:
--
-- > joinTable = jtAssoc defaultJoinTable
--
-- For example:
--
-- > aToB :: Association A B
-- > aToB = joinTable
-- >
-- > bToA :: Association B A
-- > bToA = joinTable
joinTable :: (ToField k1, Model a k1, Model b k2) => Association a b
joinTable = jtAssoc defaultJoinTable
