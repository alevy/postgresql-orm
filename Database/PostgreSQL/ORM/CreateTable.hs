{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for creating a table from a model.  These are mostly
-- useful in development, for very rigid applications, or to compare
-- what would be created against what is actually in the database.  In
-- practice, production settings should create and update tables using
-- migrations.
--
-- Note that often it is more interesting to see what would be created
-- than to create an actual table.  For that reason, functions
-- creating the statements are exposed.
module Database.PostgreSQL.ORM.CreateTable (
  modelCreateStatement, modelCreate, GDefTypes
  , jtCreateStatement, jtCreate
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Int
import Data.List
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Data.AsTypeOf
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.Association
import Database.PostgreSQL.ORM.SqlType

-- | This is a helper class used to extract the row types.  You don't
-- need to use this class.  If you are creating custom types, just
-- declare an instance of 'SqlType'.
class GDefTypes f where
  gDefTypes :: f p -> [S.ByteString]
instance (SqlType c) => GDefTypes (K1 i c) where
  gDefTypes ~(K1 c) = [sqlType c]
instance (GDefTypes a, GDefTypes b) => GDefTypes (a :*: b) where
  gDefTypes ~(a :*: b) = gDefTypes a ++ gDefTypes b
instance (GDefTypes f) => GDefTypes (M1 i c f) where
  gDefTypes ~(M1 fp) = gDefTypes fp

customModelCreateStatement ::
  (Model a, Generic a, GDefTypes (Rep a)) =>
  [(S.ByteString, S.ByteString)]
  -- ^ A list of @(@/field/@,@/type/@)@ pairs to overwrite the default
  -- SQL types of fields.
  -> [S.ByteString]
  -- ^ A list of extra table constraints.
  -> a
  -- ^ A non-strict argument to specify which model's table you want
  -- to create.  @(undefined :: YourModel)@ should be fine.
  -> Query
customModelCreateStatement except constraints a
  | not (null extraneous) =
    error $ "customCreateTableStatement: no such columns: " ++ show extraneous
  | otherwise = Query $ S.concat [
  "CREATE TABLE ", quoteIdent $ modelTable info, " ("
  , S.intercalate ", " (go types names)
  , S.concat $ concatMap (\c -> [", ", c]) constraints, ")"
  ]
  where extraneous = fst (unzip except) \\ names
        types = gDefTypes $ from a
        info = modelInfo `gAsTypeOf` a
        names = modelColumns info
        go (t:ts) (n:ns)
          | Just t' <- lookup n except = quoteIdent n <> " " <> t' : go ts ns
          | otherwise = quoteIdent n <> " " <> t : go ts ns
        go [] [] = []
        go _ _ = error $ "createTable: " ++ S8.unpack (modelTable info) ++
                 " has incorrect number of columns"

-- | Statement for creating the table corresponding to a model.  Not
-- strict in its argument.
modelCreateStatement :: (Model a, Generic a, GDefTypes (Rep a)) => a -> Query
modelCreateStatement a = customModelCreateStatement except constraints a
  where ModelCreateInfo except constraint = modelCreateInfo `gAsTypeOf` a
        constraints = if S.null constraint then [] else [constraint]

-- | Create a the database table for a model.
modelCreate :: (Model a, Generic a, GDefTypes (Rep a)) =>
               Connection -> a -> IO Int64
modelCreate c a = execute_ c (modelCreateStatement a)

-- | Create the database table corresponding to a 'JoinTable'.
jtCreateStatement :: (Model a, Model b) => JoinTable a b -> Query
jtCreateStatement jt = Query $ S.concat [
    "CREATE TABLE ", quoteIdent $ jtTable jt, " ("
    , S.intercalate ", " $ sort [typa, typb]
    , ", UNIQUE (", S.intercalate ", " $ sort [ida, idb], "))"
  ]
  where ida = quoteIdent $ jtColumnA jt
        idb = quoteIdent $ jtColumnB jt
        refa = (undefined :: JoinTable a b -> DBRef a) jt
        refb = (undefined :: JoinTable a b -> DBRef b) jt
        typa = ida <> " " <> sqlBaseType refa <> " ON DELETE CASCADE NOT NULL"
        typb = idb <> " " <> sqlBaseType refb <> " ON DELETE CASCADE NOT NULL"

-- | Create a join table in the database.
jtCreate :: (Model a, Model b) => Connection -> JoinTable a b -> IO Int64
jtCreate c jt = execute_ c (jtCreateStatement jt)
