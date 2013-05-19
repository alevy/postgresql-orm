{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.ORM.CreateTable where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.List
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Data.AsTypeOf
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.Association
import Database.PostgreSQL.ORM.SqlType

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

-- | Create a table with all the default fields and no extra
-- constraints.  Equivalent to:
--
-- > modelCreateStatement a = customModelCreateStatement [] [] a
modelCreateStatement :: (Model a, Generic a, GDefTypes (Rep a)) => a -> Query
modelCreateStatement a = customModelCreateStatement [] [] a

{-
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

-}
