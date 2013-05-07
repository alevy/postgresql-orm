{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings #-}

module Database.PostgreSQL.ORM.CreateTable where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Data.AsTypeOf
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.Relationships
import Database.PostgreSQL.ORM.SqlType

class GDefTypes f where
  gDefTypes :: f p -> [S.ByteString]
instance (SqlType c) => GDefTypes (K1 i c) where
  gDefTypes ~(K1 c) = [sqlType c]
instance (GDefTypes a, GDefTypes b) => GDefTypes (a :*: b) where
  gDefTypes ~(a :*: b) = gDefTypes a ++ gDefTypes b
instance (GDefTypes f) => GDefTypes (M1 i c f) where
  gDefTypes ~(M1 fp) = gDefTypes fp


createTableWithTypes :: (Model a, Generic a, GDefTypes (Rep a)) =>
                        [(S.ByteString, S.ByteString)] -> a -> Query
createTableWithTypes except a = Query $ S.concat [
  "create table ", quoteIdent $ modelTable info, " ("
  , S.intercalate ", " (go types names), ")"
  ]
  where types = gDefTypes $ from a
        info = modelInfo `gAsTypeOf` a
        names = modelColumns info
        go (t:ts) (n:ns)
          | Just t' <- lookup n except = quoteIdent n <> " " <> t' : go ts ns
          | otherwise = quoteIdent n <> " " <> t : go ts ns
        go [] [] = []
        go _ _ = error $ "createTable: " ++ S8.unpack (modelTable info)
                 ++ " has incorrect number of columns"


createTable :: (Model a, Generic a, GDefTypes (Rep a)) => a -> Query
createTable a = createTableWithTypes [] a

createJoinTable :: (Joinable a b) => (a, b) -> Query
createJoinTable ab
  | not (jtAllowModification jt) = error $ S8.unpack (jtTable jt) ++
                                   ": read-only join table"
  | otherwise = Query $ S.concat [
      "create table ", quoteIdent $ jtTable jt, " ("
    , quoteIdent (jtColumnA jt), " ", sqlType refa
    , ", ", quoteIdent (jtColumnB jt), " ", sqlType refb, ")"
  ]
  where jt = (const joinTable
               :: (Joinable a b) => (a, b) -> JoinTableInfo a b) ab
        (refa, refb) = (const (undefined, undefined)
                        :: (a, b) -> (DBRef a, DBRef b)) ab

