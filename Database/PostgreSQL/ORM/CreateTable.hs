{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Database.PostgreSQL.ORM.CreateTable where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Int
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Database.PostgreSQL.ORM.Model
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
        info = modelToInfo a
        names = modelColumns info
        go (t:ts) (n:ns)
          | Just t' <- lookup n except = quoteIdent n <> " " <> t' : go ts ns
          | otherwise = quoteIdent n <> " " <> t : go ts ns
        go [] [] = []
        go _ _ = error $ "createTable: " ++ S8.unpack (modelTable info)
                 ++ " has incorrect number of columns"


class (Model a, Generic a, GDefTypes (Rep a)) => CreateTable a where
  createTableTypes :: ModelInfo a -> [(S.ByteString, S.ByteString)]
  createTableTypes _ = []

createTable :: (CreateTable a) => a -> Query
createTable a = createTableWithTypes (createTableTypes $ modelToInfo a) a


data Bar = Bar {
  bar_key :: !DBKey
  , bar_none :: !Int32
  , bar_string :: !String
  , bar_parent :: !(Maybe (DBURef Bar))
  } deriving (Show, Generic)
                                    
instance Model Bar
instance CreateTable Bar where
  createTableTypes _ = [("barString", "varchar(16)")]

bar :: Bar
bar = Bar NullKey 77 "hi" Nothing

mkc :: IO Connection
mkc = connectPostgreSQL ""

bar' :: Bar
bar' = Bar NullKey 78 "bye" Nothing


x :: Maybe Int32
x = Just 5

y :: Maybe Float
y = Just 6.0
