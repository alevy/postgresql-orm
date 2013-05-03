{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Database.PostgreSQL.ORM.Fields where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Monoid
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.TypeInfo.Static

import Database.PostgreSQL.ORM.Model

import GHC.Generics

class (ToField a, FromField a) => SqlType a where
  sqlType :: a -> S.ByteString
  sqlType a = (sqlBaseType $ undefined `asTypeOf` a) <> " NOT NULL"
  sqlBaseType :: a -> S.ByteString

#define TYPE(hs, sql) \
    instance SqlType (hs) where sqlBaseType _ = typname (sql)
TYPE(Bool, bool)
TYPE(Double, float8)
TYPE(Float, float4)
TYPE(Int16, int2)
TYPE(Int32, int4)
TYPE(Int64, int8)
TYPE(S.ByteString, text)
TYPE(L.ByteString, text)
TYPE(ST.Text, text)
TYPE(LT.Text, text)
TYPE(Oid,oid)
TYPE(LocalTime, timestamp)
TYPE(ZonedTime, timestamptz)
TYPE(TimeOfDay, time)
TYPE(UTCTime, timestamptz)
TYPE(Day, date)
TYPE(Date, date)
TYPE(ZonedTimestamp, timestamptz)
TYPE(UTCTimestamp, timestamptz)
TYPE(LocalTimestamp, timestamp)
TYPE(String, text)
TYPE(Binary S.ByteString, bytea)
TYPE(Binary L.ByteString, bytea)

#undef TYPE

instance SqlType DBKey where
  sqlType _ = "bigserial UNIQUE NOT NULL PRIMARY KEY"
  sqlBaseType _ = error "DBKey should not be wrapped in type"

instance (SqlType a) => SqlType (Maybe a) where
  sqlType ~(Just a) = sqlBaseType a
  sqlBaseType _ = error "Table field Maybe should not be wrapped in other type"

dbRefToInfo :: (Model t) => GDBRef rt t -> ModelInfo t
dbRefToInfo _ = modelInfo

instance (Model a) => SqlType (DBRef a) where
  sqlBaseType r@(GDBRef k) = sqlBaseType k <> ref
    where t = dbRefToInfo r
          ref = S.concat [
              " references ", quoteIdent (modelTable t) , "("
              , quoteIdent (modelColumns t !! modelPrimaryColumn t), ")" ]

instance (Model a) => SqlType (DBURef a) where
  sqlBaseType r@(GDBRef k) = sqlBaseType k <> ref
    where t = dbRefToInfo r
          ref = S.concat [
              " unique references ", quoteIdent (modelTable t) , "("
              , quoteIdent (modelColumns t !! modelPrimaryColumn t), ")" ]

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
