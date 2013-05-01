{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module PostgresOps where

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
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.TypeInfo.Static

import GenericRow

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

instance (Table a) => SqlType (DBRef a) where
  sqlBaseType r@(DBRef k) = sqlBaseType k <> ref
    where t = dbRefTable r
          ref = " references " <> tableName t <> "(" <> primaryColumn t <> ")"

class GDefTypes f where
  gDefTypes :: f p -> [S.ByteString]
instance (SqlType c) => GDefTypes (K1 i c) where
  gDefTypes ~(K1 c) = [sqlType c]
instance (GDefTypes a, GDefTypes b) => GDefTypes (a :*: b) where
  gDefTypes ~(a :*: b) = gDefTypes a ++ gDefTypes b
instance (GDefTypes f) => GDefTypes (M1 i c f) where
  gDefTypes ~(M1 fp) = gDefTypes fp


createTable :: (Table a, Generic a, GDefTypes (Rep a)) => a -> Query
createTable a = Query $ "create table " <> tableName a <> " ("
                <> S.intercalate ", " (go types names) <> ")"
  where types = gDefTypes $ from a
        names = tableColumns a
        except = columnTypes a
        go (t:ts) (n:ns)
          | Just t' <- lookup n except = n <> " " <> t' : go ts ns
          | otherwise = n <> " " <> t : go ts ns
        go [] [] = []
        go _ _ = error $ "createTable: " ++ S8.unpack (tableName a)
                 ++ " has incorrect number of columns"

{-
saveQuery :: (Table a) => a -> Query
saveQuery a0 =
  where insq 
-}

data Bar = Bar {
  barKey :: !DBKey
  , barNone :: !Int32
  , barString :: !String
  , barParent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)
                                    
instance Table Bar

bar :: Bar
bar = Bar (DBKey 4) 77 "hi" Nothing


x :: Maybe Int32
x = Just 5

y :: Maybe Float
y = Just 6.0
