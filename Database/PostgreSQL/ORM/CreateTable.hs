{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, MultiParamTypeClasses #-}

module Database.PostgreSQL.ORM.CreateTable where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Int
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Data.AsTypeOf
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.Relationships
import Database.PostgreSQL.ORM.SqlType

-- import Database.PostgreSQL.ORM.Keywords
import Control.Exception
import Data.Functor
import Data.Maybe

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


class (Model a, Generic a, GDefTypes (Rep a)) => CreateTable a where
  createTableTypes :: ModelInfo a -> [(S.ByteString, S.ByteString)]
  createTableTypes _ = []

createTable :: (CreateTable a) => a -> Query
createTable a = createTableWithTypes
                (createTableTypes $ modelInfo `gAsTypeOf` a) a

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


data Foo = Foo {
  foo_key :: !DBKey
  , foo_name :: String
  -- , parent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)
                                    
instance Model Foo
instance CreateTable Foo

mkFoo :: String -> Foo
mkFoo = Foo NullKey

data Bar = Bar {
    bar_key :: !DBKey
  , bar_none :: !(Maybe Int32)
  , bar_name :: !String
  , bar_parent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)

instance Model Bar -- where modelInfo = underscoreModelInfo "bar"
instance CreateTable Bar where
  createTableTypes _ = [("bar_string", "varchar(16)")]

mkBar :: String -> Bar
mkBar msg = Bar NullKey (Just n) msg Nothing
  where n = foldl (+) 0 $ map (toEnum . fromEnum) msg

instance HasMany Bar Bar
instance HasParent Bar Bar

data Joiner = Joiner {
    jkey :: !DBKey
  , jcomment :: !String
  , jfoo :: (DBRef Foo)
  , jbar :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)
instance Model Joiner
instance CreateTable Joiner

joiner :: Joiner
joiner = Joiner (DBKey 5) "join comment" (DBRef 1) Nothing

instance Joinable Foo Bar where
  -- joinTable = (joinThroughModel joiner) { jtAllowModification = True }
  joinTable = joinDefault
instance Joinable Bar Foo where
  joinTable = joinReverse

bar :: Bar
bar = Bar NullKey (Just 44) "hi" Nothing

mkc :: IO Connection
mkc = connectPostgreSQL ""

bar' :: Bar
bar' = Bar NullKey (Just 75) "bye" Nothing

data X = X deriving (Generic)
instance RowAlias X

selfjoin :: IO [Bar :. As X Bar]
selfjoin = bracket mkc close $ \c ->
  findWhere "bar.bar_key = x.bar_parent" c () :: IO [Bar :. As X Bar]

selfjoin' :: IO [(Bar,Bar)]
selfjoin' = bracket mkc close $ \c ->
  map (\(b1 :. b2) -> (b1, fromAs X b2)) <$>
      findWhere "bar.bar_key = X.bar_parent" c ()

getOne :: (Model a) => DBKeyType -> IO a
getOne k = bracket mkc close $ \c ->
  let r = fromJust <$> findRow c (DBRef k `gAsTypeOf1` r)
  in r

x :: Maybe Int32
x = Just 5

y :: Maybe Float
y = Just 6.0


