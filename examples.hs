{-# LANGUAGE TypeOperators, MultiParamTypeClasses, DeriveGeneric, OverloadedStrings #-}
import Control.Exception
import Database.PostgreSQL.ORM.CreateTable
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.Relationships
import Database.PostgreSQL.Simple
import GHC.Generics
import Data.AsTypeOf
import Data.Int
import Data.Maybe

import Control.Applicative
import Database.PostgreSQL.Keywords
import System.IO.Unsafe
import Data.GetField
import Database.PostgreSQL.ORM.DBSelect

data Foo = Foo {
    foo_key :: !DBKey
  , foo_name :: String
  , parent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)

instance Model Foo
instance HasParent Foo Bar
instance HasMany Bar Foo

mkFoo :: String -> Foo
mkFoo name = Foo NullKey name Nothing

data Bar = Bar {
    barId :: !DBKey
  , barNone :: !(Maybe Int32)
  , barName :: !String
  , barParent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)

instance Model Bar where modelInfo = underscoreModelInfo "bar"

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

c :: Connection
{-# NOINLINE c #-}
c = unsafePerformIO mkc

bar' :: Bar
bar' = Bar NullKey (Just 75) "bye" Nothing

data X = X deriving (Generic)
instance RowAlias X

selfjoin :: IO [Bar :. As X Bar]
selfjoin = bracket mkc close $ \c ->
  findWhere "bar.id = x.parent" c () :: IO [Bar :. As X Bar]

selfjoin' :: IO [(Bar,Bar)]
selfjoin' = bracket mkc close $ \c ->
  map (\(b1 :. b2) -> (b1, fromAs X b2)) <$>
      findWhere "bar.bar_key = X.bar_parent" c ()

getOne :: (Model a) => DBKeyType -> IO a
getOne k = bracket mkc close $ \c ->
  let r = fromJust <$> findRow c (DBRef k `gAsTypeOf1` r)
  in r


junk = do
  foos <- findAll c :: IO [Foo]
  bars <- findAll c :: IO [Bar]
  sequence $ zipWith (addJoin c) foos (drop 4 bars)
  sequence $ zipWith (addJoin c) foos (drop 19 bars)
  sequence $ zipWith (addJoin c) (drop 41 foos) bars

data Quizog = Quizog {
    qId :: !DBKey
  , qNone :: !(Maybe Int32)
  , qName :: !String
  , qParent :: !(Maybe (DBRef Bar))
  , qEd :: !(Only String)
  } deriving (Show, Generic)

quizog :: Quizog
quizog = Quizog { qId = NullKey
                , qNone = Just 3
                , qName = "Mr. Quizog to you"
                , qParent = Nothing
                , qEd = Only "Q.E.D."
                }
