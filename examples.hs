{-# LANGUAGE TypeOperators, MultiParamTypeClasses, DeriveGeneric, OverloadedStrings #-}

import Control.Exception
import Data.AsTypeOf
import qualified Data.ByteString as S
import Data.Int
import Data.Maybe
import Database.PostgreSQL.ORM.Association
import Database.PostgreSQL.ORM.CreateTable
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.Simple
import GHC.Generics

import Control.Applicative
import Database.PostgreSQL.Keywords
import System.IO.Unsafe
import Data.GetField
import Database.PostgreSQL.ORM.DBSelect

data MyType = MyType { myString :: String             -- position 0
                     , myInt :: Int                   -- position 1
                     , myBool :: Bool                 -- position 2
                     , myMaybeChar :: Maybe Char      -- position 3
                     , myMaybeString :: Maybe String  -- position 4
                     } deriving (Show, Generic)

myType :: MyType
myType = MyType "my type" 21 True Nothing (Just "maybe string")

data Foo = Foo {
    foo_key :: !DBKey
  , foo_name :: String
  , parent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)

instance Model Foo

mkFoo :: String -> Foo
mkFoo name = Foo NullKey name Nothing

data Bar = Bar {
    barId :: !DBKey
  , barNone :: !(Maybe Int32)
  , barName :: !String
  , barParent :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)
instance Model Bar where modelInfo = underscoreModelInfo "bar"

data ParentBar = ParentBar
instance RowAlias ParentBar where rowAliasName _ = "parent_bar"

toParent :: Association Bar (As ParentBar Bar)
toParent = belongsTo

toChild :: Association (As ParentBar Bar) Bar
toChild = has


mkBar :: String -> Bar
mkBar msg = Bar NullKey (Just n) msg Nothing
  where n = foldl (+) 0 $ map (toEnum . fromEnum) msg

data Joiner = Joiner {
    jkey :: !DBKey
  , jcomment :: !String
  , jfoo :: (DBRef Foo)
  , jbar :: !(Maybe (DBRef Bar))
  } deriving (Show, Generic)
instance Model Joiner

joiner :: Joiner
joiner = Joiner (DBKey 5) "join comment" (DBRef 1) Nothing

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

{-
selfjoin :: IO [Bar :. As X Bar]
selfjoin = bracket mkc close $ \c ->
  findWhere "bar.id = x.parent" c () :: IO [Bar :. As X Bar]

selfjoin' :: IO [(Bar,Bar)]
selfjoin' = bracket mkc close $ \c ->
  map (\(b1 :. b2) -> (b1, fromAs X b2)) <$>
      findWhere "bar.bar_key = X.bar_parent" c ()
-}

getOne :: (Model a) => DBKeyType -> IO a
getOne k = bracket mkc close $ \c ->
  let r = fromJust <$> findRow c (DBRef k `gAsTypeOf1` r)
  in r

data T1 = T1 deriving (Show, Generic)
instance RowAlias T1


data Author = Author {
    authorId :: DBKey
  } deriving (Show, Generic)
instance Model Author where modelInfo = underscoreModelInfo "author"

data Post = Post {
    postId :: DBKey
  , postAuthorId :: DBRef Author
  } deriving (Show, Generic)
instance Model Post where modelInfo = underscoreModelInfo "post"

data Comment = Comment {
    commentId :: DBKey
  , commentPostId :: DBRef Post
  } deriving (Show, Generic)
instance Model Comment where modelInfo = underscoreModelInfo "comment"


author_posts :: Association Author Post
post_author :: Association Post Author
(post_author, author_posts) = dbrefAssocs defaultDBRefInfo


post_comments :: Association Post Comment
post_comments = has
comment_post :: Association Comment Post
comment_post = belongsTo

comment_author :: Association Comment Author
comment_author = chainAssoc comment_post post_author

author_comments :: Association Author Comment
author_comments =  chainAssoc author_posts post_comments

{-
junk = do
  foos <- findAll c :: IO [Foo]
  bars <- findAll c :: IO [Bar]
  -- sequence $ zipWith (addJoin c) foos (drop 4 bars)
  -- sequence $ zipWith (addJoin c) foos (drop 19 bars)
  -- sequence $ zipWith (addJoin c) (drop 41 foos) bars
-}

data Quizog = Quizog {
    qId :: !DBKey
  , qNone :: !(Maybe Int32)
  , qName :: !String
  , qParent :: !(Maybe (DBRef Bar))
  , qEd :: !String
  } deriving (Show, Generic)
instance Model Quizog

quizog :: Quizog
quizog = Quizog { qId = NullKey
                , qNone = Just 3
                , qName = "Mr. Quizog to you"
                , qParent = Nothing
                , qEd = "Q.E.D."
                }

