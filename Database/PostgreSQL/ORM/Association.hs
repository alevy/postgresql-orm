{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Database.PostgreSQL.ORM.Association where

import qualified Data.ByteString as S
import Data.Function
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import GHC.Generics

import Data.AsTypeOf
import Data.GetField
import Database.PostgreSQL.ORM.DBSelect
import Database.PostgreSQL.ORM.Model

newtype TrivParam = TrivParam [Action] deriving (Show)
instance ToRow TrivParam where
  toRow (TrivParam actions) = actions

-- | How to find an @a@ given some predicate on @b@s ('assocSelect'),
-- or a particular @b@ ('assocQuery' and 'assocParam').
data Association a b = Association {
    assocSelect :: !(DBSelect (LookupRow a))
    -- ^ General select returning all fields of @a@ from the join
    -- relation between @a@ and @b@.
  , assocQuery :: !Query
    -- ^ An optimized 'Query' to find all the 'a's associated with a
    -- particular @b@.  This can often be done more efficiently than
    -- through 'assocSelect'.
  , assocParam :: !(b -> TrivParam)
    -- ^ The query parameters for the query returned by 'assocQuery'.
  }

instance Show (Association a b) where
  show assoc = "Association " ++ show (assocSelect assoc) ++ " " ++
               show (assocQuery assoc) ++ " ?"

data ExtractRef a = ExtractRef deriving (Show)
instance Extractor ExtractRef (GDBRef rt a) (DBRef a) THasOne where
  extract _ (DBRef k) = THasOne $ DBRef k
instance Extractor ExtractRef (Maybe (GDBRef rt a)) (DBRef a) THasOne where
  extract _ (Just (DBRef k)) = THasOne $ DBRef k

refAssoc :: (Model child, Model parent
           , GetField ExtractRef child (DBRef parent)) =>
           (Association child parent, Association parent child)
refAssoc = (c_p, p_c)
  where idp = modelIdentifiers `gAsTypeOf1` c_p
        idc = modelIdentifiers `gAsTypeOf1` p_c
        extractor = (const ExtractRef :: g p -> ExtractRef (DBRef p)) c_p
        ccol = modelQColumns idc !! getFieldPos extractor (undef1 p_c)
        mkSel :: (Model a) => ModelIdentifiers a -> DBSelect (LookupRow a)
        mkSel ids = fix $ \r -> (modelDBSelect `asTypeOf` r) {
            selFields = Query $ S.intercalate ",  " $ modelQColumns ids
          , selFrom = Query $ "FROM " <> modelQTable idp
          , selJoinOp = "JOIN"
          , selJoinTo = Query $ modelQTable idc
          , selOn = Query $ " ON " <> modelQPrimaryColumn idp <> " = " <> ccol
          }
        c_p = Association {
            assocSelect = mkSel idc
          , assocQuery = Query $ modelSelectFragment idc <>
                         " WHERE " <> ccol <> " = ?"
          , assocParam = \p -> TrivParam [toField $ primaryKey p]
          }
        p_c = Association {
            assocSelect = mkSel idp
          , assocQuery = defaultModelLookupQuery idp
          , assocParam = \c -> TrivParam [toField $ getFieldVal extractor c]
          }

has :: (Model a, Model b, GetField ExtractRef b (DBRef a)) => Association a b
has = snd refAssoc

belongsTo :: (Model a, Model b, GetField ExtractRef b (DBRef a)) =>
             Association b a
belongsTo = fst refAssoc


{-
chainAssoc :: Association a b -> Association b c -> Association a (b :. c)
chainAssoc ab bc = Association {
    assocSelect = (assocSelect ab) { selFrom = newFrom }
  , assocQuery = assocQuery ab
  , assocParam = \(b :. _) -> assocParam ab b
  }
  where newFrom =
-}
                   


data Quizog = Quizog {
    qId :: !DBKey
  , qNone :: !(Maybe Int32)
  , qName :: !String
  , qEd :: !String
  } deriving (Show, Generic)
instance Model Quizog

data RefTest = RefTest { rtId :: DBKey
                       , rtRef :: (Maybe (DBRefUnique Quizog))
                       } deriving (Show, Generic)
instance Model RefTest

refTest :: RefTest
refTest = RefTest NullKey (Just (DBRef 0))

xx :: Association Quizog RefTest
xx = has
