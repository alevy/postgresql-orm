
module Database.PostgreSQL.ORM (
  -- * The Model class and related types
    Model(modelInfo), ModelInfo(..)
  , defaultModelInfo, underscoreModelInfo
  , DBKey(..), DBRef, DBRefUnique, mkDBRef
  , (:.), As(..), RowAlias(..), fromAs, toAs
  -- ** Single-row operations
  , findRow, save, destroy, destroyByRef
  -- * Abstracted select queries
  , DBSelect, modelDBSelect, dbSelectParams, dbSelect
  , addWhere_, addWhere, setOrderBy, setLimit, setOffset
  -- * Associations between models
  , Association, assocSelect, assocProject, assocWhere, findAssoc
  -- ** Parent-child associations
  , GDBRefInfo(..), DBRefInfo, defaultDBRefInfo, dbrefAssocs, has, belongsTo
  -- ** Join table associations
  , JoinTable(..), defaultJoinTable, jtAssocs, jtAdd, jtRemove, jtRemoveById
  -- ** Chaining associations
  , nestAssoc, chainAssoc
  ) where

import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.ORM.DBSelect
import Database.PostgreSQL.ORM.Association
import Database.PostgreSQL.Simple ((:.))
