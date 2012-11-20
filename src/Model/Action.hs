{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Action (
    Action(..)
    ) where

import GHC.Generics

import Data.Text (Text)
import Data.Time

import Model.Base

data Action (k :: FieldKind) = Action {
    id :: Field k Int,
    parentId :: Field k Text,
    caseId :: Field k Text,
    name :: Field k Text,
    description :: Field k Text,
    duetime :: Field k LocalTime,
    comment :: Field k Text,
    result :: Field k Text,
    ctime :: Field k LocalTime,
    mtime :: Field k LocalTime,
    openTime :: Field k LocalTime,
    closeTime :: Field k LocalTime,
    assignedTo :: Field k Text,
    targetGroup :: Field k Text,
    priority :: Field k Text,
    closed :: Field k Bool }
        deriving (Generic)

instance Model Action
instance ModelTable Action where
    modelTable _ = "actiontbl"
