{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Call (
    Call(..)
    ) where

import GHC.Generics

import Data.Text (Text)
import Data.Time

import Model.Base

data Call (k :: FieldKind) = Call {
    id :: Field k Int,
    callDate :: Field k LocalTime,
    callTaker :: Field k Text,
    program :: Field k Text,
    wazzup :: Field k Text,
    callerName :: Field k Text,
    callerType :: Field k Text,
    city :: Field k Text,
    make :: Field k Text,
    model :: Field k Text,
    callType :: Field k Text }
        deriving (Generic)

instance Model Call
instance ModelTable Call where
    modelTable _ = "callbl"
