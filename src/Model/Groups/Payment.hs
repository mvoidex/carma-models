{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.Payment (
    Payment(..)
    ) where

import Data.Text (Text)

import Model.Base

import GHC.Generics

data Payment (k :: FieldKind) = Payment {
    expectedCost :: Field k Int,
    costTranscript :: Field k Int,
    partnerCost :: Field k Int,
    calculatedCost :: Field k Int,
    limitedCost :: Field k Int,
    overcosted :: Field k Bool,
    paidByRUAMC :: Field k Text,
    paidByClient :: Field k Text }
        deriving (Generic)

instance Model Payment
