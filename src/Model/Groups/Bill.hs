{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.Bill (
    Bill(..)
    ) where

import Data.Text (Text)
import Data.Time

import Model.Base

import GHC.Generics

data Bill (k :: FieldKind) = Bill {
    billNumber :: Field k Int,
    billingCost :: Field k Int,
    billingDate :: Field k LocalTime }
        deriving (Generic)

instance Model Bill
