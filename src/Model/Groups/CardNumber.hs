{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.CardNumber (
    CardNumber(..)
    ) where

import Data.Text (Text)
import Data.Time

import Model.Base

import GHC.Generics

data CardNumber (k :: FieldKind) = CardNumber {
    cardNumber :: Field k Text,
    validFrom :: Field k LocalTime,
    validUntil :: Field k LocalTime,
    validUntilMilage :: Field k Int,
    milageTO :: Field k Int,
    serviceInterval :: Field k Int,
    cardOwner :: Field k Text,
    manager :: Field k Text }
        deriving (Generic)

instance Model CardNumber
