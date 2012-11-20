{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.VinCar (
    VinCar(..)
    ) where

import Data.Text (Text)
import Data.Time

import Model.Base

import GHC.Generics

data VinCar (k :: FieldKind) = VinCar {
    make :: Field k Text,
    model :: Field k Text,
    plateNum :: Field k Text,
    makeYear :: Field k Int,
    color :: Field k Text,
    buyDate :: Field k LocalTime,
    checkupDate :: Field k LocalTime }
        deriving (Generic)

instance Model VinCar
