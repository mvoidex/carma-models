{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.Car (
    Car(..)
    ) where

import Data.Text (Text)
import Data.Time

import Model.Base

import GHC.Generics

data Car (k :: FieldKind) = Car {
    vin :: Field k Text,
    seller :: Field k Text,
    make :: Field k Text,
    model :: Field k Text,
    plateNum :: Field k Text,
    makeYear :: Field k Int,
    color :: Field k Text,
    buyDate :: Field k LocalTime,
    checkupDate :: Field k LocalTime,
    dealerTO :: Field k Text,
    mileage :: Field k Int,
    checkupMileage :: Field k Int,
    transmission :: Field k Text,
    engine :: Field k Text,
    liters :: Field k Int,
    capacity :: Field k Int,
    dims :: Field k Text,
    weight :: Field k Int,
    checkPeriod :: Field k Int,
    cclass :: Field k Text,
    makeCode :: Field k Int,
    modelCode :: Field k Int,
    faultCode :: Field k Int }
        deriving (Generic)

instance Model Car
