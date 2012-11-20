{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module Model.Case (
    Case(..)
    ) where

import GHC.Generics

import Data.Text (Text)
import Data.Time

import Model.Base
import Model.Groups

-- | Case modeld
data Case (k :: FieldKind) = Case {
    id :: Field k Int,
    callDate :: Field k LocalTime,
    callTaker :: Field k Text,
    comment :: Field k Text,
    diagnosis1 :: Field k Text,
    diagnosis2 :: Field k Text,
    diagnosis3 :: Field k Text,
    diagnosis4 :: Field k Text,
    contact :: Group (CarContact k),
    program :: Field k Text,
    car :: Group (Car k),
    cardNumber :: Group (CardNumber k),
    vinChecked :: Field k Text,
    caseAddress :: Group (Address k),
    city :: Field k Text,
    temperature :: Field k Text,
    dealerCause :: Field k Text,
    caseStatus :: Field k Text,
    claim :: Field k Text,
    betaComment :: Field k Text,
    services :: Field k Text,
    actions :: Field k Text,
    files :: Field k Text,
    comments :: Field k Text }
        deriving (Generic)

instance Model Case
instance ModelTable Case where
    modelTable _ = "casetbl"
