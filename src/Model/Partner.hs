{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module Model.Partner (
    Partner(..)
    ) where

import GHC.Generics

import Data.Text (Text)

import Model.Base

data Partner (k :: FieldKind) = Partner {
    id :: Field k Int,
    isActive :: Field k Bool,
    isDealer :: Field k Bool,
    isMobile :: Field k Bool,
    name :: Field k Text,
    code :: Field k Int,
    city :: Field k Text,
    addrDeJure :: Field k Text,
    addrDeFacto :: Field k Text,
    workingTime :: Field k Text,
    phone1 :: Field k Text,
    fax :: Field k Text,
    closeTicketPhone :: Field k Text,
    closeTicketEmail :: Field k Text,
    personInCharge :: Field k Text,
    makers :: Field k Text,
    serviceAddress :: Field k Text,
    servicePhone :: Field k Text,
    serviceWorking :: Field k Text,
    salesAddress :: Field k Text,
    salesPhone :: Field k Text,
    salesWorking :: Field k Text,
    taxScheme :: Field k Text,
    comment :: Field k Text,
    isPayBackConfirmed :: Field k Bool,
    services :: Field k Text }
        deriving (Generic)

instance Model Partner
instance ModelTable Partner where
    modelTable _ = "partnertbl"
