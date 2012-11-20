{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.Times (
    Times(..)
    ) where

import Data.Text (Text)
import Data.Time

import Model.Base

import GHC.Generics

data Times (k :: FieldKind) = Times {
    expectedServiceStart :: Field k LocalTime,
    factServiceStart :: Field k LocalTime,
    expectedServiceEnd :: Field k LocalTime,
    factServiceEnd :: Field k LocalTime,
    expectedDealerInfo :: Field k LocalTime,
    factDealerInfo :: Field k LocalTime,
    expectedServiceClosure :: Field k LocalTime,
    factServiceClosure :: Field k LocalTime }
        deriving (Generic)

instance Model Times
