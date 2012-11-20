module Model (
    Action,
    Call,
    Case,
    Partner,

    module Model.Base,
    module Model.Serialization
    ) where

import Model.Base
import Model.Serialization

import Model.Action (Action)
import Model.Call (Call)
import Model.Case (Case)
import Model.Partner (Partner)
