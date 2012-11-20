{-# LANGUAGE DataKinds, ConstraintKinds, FlexibleContexts #-}

module Main (
    main
    ) where

import Control.Monad

import qualified Data.ByteString.Char8 as C8
import qualified Database.PostgreSQL.Simple as P

import Data.Serialization
import Data.Serialization.Postgresql

import Model
import Model.Base

import Model.Case (Case)
import qualified Model.Case as Case
import Model.Partner (Partner)
import qualified Model.Partner as Partner
import Model.Action (Action)
import qualified Model.Action as Action

psqlCon :: P.ConnectInfo
psqlCon = P.defaultConnectInfo {
    P.connectUser = "carma_db_sync",
    P.connectPassword = "pass",
    P.connectDatabase = "carma" }

dump :: (ModelTable m, PgsedModel m Patch, JsonedModel m Patch) => P.Connection -> Int -> Int -> IO [m Patch]
dump con f t = do
    vs <- select con " where id >= ? and id <= ?" (f, t)
    mapM_ (either (const $ return ()) C8.putStrLn . encodeJSON) vs
    return vs

main :: IO ()
main = do
    putStrLn "Inpurt model, start id and end id:"
    [mdl, from, to] <- fmap words getLine
    fromi <- readIO from
    toi <- readIO to
    con <- P.connect psqlCon
    case mdl of
        "case" -> void $ (dump con fromi toi :: IO [Case Patch])
        "partner" -> void $ (dump con fromi toi :: IO [Partner Patch])
        "action" -> void $ (dump con fromi toi :: IO [Action Patch])
        _ -> putStrLn $ "Unknown model: " ++ mdl
