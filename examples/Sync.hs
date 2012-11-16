{-# LANGUAGE OverloadedStrings, DataKinds, ConstraintKinds, FlexibleContexts #-}

module Main (
    main
    ) where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import Data.String

import qualified Database.Redis as R
import qualified Database.PostgreSQL.Simple as P

import Data.Serialization.Postgresql

import System.Environment

import Model
import Model.Case (Case)
import qualified Model.Case as Case

psqlCon :: P.ConnectInfo
psqlCon = P.defaultConnectInfo {
    P.connectUser = "carma_db_sync",
    P.connectPassword = "pass",
    P.connectDatabase = "carma" }

main :: IO ()
main = do
    args@(~([i])) <- getArgs
    if length args /= 1
        then putStrLn "Usage: sync <id>"
        else do
            rcon <- R.connect R.defaultConnectInfo
            pcon <- P.connect psqlCon
            (Right x) <- R.runRedis rcon $ R.hgetall (fromString $ "case:" ++ i)
            case decodeRedis (M.fromList x) of
                Left e -> putStrLn e
                Right cas -> void $ insert pcon (cas :: Case Object)
