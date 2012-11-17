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

import Data.Serialization
import Data.Serialization.Postgresql

import GHC.Generics

import Model
import Model.Case (Case)
import qualified Model.Case as Case
import Model.Partner (Partner)
import qualified Model.Partner as Partner

psqlCon :: P.ConnectInfo
psqlCon = P.defaultConnectInfo {
    P.connectUser = "carma_db_sync",
    P.connectPassword = "pass",
    P.connectDatabase = "carma" }

printModel :: JsonedModel m Patch => m Patch -> IO ()
printModel = either (const $ return ()) C8.putStrLn . encodeJSON

main :: IO ()
main = do
    rcon <- R.connect R.defaultConnectInfo
    pcon <- P.connect psqlCon
    putStrLn "Input source (psql or redis), model and id:"
    forever $ do
        [src, mdl, i] <- fmap words getLine
        case lookup (src, mdl) $ cmds pcon rcon of
            Nothing -> putStrLn "Invalid command"
            Just act -> act i
    where
        psqlModel :: (Model m, JsonedModel m Patch, PgsedModel m Patch) => P.Connection -> String -> IO [m Patch]
        psqlModel con i = do
            vs <- select con " where id = ?" (P.Only i)
            mapM_ printModel vs
            return vs
        redisModel :: (Model m, JsonedModel m Patch, RedisedModel m Patch) => R.Connection -> String -> String -> IO (Maybe (m Patch))
        redisModel con mdl i = do
            (Right x) <- R.runRedis con $ R.hgetall (fromString $ mdl ++ ":" ++ i)
            case decodeRedis $ M.fromList x of
                Left e -> do
                    putStrLn e
                    return Nothing
                Right v -> do
                    printModel v
                    return $ Just v
        cmds :: P.Connection -> R.Connection -> [((String, String), (String -> IO ()))]
        cmds pcon rcon = [
            (("psql", "case"), \i -> void (psqlModel pcon i :: IO [Case Patch])),
            (("psql", "partner"), \i -> void (psqlModel pcon i :: IO [Partner Patch])),
            (("redis", "case"), \i -> void (redisModel rcon "case" i :: IO (Maybe (Case Patch)))),
            (("redis", "partner"), \i -> void (redisModel rcon "partner" i :: IO (Maybe (Partner Patch))))]
