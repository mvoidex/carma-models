{-# LANGUAGE OverloadedStrings #-}

module Update (
    ) where

import Control.Monad

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Model
import Model.Case

test :: Case
test = mempty

-- > {}
-- {"name":"Hello"}
-- > {"name":"Hello"}
-- {"second":12.3}
-- > {"second":12.3,"name":"Hello"}
-- {"second":122.2}
-- > {"second":122.2,"name":"Hello"}
-- {"first":12, "name":"World"}
-- > {"second":122.2,"name":"World","first":12}
run :: IO ()
run = do
    printDecl
    run' test
    where
        printDecl :: IO ()
        printDecl = mapM_ printOne $ modelInfo test where
            printOne (k, (FieldInfo desc)) = T.putStrLn $ T.concat [k, ": ", desc]
        run' :: Case -> IO ()
        run' v = do
            putStr "> "
            either putStrLn C8.putStrLn $ encodeModel v
            i <- getLine
            either onError onUpdate $ decodePartial (C8.pack i)
            where
                onError e = putStrLn e >> run' v
                onUpdate x = run' $ update v x
