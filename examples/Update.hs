module Update (
    ) where

import Control.Monad

import qualified Data.ByteString.Char8 as C8

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
run = run' test where
    run' :: Case -> IO ()
    run' v = do
        putStr "> "
        either putStrLn C8.putStrLn $ encodeModel v
        i <- getLine
        either onError onUpdate $ decodePartial (C8.pack i)
        where
            onError e = putStrLn e >> run' v
            onUpdate x = run' $ update v x
