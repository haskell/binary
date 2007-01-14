module Main where

import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put

import Control.Exception
import System.CPUTime


main :: IO ()
main = do
    word8
    word16
    word32
    word64

time :: String -> IO a -> IO a
time label f = do
    putStr (label ++ " ")
    start <- getCPUTime
    v     <- f
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    print (diff :: Double)
    return v

test label f n fs s = time label $ do
    let bs = runPut (doN n fs s f)
    evaluate (L.length bs)
    return ()

doN 0 _ _ _ = return ()
doN !n !f !s !body = do
    body s
    doN (n-1) f (f s) body

word8  = test "Word8  1MB" putWord8    1000000 (+1) 0
word16 = test "Word16 1MB" putWord16be  500000 (+1) 0
word32 = test "Word32 1MB" putWord32be  250000 (+1) 0
word64 = test "Word64 1MB" putWord64be  125000 (+1) 0

