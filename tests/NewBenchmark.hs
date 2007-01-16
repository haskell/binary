{-# OPTIONS -fbang-patterns #-}
--
-- benchmark NewBinary
--

module Main where

import System.IO
import Data.Word
import NewBinary
import qualified Data.ByteString.Lazy as L

import Text.Printf
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
    printf "%0.4f\n" (diff :: Double)
    return v

test label f n fs s = do
    h <- openBinMem (1024 * 1024 * 10) undefined
    time label $ doN n fs s f h

doN 0 _ _ _  _ = return ()
doN !n !f !s !body !h = do
    body h s
    doN (n-1) f (f s) body h

putWord8 h w    = put h (w :: Word8)
putWord16be h w = put h (w :: Word16)
putWord32be h w = put h (w :: Word32)
putWord64be h w = put h (w :: Word64)

word8  = test "Word8  10MB" putWord8    10000000 (+1) 0
word16 = test "Word16 10MB" putWord16be  5000000 (+1) 0
word32 = test "Word32 10MB" putWord32be  2500000 (+1) 0
word64 = test "Word64 10MB" putWord64be  1250000 (+1) 0

