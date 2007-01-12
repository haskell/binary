module Main where

import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put

import Control.Exception
import System.CPUTime


main :: IO ()
main = do
    word8

time :: String -> IO a -> IO a
time label f = do
    putStr (label ++ " ")
    start <- getCPUTime
    v     <- f
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    print (diff :: Double)
    return v

word8 :: IO ()
word8 = time "Word8" $ do
    let bs = runPut (doN 1000000 (+ 1) 0 putWord8)
    evaluate (L.length bs)
    return ()

doN 0 _ _ _ = return ()
doN !n !f !s !body = do
    body s
    doN (n-1) f (f s) body
{-# INLINE doN #-}

