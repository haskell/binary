{-# OPTIONS -fbang-patterns #-}
module Main where

import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put

import Control.Exception
import System.CPUTime
import Numeric

main :: IO ()
main = sequence_ 
  [ test 1 n 10
  | n <- [1,2,4,8,16] ]

time :: IO a -> IO Double
time action = do
    start <- getCPUTime
    action
    end   <- getCPUTime
    return $! (fromIntegral (end - start)) / (10^12)

test :: Int -> Int -> Int -> IO ()
test wordSize chunkSize mb = do
    let iterations :: Int
        iterations = mb * 2^20
        bs = runPut $ go wordSize chunkSize iterations
    putStr $ show mb ++ "MB of Word" ++ show (8 * wordSize)
          ++ " in chunks of " ++ show chunkSize ++ ": "
    seconds <- time $ evaluate (L.length bs)
    let throughput = fromIntegral mb / seconds
    putStrLn $ showFFloat (Just 2) throughput "MB/s"

go :: Int -> Int -> Int -> Put
go wordSize chunkSize =
  case (wordSize, chunkSize) of
    (1, 1)  -> word8N1
    (1, 2)  -> word8N2
    (1, 4)  -> word8N4
    (1, 8)  -> word8N8
    (1, 16) -> word8N16

word8N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          loop (s+1) (n-1)

word8N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          loop (s+2) (n-2)

word8N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          loop (s+4) (n-4)

word8N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          putWord8 (s+4)
          putWord8 (s+5)
          putWord8 (s+6)
          putWord8 (s+7)
          loop (s+8) (n-8)

word8N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          putWord8 (s+4)
          putWord8 (s+5)
          putWord8 (s+6)
          putWord8 (s+7)
          putWord8 (s+8)
          putWord8 (s+9)
          putWord8 (s+10)
          putWord8 (s+11)
          putWord8 (s+12)
          putWord8 (s+13)
          putWord8 (s+14)
          putWord8 (s+15)
          loop (s+16) (n-16)
