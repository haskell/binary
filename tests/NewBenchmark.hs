{-# OPTIONS_GHC -fbang-patterns #-}
--
-- benchmark NewBinary
--

module Main where

import System.IO
import Data.Word
import NewBinary

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
    let bytes :: Int
        bytes = mb * 2^20
        iterations = bytes
    putStr $ show mb ++ "MB of Word" ++ show (8 * wordSize)
          ++ " in chunks of " ++ show chunkSize ++ ": "
    seconds <- time $ do
      h <- openBinMem bytes undefined      
      go wordSize chunkSize h iterations
    let throughput = fromIntegral mb / seconds
    putStrLn $ showFFloat (Just 2) throughput "MB/s"

go :: Int -> Int -> BinHandle -> Int -> IO ()
go wordSize chunkSize =
  case (wordSize, chunkSize) of
    (1, 1)  -> word8N1
    (1, 2)  -> word8N2
    (1, 4)  -> word8N4
    (1, 8)  -> word8N8
    (1, 16) -> word8N16

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 = put_
{-# inline putWord8 #-}

word8N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          loop (s+1) (n-1)

word8N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          loop (s+2) (n-2)

word8N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          putWord8 hnd (s+2)
          putWord8 hnd (s+3)
          loop (s+4) (n-4)

word8N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          putWord8 hnd (s+2)
          putWord8 hnd (s+3)
          putWord8 hnd (s+4)
          putWord8 hnd (s+5)
          putWord8 hnd (s+6)
          putWord8 hnd (s+7)
          loop (s+8) (n-8)

word8N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          putWord8 hnd (s+2)
          putWord8 hnd (s+3)
          putWord8 hnd (s+4)
          putWord8 hnd (s+5)
          putWord8 hnd (s+6)
          putWord8 hnd (s+7)
          putWord8 hnd (s+8)
          putWord8 hnd (s+9)
          putWord8 hnd (s+10)
          putWord8 hnd (s+11)
          putWord8 hnd (s+12)
          putWord8 hnd (s+13)
          putWord8 hnd (s+14)
          putWord8 hnd (s+15)
          loop (s+16) (n-16)
