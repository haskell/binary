module Main where

import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put

import Control.Exception
import System.CPUTime
import Numeric

mb :: Int
mb = 10

main :: IO ()
main = sequence_ 
  [ test wordSize chunkSize mb
  | wordSize  <- [1,2,4,8]
  , chunkSize <- [1,2,4,8,16] ]

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
        iterations = bytes `div` wordSize
        len = L.length $ runPut $ go wordSize chunkSize iterations
    putStr $ show mb ++ "MB of Word" ++ show (8 * wordSize)
          ++ " in chunks of " ++ show chunkSize ++ ": "
    seconds <- time $ evaluate len
    --print len
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
    (2, 1)  -> word16N1
    (2, 2)  -> word16N2
    (2, 4)  -> word16N4
    (2, 8)  -> word16N8
    (2, 16) -> word16N16
    (4, 1)  -> word32N1
    (4, 2)  -> word32N2
    (4, 4)  -> word32N4
    (4, 8)  -> word32N8
    (4, 16) -> word32N16
    (8, 1)  -> word64N1
    (8, 2)  -> word64N2
    (8, 4)  -> word64N4
    (8, 8)  -> word64N8
    (8, 16) -> word64N16

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


word16N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          loop (s+1) (n-1)

word16N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          loop (s+2) (n-2)

word16N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          loop (s+4) (n-4)

word16N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          putWord16be (s+4)
          putWord16be (s+5)
          putWord16be (s+6)
          putWord16be (s+7)
          loop (s+8) (n-8)

word16N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          putWord16be (s+4)
          putWord16be (s+5)
          putWord16be (s+6)
          putWord16be (s+7)
          putWord16be (s+8)
          putWord16be (s+9)
          putWord16be (s+10)
          putWord16be (s+11)
          putWord16be (s+12)
          putWord16be (s+13)
          putWord16be (s+14)
          putWord16be (s+15)
          loop (s+16) (n-16)


word32N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          loop (s+1) (n-1)

word32N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          loop (s+2) (n-2)

word32N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          loop (s+4) (n-4)

word32N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          putWord32be (s+4)
          putWord32be (s+5)
          putWord32be (s+6)
          putWord32be (s+7)
          loop (s+8) (n-8)

word32N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          putWord32be (s+4)
          putWord32be (s+5)
          putWord32be (s+6)
          putWord32be (s+7)
          putWord32be (s+8)
          putWord32be (s+9)
          putWord32be (s+10)
          putWord32be (s+11)
          putWord32be (s+12)
          putWord32be (s+13)
          putWord32be (s+14)
          putWord32be (s+15)
          loop (s+16) (n-16)

word64N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          loop (s+1) (n-1)

word64N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          loop (s+2) (n-2)

word64N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          loop (s+4) (n-4)

word64N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          putWord64be (s+4)
          putWord64be (s+5)
          putWord64be (s+6)
          putWord64be (s+7)
          loop (s+8) (n-8)

word64N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          putWord64be (s+4)
          putWord64be (s+5)
          putWord64be (s+6)
          putWord64be (s+7)
          putWord64be (s+8)
          putWord64be (s+9)
          putWord64be (s+10)
          putWord64be (s+11)
          putWord64be (s+12)
          putWord64be (s+13)
          putWord64be (s+14)
          putWord64be (s+15)
          loop (s+16) (n-16)
