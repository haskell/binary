module Main where

import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

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
        bs  = runPut (doPut wordSize chunkSize iterations)
        sum = runGet (doGet wordSize chunkSize iterations) bs
    putStr $ show mb ++ "MB of Word" ++ show (8 * wordSize)
          ++ " in chunks of " ++ show chunkSize ++ ": "
    putSeconds <- time $ evaluate (L.length bs)
    getSeconds <- time $ evaluate sum
--    print (L.length bs, sum)
    let putThroughput = fromIntegral mb / putSeconds
        getThroughput = fromIntegral mb / getSeconds
    putStrLn $ showFFloat (Just 2) putThroughput "MB/s write, "
            ++ showFFloat (Just 2) getThroughput "MB/s read"

doPut :: Int -> Int -> Int -> Put
doPut wordSize chunkSize =
  case (wordSize, chunkSize) of
    (1, 1)  -> putWord8N1
    (1, 2)  -> putWord8N2
    (1, 4)  -> putWord8N4
    (1, 8)  -> putWord8N8
    (1, 16) -> putWord8N16
    (2, 1)  -> putWord16N1
    (2, 2)  -> putWord16N2
    (2, 4)  -> putWord16N4
    (2, 8)  -> putWord16N8
    (2, 16) -> putWord16N16
    (4, 1)  -> putWord32N1
    (4, 2)  -> putWord32N2
    (4, 4)  -> putWord32N4
    (4, 8)  -> putWord32N8
    (4, 16) -> putWord32N16
    (8, 1)  -> putWord64N1
    (8, 2)  -> putWord64N2
    (8, 4)  -> putWord64N4
    (8, 8)  -> putWord64N8
    (8, 16) -> putWord64N16

putWord8N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          loop (s+1) (n-1)

putWord8N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          loop (s+2) (n-2)

putWord8N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          loop (s+4) (n-4)

putWord8N8 = loop 0
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

putWord8N16 = loop 0
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


putWord16N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          loop (s+1) (n-1)

putWord16N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          loop (s+2) (n-2)

putWord16N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          loop (s+4) (n-4)

putWord16N8 = loop 0
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

putWord16N16 = loop 0
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


putWord32N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          loop (s+1) (n-1)

putWord32N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          loop (s+2) (n-2)

putWord32N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          loop (s+4) (n-4)

putWord32N8 = loop 0
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

putWord32N16 = loop 0
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

putWord64N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          loop (s+1) (n-1)

putWord64N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          loop (s+2) (n-2)

putWord64N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          loop (s+4) (n-4)

putWord64N8 = loop 0
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

putWord64N16 = loop 0
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


doGet :: Int -> Int -> Int -> Get Int
doGet wordSize chunkSize =
  case (wordSize, chunkSize) of
    (1, 1)  -> fmap fromIntegral . getWord8N1
    (1, 2)  -> fmap fromIntegral . getWord8N2
    (1, 4)  -> fmap fromIntegral . getWord8N4
    (1, 8)  -> fmap fromIntegral . getWord8N8
    (1, 16) -> fmap fromIntegral . getWord8N16
    (2, 1)  -> fmap fromIntegral . getWord16N1
    (2, 2)  -> fmap fromIntegral . getWord16N2
    (2, 4)  -> fmap fromIntegral . getWord16N4
    (2, 8)  -> fmap fromIntegral . getWord16N8
    (2, 16) -> fmap fromIntegral . getWord16N16
    (4, 1)  -> fmap fromIntegral . getWord32N1
    (4, 2)  -> fmap fromIntegral . getWord32N2
    (4, 4)  -> fmap fromIntegral . getWord32N4
    (4, 8)  -> fmap fromIntegral . getWord32N8
    (4, 16) -> fmap fromIntegral . getWord32N16
    (8, 1)  -> fmap fromIntegral . getWord64N1
    (8, 2)  -> fmap fromIntegral . getWord64N2
    (8, 4)  -> fmap fromIntegral . getWord64N4
    (8, 8)  -> fmap fromIntegral . getWord64N8
    (8, 16) -> fmap fromIntegral . getWord64N16

getWord8N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          loop (s+s0) (n-1)

getWord8N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          loop (s+s0+s1) (n-2)

getWord8N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          loop (s+s0+s1+s2+s3) (n-4)

getWord8N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord8N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          s8 <- getWord8
          s9 <- getWord8
          s10 <- getWord8
          s11 <- getWord8
          s12 <- getWord8
          s13 <- getWord8
          s14 <- getWord8
          s15 <- getWord8
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)


getWord16N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          loop (s+s0) (n-1)

getWord16N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          loop (s+s0+s1) (n-2)

getWord16N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          s2 <- getWord16be
          s3 <- getWord16be
          loop (s+s0+s1+s2+s3) (n-4)

getWord16N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          s2 <- getWord16be
          s3 <- getWord16be
          s4 <- getWord16be
          s5 <- getWord16be
          s6 <- getWord16be
          s7 <- getWord16be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord16N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          s2 <- getWord16be
          s3 <- getWord16be
          s4 <- getWord16be
          s5 <- getWord16be
          s6 <- getWord16be
          s7 <- getWord16be
          s8 <- getWord16be
          s9 <- getWord16be
          s10 <- getWord16be
          s11 <- getWord16be
          s12 <- getWord16be
          s13 <- getWord16be
          s14 <- getWord16be
          s15 <- getWord16be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)


getWord32N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          loop (s+s0) (n-1)

getWord32N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          loop (s+s0+s1) (n-2)

getWord32N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          s2 <- getWord32be
          s3 <- getWord32be
          loop (s+s0+s1+s2+s3) (n-4)

getWord32N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          s2 <- getWord32be
          s3 <- getWord32be
          s4 <- getWord32be
          s5 <- getWord32be
          s6 <- getWord32be
          s7 <- getWord32be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord32N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          s2 <- getWord32be
          s3 <- getWord32be
          s4 <- getWord32be
          s5 <- getWord32be
          s6 <- getWord32be
          s7 <- getWord32be
          s8 <- getWord32be
          s9 <- getWord32be
          s10 <- getWord32be
          s11 <- getWord32be
          s12 <- getWord32be
          s13 <- getWord32be
          s14 <- getWord32be
          s15 <- getWord32be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

getWord64N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          loop (s+s0) (n-1)

getWord64N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          loop (s+s0+s1) (n-2)

getWord64N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          s2 <- getWord64be
          s3 <- getWord64be
          loop (s+s0+s1+s2+s3) (n-4)

getWord64N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          s2 <- getWord64be
          s3 <- getWord64be
          s4 <- getWord64be
          s5 <- getWord64be
          s6 <- getWord64be
          s7 <- getWord64be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord64N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          s2 <- getWord64be
          s3 <- getWord64be
          s4 <- getWord64be
          s5 <- getWord64be
          s6 <- getWord64be
          s7 <- getWord64be
          s8 <- getWord64be
          s9 <- getWord64be
          s10 <- getWord64be
          s11 <- getWord64be
          s12 <- getWord64be
          s13 <- getWord64be
          s14 <- getWord64be
          s15 <- getWord64be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)
