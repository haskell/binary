{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq

import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)

import Control.Monad
import Foreign
import System.Environment
import System.IO

import Test.QuickCheck
import QuickCheckUtils
import Text.Printf

roundTrip :: (Eq a, Binary a) => a -> Bool
roundTrip a = a == decode (encode a)

roundTripWith put get x = x == runGet get (runPut (put x))

-- low level ones:

prop_Word16be = roundTripWith putWord16be getWord16be
prop_Word16le = roundTripWith putWord16le getWord16le

prop_Word32be = roundTripWith putWord32be getWord32be
prop_Word32le = roundTripWith putWord32le getWord32le

prop_Word64be = roundTripWith putWord64be getWord64be
prop_Word64le = roundTripWith putWord64le getWord64le

-- be lazy!

-- doesn't do fair testing of lazy put/get.
-- tons of untested cases

lazyTrip :: (Binary a, Eq a) => a -> Bool
lazyTrip a = a == (runGet lazyGet . runPut . lazyPut $ a)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    run tests

run :: [(String, Int -> IO ())] -> IO ()
run tests = do
    x <- getArgs
    let n = if null x then 300 else read . head $ x
    mapM_ (\(s,a) -> printf "%-50s" s >> a n) tests

tests =
        [ ("Word16be", mytest prop_Word16be)
        , ("Word16le", mytest prop_Word16le)
        , ("Word32be", mytest prop_Word32be)
        , ("Word32le", mytest prop_Word32le)
        , ("Word64be", mytest prop_Word64be)
        , ("Word64le", mytest prop_Word64le)
-- higher level ones using the Binary class
        ,("()",     mytest (roundTrip :: () -> Bool))
        ,("Bool",   mytest (roundTrip :: Bool -> Bool))
        ,("Char",   mytest (roundTrip :: Char -> Bool))
        ,("Int",    mytest (roundTrip :: Int -> Bool))
        ,("Word8",  mytest (roundTrip :: Word8 -> Bool))
        ,("Word16", mytest (roundTrip :: Word16 -> Bool))
        ,("Word32", mytest (roundTrip :: Word32 -> Bool))
        ,("Word64", mytest (roundTrip :: Word64 -> Bool))
        ,("Int8",   mytest (roundTrip :: Int8 -> Bool))
        ,("Int16",  mytest (roundTrip :: Int16 -> Bool))
        ,("Int32",  mytest (roundTrip :: Int32 -> Bool))
        ,("Int64",  mytest (roundTrip :: Int64 -> Bool))
        ,("[Word8]", mytest (roundTrip :: [Word8] -> Bool))
        ,("String",  mytest (roundTrip :: String -> Bool))
        ,("Maybe Int8", mytest (roundTrip :: Maybe Int8 -> Bool))
        ,("Either Int8 Int16", mytest (roundTrip :: Either Int8 Int16 -> Bool))
        ,("(Int32, [Int])", mytest (roundTrip :: (Int32, [Int]) -> Bool))
        ,("(Maybe Int64, Bool, [Int])", mytest (roundTrip :: (Maybe Int64, Bool, [Int]) -> Bool))
        ,("(Maybe Word8, Bool, [Int], Either Bool Word8)", mytest (roundTrip :: (Maybe Word8, Bool, [Int], Either Bool Word8) -> Bool))
        ,("lazy IntMap", mytest (lazyTrip :: IntSet.IntSet -> Bool))
{-        ,("(Maybe Word16, Bool, [Int], Either Bool Word16, Int)", mytest (roundTrip :: (Maybe Word16, Bool, [Int], Either Bool Word16, Int) -> Bool))
        ,("(Maybe Word32, Bool, [Int], Either Bool Word32, Int, Int)", mytest (roundTrip :: (Maybe Word32, Bool, [Int], Either Bool Word32, Int, Int) -> Bool))
        ,("(Maybe Word64, Bool, [Int], Either Bool Word64, Int, Int, Int)", mytest (roundTrip :: (Maybe Word64, Bool, [Int], Either Bool Word64, Int, Int, Int) -> Bool))-}
        ,("B.ByteString",  mytest (roundTrip :: B.ByteString -> Bool))
        ,("L.ByteString",  mytest (roundTrip :: L.ByteString -> Bool))
        ,("IntSet",        mytest (roundTrip :: IntSet.IntSet -> Bool))
        ,("IntMap String", mytest (roundTrip :: IntMap.IntMap String -> Bool))
        ,("Set Word32",      mytest (roundTrip :: Set.Set Word32 -> Bool))
        ,("Map Word16 Int",  mytest (roundTrip :: Map.Map Word16 Int -> Bool))
        ,("Sequence", mytest (roundTrip :: Seq.Seq Int64 -> Bool))
        ,("Integer" , mytest (roundTrip :: Integer -> Bool))
        ]
