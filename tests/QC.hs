{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.ByteString as B
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
import Test.QuickCheck.Batch

roundTrip :: (Eq a, Binary a) => a -> Bool
roundTrip a = a == decode (encode a)

roundTripWith put get x = x == runGet get (runPut (put x))

instance Arbitrary Word8 where
    arbitrary = liftM fromIntegral (choose (0, 2^8-1))
    coarbitrary w = variant 0

instance Arbitrary Word16 where
    arbitrary = liftM fromIntegral (choose (0, 2^16-1))
    coarbitrary = undefined

instance Arbitrary Word32 where
    arbitrary = liftM fromIntegral (choose (0, 2^32-1))
    coarbitrary = undefined

instance Arbitrary Word64 where
    arbitrary = liftM fromIntegral (choose (0, 2^64-1))
    coarbitrary = undefined

instance Arbitrary Int8 where
    arbitrary = liftM fromIntegral (choose (0, 2^8-1))
    coarbitrary w = variant 0

instance Arbitrary Int16 where
    arbitrary = liftM fromIntegral (choose (0, 2^16-1))
    coarbitrary = undefined

instance Arbitrary Int32 where
    arbitrary = liftM fromIntegral (choose (0, 2^32-1))
    coarbitrary = undefined

instance Arbitrary Int64 where
    arbitrary = liftM fromIntegral (choose (0, 2^64-1))
    coarbitrary = undefined

instance Arbitrary Char where
    arbitrary = choose (maxBound, minBound)
    coarbitrary = undefined

instance Arbitrary a => Arbitrary (Maybe a) where
    arbitrary = oneof [ return Nothing, liftM Just arbitrary]
    coarbitrary = undefined

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = oneof [ liftM Left arbitrary, liftM Right arbitrary]
    coarbitrary = undefined

instance Arbitrary IntSet.IntSet where
    arbitrary = fmap IntSet.fromList arbitrary
    coarbitrary = undefined

instance (Arbitrary e) => Arbitrary (IntMap.IntMap e) where
    arbitrary = fmap IntMap.fromList arbitrary
    coarbitrary = undefined

instance (Arbitrary a, Ord a) => Arbitrary (Set.Set a) where
    arbitrary = fmap Set.fromList arbitrary
    coarbitrary = undefined

instance (Arbitrary a, Ord a, Arbitrary b) => Arbitrary (Map.Map a b) where
    arbitrary = fmap Map.fromList arbitrary
    coarbitrary = undefined

instance (Arbitrary a) => Arbitrary (Seq.Seq a) where
    arbitrary = fmap Seq.fromList arbitrary
    coarbitrary = undefined

instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . L.LPS . filter (not. P.null) -- maintain the invariant.
    coarbitrary s = coarbitrary (L.unpack s)

instance Arbitrary P.ByteString where
  arbitrary = P.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (P.unpack s)

-- low level ones:

prop_Word16be = roundTripWith putWord16be getWord16be
prop_Word16le = roundTripWith putWord16le getWord16le

prop_Word32be = roundTripWith putWord32be getWord32be
prop_Word32le = roundTripWith putWord32le getWord32le

prop_Word64be = roundTripWith putWord64be getWord64be
prop_Word64le = roundTripWith putWord64le getWord64le

main = do
    hSetBuffering stdout NoBuffering
    runTests "Binary" opts (map (run . uncurry label) tests)
    where
    opts = defOpt { no_of_tests = 1000, length_of_tests = 1000 }
    tests =
        [ ("Word16be", property prop_Word16be)
        , ("Word16le", property prop_Word16le)
        , ("Word32be", property prop_Word32be)
        , ("Word32le", property prop_Word32le)
        , ("Word64be", property prop_Word64be)
        , ("Word64le", property prop_Word64le)
-- higher level ones using the Binary class
        ,("()",     property (roundTrip :: () -> Bool))
        ,("Bool",   property (roundTrip :: Bool -> Bool))
        ,("Char",   property (roundTrip :: Char -> Bool))
        ,("Int",    property (roundTrip :: Int -> Bool))
        ,("Word8",  property (roundTrip :: Word8 -> Bool))
        ,("Word16", property (roundTrip :: Word16 -> Bool))
        ,("Word32", property (roundTrip :: Word32 -> Bool))
        ,("Word64", property (roundTrip :: Word64 -> Bool))
        ,("Int8",   property (roundTrip :: Int8 -> Bool))
        ,("Int16",  property (roundTrip :: Int16 -> Bool))
        ,("Int32",  property (roundTrip :: Int32 -> Bool))
        ,("Int64",  property (roundTrip :: Int64 -> Bool))
        ,("[Word8]", property (roundTrip :: [Word8] -> Bool))
        ,("String",  property (roundTrip :: String -> Bool))
        ,("Maybe Word8", property (roundTrip :: Maybe Word8 -> Bool))
        ,("Either Word8 Word16", property (roundTrip :: Either Word8 Word16 -> Bool))
        ,("(Char, [Int])", property (roundTrip :: (Char, [Int]) -> Bool))
        ,("(Maybe Char, Bool, [Int])", property (roundTrip :: (Maybe Char, Bool, [Int]) -> Bool))
        ,("(Maybe Char, Bool, [Int], Either Bool Char)", property (roundTrip :: (Maybe Char, Bool, [Int], Either Bool Char) -> Bool))
{-        ,("(Maybe Char, Bool, [Int], Either Bool Char, Int)", property (roundTrip :: (Maybe Char, Bool, [Int], Either Bool Char, Int) -> Bool))
        ,("(Maybe Char, Bool, [Int], Either Bool Char, Int, Int)", property (roundTrip :: (Maybe Char, Bool, [Int], Either Bool Char, Int, Int) -> Bool))
        ,("(Maybe Char, Bool, [Int], Either Bool Char, Int, Int, Int)", property (roundTrip :: (Maybe Char, Bool, [Int], Either Bool Char, Int, Int, Int) -> Bool))
        ,("B.ByteString",  property (roundTrip :: B.ByteString -> Bool))
        ,("L.ByteString",  property (roundTrip :: L.ByteString -> Bool))-}
        ,("IntSet",        property (roundTrip :: IntSet.IntSet -> Bool))
        ,("IntMap String", property (roundTrip :: IntMap.IntMap String -> Bool))
        ,("Set Char",      property (roundTrip :: Set.Set Char -> Bool))
        ,("Map Char Int",  property (roundTrip :: Map.Map Char Int -> Bool))
        ,("Sequence", property (roundTrip :: Seq.Seq Int64 -> Bool))
        ]
