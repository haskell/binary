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

import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)

import Control.Monad
import Foreign
import System.Environment
import System.IO

import Test.QuickCheck hiding (test)
import QuickCheckUtils
import Text.Printf

-- import qualified Data.Sequence as Seq

------------------------------------------------------------------------

roundTrip :: (Eq a, Binary a) => a -> Bool
roundTrip a = a ==
    {-# SCC "decode.encode" #-} decode (encode a)

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
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-50s" s >> a n) tests

------------------------------------------------------------------------

type T a = a -> Bool

p       :: Testable a => a -> Int -> IO ()
p       = mytest

test    :: (Eq a, Binary a) => a -> Bool
test    = roundTrip

tests =
-- Primitives
        [ ("Word16be",      p prop_Word16be)
        , ("Word16le",      p prop_Word16le)
        , ("Word32be",      p prop_Word32be)
        , ("Word32le",      p prop_Word32le)
        , ("Word64be",      p prop_Word64be)
        , ("Word64le",      p prop_Word64le)

-- higher level ones using the Binary class
        ,("()",         p (test :: T ()                     ))
        ,("Bool",       p (test :: T Bool                   ))
        ,("Ordering",   p (test :: T Ordering               ))

        ,("Word8",      p (test :: T Word8                  ))
        ,("Word16",     p (test :: T Word16                 ))
        ,("Word32",     p (test :: T Word32                 ))
        ,("Word64",     p (test :: T Word64                 ))

        ,("Int8",       p (test :: T Int8                   ))
        ,("Int16",      p (test :: T Int16                  ))
        ,("Int32",      p (test :: T Int32                  ))
        ,("Int64",      p (test :: T Int64                  ))

        ,("Word",       p (test :: T Word                   ))
        ,("Int",        p (test :: T Int                    ))
        ,("Integer",    p (test :: T Integer                ))

        ,("Char",       p (test :: T Char                   ))

        ,("[()]",       p (test :: T [()]                  ))
        ,("[Word8]",    p (test :: T [Word8]               ))
        ,("[Word32]",   p (test :: T [Word32]              ))
        ,("[Word64]",   p (test :: T [Word64]              ))
        ,("[Word]",     p (test :: T [Word]                ))
        ,("[Int]",      p (test :: T [Int]                 ))
        ,("[Integer]",  p (test :: T [Integer]             ))
        ,("String",     p (test :: T String                ))

        ,("((), ())",           p (test :: T ((), ())        ))
        ,("(Word8, Word32)",    p (test :: T (Word8, Word32) ))
        ,("(Int8, Int32)",      p (test :: T (Int8,  Int32)  ))
        ,("(Int32, [Int])",     p (test :: T (Int32, [Int])  ))

        ,("Maybe Int8",         p (test :: T (Maybe Int8)        ))
        ,("Either Int8 Int16",  p (test :: T (Either Int8 Int16) ))

        ,("(Maybe Word8, Bool, [Int], Either Bool Word8)",
                p (test :: T (Maybe Word8, Bool, [Int], Either Bool Word8) ))

        ,("Lazy IntMap",   p (lazyTrip  :: T IntSet.IntSet          ))
        ,("IntSet",        p (test      :: T IntSet.IntSet          ))
        ,("IntMap ByteString", p (test      :: T (IntMap.IntMap B.ByteString) ))

        ,("B.ByteString",  p (test :: T B.ByteString        ))
        ,("L.ByteString",  p (test :: T L.ByteString        ))

        ,("Set Word32",      p (test :: T (Set.Set Word32)      ))
        ,("Map Word16 Int",  p (test :: T (Map.Map Word16 Int)  ))

        ,("(Maybe Int64, Bool, [Int])", p (roundTrip :: (Maybe Int64, Bool, [Int]) -> Bool))

{-
--
-- Big tuples lack an Arbitrary instance in Hugs/QuickCheck
--

        ,("(Maybe Word16, Bool, [Int], Either Bool Word16, Int)",
            p (test :: T (Maybe Word16, Bool, [Int], Either Bool Word16, Int) ))

        ,("(Maybe Word32, Bool, [Int], Either Bool Word32, Int, Int)", p (roundTrip :: (Maybe Word32, Bool, [Int], Either Bool Word32, Int, Int) -> Bool))

        ,("(Maybe Word64, Bool, [Int], Either Bool Word64, Int, Int, Int)", p (roundTrip :: (Maybe Word64, Bool, [Int], Either Bool Word64, Int, Int, Int) -> Bool))
-}

-- GHC only:
--      ,("Sequence", p (roundTrip :: Seq.Seq Int64 -> Bool))

-- Obsolete
--      ,("ensureLeft/Fail", mytest (shouldFail (decode L.empty :: Either ParseError Int)))
        ]
