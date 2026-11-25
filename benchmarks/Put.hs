{-# LANGUAGE DeriveGeneric #-}

import Control.DeepSeq
import Control.Exception (evaluate)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Fixed (Fixed, E2)
import Data.Functor.Identity (Identity(..))
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word
import Test.Tasty.Bench

import GHC.Generics

import Data.Binary
import Data.Binary.Put
import Data.ByteString.Builder as BB

main :: IO ()
main = do
  evaluate $ rnf
    [ rnf bigIntegers
    , rnf smallIntegers
    , rnf smallByteStrings
    , rnf smallStrings
    , rnf doubles
    , rnf word8s
    , rnf word16s
    , rnf word32s
    , rnf word64s
    , rnf words10k
    , rnf ints10k
    , rnf intSet10k
    , rnf set10k
    , rnf intMap10k
    , rnf map10k
    , rnf seq10k
    , rnf sumInts10k
    , rnf identityW32s10k
    , rnf fixedE2s10k
    ]
  defaultMain
    [ bench "small Integers" $ whnf (run . foldMap put) smallIntegers
    , bench "[small Integer]" $ whnf (run . put) smallIntegers

    , bench "big Integers" $ whnf (run . foldMap put) bigIntegers
    , bench "[big Integer]" $ whnf (run . put) bigIntegers

    , bench "small ByteStrings" $ whnf (run . foldMap put) smallByteStrings
    , bench "[small ByteString]" $ whnf (run . put) smallByteStrings

    , bench "small Strings" $ whnf (run . foldMap put) smallStrings
    , bench "[small String]" $ whnf (run . put) smallStrings

    , bench "Double" $ whnf (run . put) doubles

    , bench "Word8s monoid put" $ whnf (run . foldMap put) word8s
    , bench "Word8s builder" $ whnf (L.length . toLazyByteString . foldMap BB.word8) word8s
    , bench "[Word8]" $ whnf (run . put) word8s
    , bench "Word16s monoid put" $ whnf (run . foldMap put) word16s
    , bench "Word16s builder" $ whnf (L.length . toLazyByteString . foldMap BB.word16BE) word16s
    , bench "[Word16]" $ whnf (run . put) word16s
    , bench "Word32s monoid put" $ whnf (run . foldMap put) word32s
    , bench "Word32s builder" $ whnf (L.length . toLazyByteString . foldMap BB.word32BE) word32s
    , bench "[Word32]" $ whnf (run . put) word32s
    , bench "Word64s monoid put" $ whnf (run . foldMap put) word64s
    , bench "Word64s builder" $ whnf (L.length . toLazyByteString . foldMap BB.word64BE) word64s
    , bench "[Word64]" $ whnf (run . put) word64s
    , bench "[Word]" $ whnf (run . put) words10k
    , bench "[Int]" $ whnf (run . put) ints10k

    , bgroup "Generics"
      [ bench "Struct monoid put" $ whnf (run . foldMap put) structs
      , bench "Struct put as list" $ whnf (run . put) structs
      , bench "StructList monoid put" $ whnf (run . foldMap put) structLists
      , bench "StructList put as list" $ whnf (run . put) structLists
      ]
    , bgroup "Containers"
      [ bench "Set Int (10k)" $ whnf (run . put) set10k
      , bench "Map Int Word32 (10k)" $ whnf (run . put) map10k
      , bench "IntSet (10k)" $ whnf (run . put) intSet10k
      , bench "IntMap Word32 (10k)" $ whnf (run . put) intMap10k
      , bench "Seq Int (10k)" $ whnf (run . put) seq10k
      ]
    , bgroup "Wrappers (10k)"
      [ bench "[Sum Int]" $ whnf (run . put) sumInts10k
      , bench "[Identity Word32]" $ whnf (run . put) identityW32s10k
      , bench "[Fixed E2]" $ whnf (run . put) fixedE2s10k
      ]
    ]
  where
    run = L.length . runPut

data Struct = Struct Word8 Word16 Word32 Word64 deriving Generic
instance Binary Struct

data StructList = StructList [Struct] deriving Generic
instance Binary StructList

structs :: [Struct]
structs = take 10000 $ [ Struct a b 0 0 | a <- [0 .. maxBound], b <- [0 .. maxBound] ]

structLists :: [StructList]
structLists = replicate 1000 (StructList (take 10 structs))

-- Input data

smallIntegers :: [Integer]
smallIntegers = [0..10000]
{-# NOINLINE smallIntegers #-}

bigIntegers :: [Integer]
bigIntegers = [m .. m + 10000]
  where
    m :: Integer
    m = fromIntegral (maxBound :: Word64)
{-# NOINLINE bigIntegers #-}

smallByteStrings :: [S.ByteString]
smallByteStrings = replicate 10000 $ C.pack "abcdefghi"
{-# NOINLINE smallByteStrings #-}

smallStrings :: [String]
smallStrings = replicate 10000 "abcdefghi"
{-# NOINLINE smallStrings #-}

doubles :: [Double]
doubles = take 10000 $ [ sign * 2 ** n | sign <- [-1, 1], n <- [ 0, 0.2 .. 1023 ]]

word8s :: [Word8]
word8s = take 10000 $ cycle [minBound .. maxBound]
{-# NOINLINE word8s #-}

word16s :: [Word16]
word16s = take 10000 $ cycle [minBound .. maxBound]
{-# NOINLINE word16s #-}

word32s :: [Word32]
word32s = take 10000 $ cycle [minBound .. maxBound]
{-# NOINLINE word32s #-}

word64s :: [Word64]
word64s = take 10000 $ cycle [minBound .. maxBound]
{-# NOINLINE word64s #-}

words10k :: [Word]
words10k = [1..10000]
{-# NOINLINE words10k #-}

ints10k :: [Int]
ints10k = [1..10000]
{-# NOINLINE ints10k #-}

set10k :: Set.Set Int
set10k = Set.fromList [1..10000]
{-# NOINLINE set10k #-}

map10k :: Map.Map Int Word32
map10k = Map.fromList [(i, fromIntegral i) | i <- [1..10000]]
{-# NOINLINE map10k #-}

intSet10k :: IntSet.IntSet
intSet10k = IntSet.fromList [1..10000]
{-# NOINLINE intSet10k #-}

intMap10k :: IntMap.IntMap Word32
intMap10k = IntMap.fromList [(i, fromIntegral i) | i <- [1..10000]]
{-# NOINLINE intMap10k #-}

seq10k :: Seq.Seq Int
seq10k = Seq.fromList [1..10000]
{-# NOINLINE seq10k #-}

sumInts10k :: [Monoid.Sum Int]
sumInts10k = [Monoid.Sum i | i <- [1..10000]]
{-# NOINLINE sumInts10k #-}

identityW32s10k :: [Identity Word32]
identityW32s10k = [Identity (fromIntegral i) | i <- [1..10000 :: Int]]
{-# NOINLINE identityW32s10k #-}

fixedE2s10k :: [Fixed E2]
fixedE2s10k = [fromIntegral i | i <- [1..10000 :: Int]]
{-# NOINLINE fixedE2s10k #-}
