{-# LANGUAGE DeriveGeneric #-}

import Control.DeepSeq
import Control.Exception (evaluate)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Test.Tasty.Bench

import GHC.Generics

import Data.Binary
import Data.Binary.Put
import Data.ByteString.Builder as BB
import Prelude -- Silence Monoid import warning.

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

    , bgroup "Generics"
      [ bench "Struct monoid put" $ whnf (run . foldMap put) structs
      , bench "Struct put as list" $ whnf (run . put) structs
      , bench "StructList monoid put" $ whnf (run . foldMap put) structLists
      , bench "StructList put as list" $ whnf (run . put) structLists
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
