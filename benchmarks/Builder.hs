{-# LANGUAGE ExistentialQuantification #-}
module Main (main) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main hiding (run)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Word (Word8)

import Data.Binary.Builder

instance NFData S.ByteString

data B = forall a. NFData a => B a

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = defaultMainWith defaultConfig
    (liftIO . evaluate $ rnf [B word8s, B smallByteString, B largeByteString])
    [ -- Test GHC loop optimization of continuation based code.
      bench "[Word8]" $ whnf (run . fromWord8s) word8s

      -- Test bounds check merging
    , bench "bounds/[Word8]" $ whnf (run . from4Word8s) word8s

    , bench "small ByteString" $ whnf (run . fromByteString) smallByteString
    , bench "large ByteString" $ whnf (run . fromByteString) largeByteString
    ]
  where
    run = L.length . toLazyByteString

-- Input data

word8s :: [Word8]
word8s = replicate 10000 $ fromIntegral $ ord 'a'
{-# NOINLINE word8s #-}

smallByteString :: S.ByteString
smallByteString = C.pack "abcdefghi"

largeByteString :: S.ByteString
largeByteString = S.pack word8s

-- Benchmarks

fromWord8s :: [Word8] -> Builder
fromWord8s [] = mempty
fromWord8s (x:xs) = singleton x <> fromWord8s xs

from4Word8s :: [Word8] -> Builder
from4Word8s [] = mempty
from4Word8s (x:xs) = singleton x <> singleton x <> singleton x <> singleton x <>
                     from4Word8s xs

-- Utilities

infixr 6 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
