{-# LANGUAGE CPP, OverloadedStrings, ExistentialQuantification, BangPatterns #-}

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Main (main) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main hiding (run)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Word (Word8, Word16, Word32)

import Control.Applicative
import Data.Binary.Get
import Data.Binary ( get )

instance NFData S.ByteString

main :: IO ()
main = do
  evaluate $ rnf [
#if defined(ALTERNATIVE)
    -- rnf brackets,
#endif
    rnf oneMegabyte
    -- rnf oneMegabyteLBS]
     ]
  defaultMain
    [
#if defined(ALTERNATIVE)
      bench "brackets 100k" $ whnf (runTest bracketParser) brackets,
#endif
      bench "getStruct4 1MB struct of 4 word32 strict" $
        whnf (runTest (getStruct4Strict mega)) oneMegabyteLBS
    , bench "getStruct4 1MB struct of 4 word32" $
        whnf (runTest (getStruct4 mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 1 byte" $
        whnf (runTest (getWord8N1 mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 2 bytes" $
        whnf (runTest (getWord8N2 mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 4 bytes" $
        whnf (runTest (getWord8N4 mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 8 bytes" $
        whnf (runTest (getWord8N8 mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 16 bytes" $
        whnf (runTest (getWord8N16 mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 2 bytes Applicative" $
        whnf (runTest (getWord8N2A mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 4 bytes Applicative" $
        whnf (runTest (getWord8N4A mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 8 bytes Applicative" $
        whnf (runTest (getWord8N8A mega)) oneMegabyteLBS
    , bench "getWord8 1MB chunk size 16 bytes Applicative" $
        whnf (runTest (getWord8N16A mega)) oneMegabyteLBS
    ]

runTest parser inp = runGet parser inp

-- Defs.

oneMegabyte :: S.ByteString
oneMegabyte = S.replicate mega $ fromIntegral $ ord 'a'

oneMegabyteLBS :: L.ByteString
oneMegabyteLBS = L.fromChunks [oneMegabyte]

mega = 1024 * 1024

-- 100k of brackets
#if defined(ALTERNATIVE)
bracketTest inp = runTest bracketParser inp

brackets = L.fromChunks [C8.concat (replicate 1024 "((()((()()))((()(()()()()()()()(((()()()()(()()(()(()())))))()((())())))()())(((())())(()))))()(()))")]

bracketParser = cont <|> end
  where
  end = return 0
  cont = do v <- some ( do 40 <- getWord8
                           n <- bracketParser
                           41 <- getWord8
                           return $! n + 1)
            return $! sum v
#endif

-- Struct strict

data Struct4S = Struct4S !Word32 !Word32 !Word32 !Word32

instance NFData Struct4S where
  rnf (Struct4S !a !b !c !d) = ()

getStruct4Strict = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- get
          !w1 <- get
          !w2 <- get
          !w3 <- get
          loop (Struct4S w0 w1 w2 w3 : acc) (n - 16)

-- Struct lazy

data Struct4 = Struct4 Word32 Word32 Word32 Word32

instance NFData Struct4 where
  rnf (Struct4 !a !b !c !d) = ()

getStruct4 = loop []
  where loop acc 0 = return acc
        loop acc n = do
          w0 <- get
          w1 <- get
          w2 <- get
          w3 <- get
          loop (Struct4 w0 w1 w2 w3 : acc) (n - 16)

-- No-allocation loops.

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

getWord8N2A = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          v <- (+) <$> getWord8 <*> getWord8
          loop (s+v) (n-2)

getWord8N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          loop (s+s0+s1+s2+s3) (n-4)

getWord8N4A = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          let p !s0 !s1 !s2 !s3 = s0 + s1 + s2 + s3
          v <- p <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
          loop (s+v) (n-4)

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

getWord8N8A = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          let p !s0 !s1 !s2 !s3 !s4 !s5 !s6 !s7 =
                s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7
          v <- p <$> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
          loop (s+v) (n-8)

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
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15) (n-16)

getWord8N16A = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          let p !s0 !s1 !s2 !s3 !s4 !s5 !s6 !s7 !s8 !s9 !s10 !s11 !s12 !s13 !s14 !s15 =
                s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15
          !v <- p <$> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
          loop (s+v) (n-16)
