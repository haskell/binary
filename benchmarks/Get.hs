{-# LANGUAGE CPP, OverloadedStrings, ExistentialQuantification, BangPatterns #-}

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Main
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Word (Word8, Word16, Word32)

import Control.Applicative
import Data.Binary.Get
import Data.Binary ( get )

import qualified Data.Serialize.Get as Cereal
import qualified Data.Serialize as Cereal

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as AL

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData S.ByteString
instance NFData L.ByteString where
  rnf = rnf . L.toChunks
#endif

main :: IO ()
main = do
  evaluate $ rnf [
    rnf brackets,
    rnf bracketsInChunks,
    rnf bracketCount,
    rnf oneMegabyte,
    rnf oneMegabyteLBS
     ]
  defaultMain
    [
      bench "brackets 100kb one chunk input" $
        whnf (checkBracket . runTest bracketParser) brackets
    , bench "brackets 100kb in 100 byte chunks" $
        whnf (checkBracket . runTest bracketParser) bracketsInChunks
    , bench "Attoparsec lazy-bs brackets 100kb one chunk" $
        whnf (checkBracket . runAttoL bracketParser_atto) brackets
    , bench "Attoparsec lazy-bs brackets 100kb in 100 byte chunks" $
        whnf (checkBracket . runAttoL bracketParser_atto) bracketsInChunks
    , bench "Attoparsec strict-bs brackets 100kb" $
        whnf (checkBracket . runAtto bracketParser_atto) $ S.concat (L.toChunks brackets)
    , bench "Cereal strict-bs brackets 100kb" $
        whnf (checkBracket . runCereal bracketParser_cereal) $ S.concat (L.toChunks brackets)
    , bench "Binary getStruct4 1MB struct of 4 word8" $
        whnf (runTest (getStruct4 mega)) oneMegabyteLBS
    , bench "Cereal getStruct4 1MB struct of 4 word8" $
        whnf (runCereal (getStruct4_cereal mega)) oneMegabyte
    , bench "Attoparsec getStruct4 1MB struct of 4 word8" $
        whnf (runAtto (getStruct4_atto mega)) oneMegabyte
    , bench "Binary getWord8 1MB chunk size 1 byte" $
        whnf (runTest (getWord8N1 mega)) oneMegabyteLBS
    , bench "Cereal getWord8 1MB chunk size 1 byte" $
        whnf (runCereal (getWord8N1_cereal mega)) oneMegabyte
    , bench "Attoparsec getWord8 1MB chunk size 1 byte" $
        whnf (runAtto (getWord8N1_atto mega)) oneMegabyte
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

checkBracket x | x == bracketCount = x
               | otherwise = error "argh!"

runTest decoder inp = runGet decoder inp
runCereal decoder inp = case Cereal.runGet decoder inp of
                          Right a -> a
                          Left err -> error err
runAtto decoder inp = case A.parseOnly decoder inp of
                        Right a -> a
                        Left err -> error err
runAttoL decoder inp = case AL.parse decoder inp of
                        AL.Done _ r -> r
                        a -> error (show a)

-- Defs.

oneMegabyte :: S.ByteString
oneMegabyte = S.replicate mega $ fromIntegral $ ord 'a'

oneMegabyteLBS :: L.ByteString
oneMegabyteLBS = L.fromChunks [oneMegabyte]

mega = 1024 * 1024

-- 100k of brackets
bracketTest inp = runTest bracketParser inp

bracketCount :: Int
bracketCount = fromIntegral $ L.length brackets `div` 2

brackets = L.fromChunks [C8.concat (L.toChunks bracketsInChunks)]
bracketsInChunks = L.fromChunks (replicate chunksOfBrackets oneChunk)
  where
    oneChunk = "((()((()()))((()(()()()()()()()(((()()()()(()()(()(()())))))()((())())))()())(((())())(()))))()(()))"
    chunksOfBrackets = 102400 `div` S.length oneChunk

bracketParser :: Get Int
bracketParser = cont <|> return 0
  where
  cont = do v <- some ( do 40 <- getWord8
                           n <- many cont
                           41 <- getWord8
                           return $! sum n + 1)
            return $! sum v

bracketParser_cereal :: Cereal.Get Int
bracketParser_cereal = cont <|> return 0
  where
  cont = do v <- some ( do 40 <- Cereal.getWord8
                           n <- many cont
                           41 <- Cereal.getWord8
                           return $! sum n + 1)
            return $! sum v

bracketParser_atto :: A.Parser Int
bracketParser_atto = cont <|> return 0
  where
  cont = do v <- some ( do A.word8 40
                           n <- bracketParser_atto
                           A.word8 41
                           return $! n + 1)
            return $! sum v

-- Strict struct of 4 Word8s
data Struct4 = Struct4 {-# UNPACK #-} !Word8
                       {-# UNPACK #-} !Word8
                       {-# UNPACK #-} !Word8
                       {-# UNPACK #-} !Word8
               deriving Show

getStruct4 = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- getWord8
          !w1 <- getWord8
          !w2 <- getWord8
          !w3 <- getWord8
          let !s = Struct4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getStruct4_cereal = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- Cereal.getWord8
          !w1 <- Cereal.getWord8
          !w2 <- Cereal.getWord8
          !w3 <- Cereal.getWord8
          let !s = Struct4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getStruct4_atto = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- A.anyWord8
          !w1 <- A.anyWord8
          !w2 <- A.anyWord8
          !w3 <- A.anyWord8
          let !s = Struct4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

-- No-allocation loops.

getWord8N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          loop (s0+s) (n-1)

getWord8N1_cereal = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- Cereal.getWord8
          loop (s0+s) (n-1)

getWord8N1_atto = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- A.anyWord8
          loop (s0+s) (n-1)

getWord8N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          loop (s0+s1+s) (n-2)

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
