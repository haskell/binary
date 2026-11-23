{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Self-contained Bencode (BitTorrent metadata) decoder and encoder.
--
-- Provides three decoding styles in "Data.Binary.Get" to benchmark
-- @(\<|\>)@, @many@, @lookAhead@, and @label@ against a direct LL(1)
-- decoder (and against @attoparsec@ in "Main"):
--
--   * 'getBValAlt'       — idiomatic @(\<|\>)@ + @many@ + @label@
--   * 'getBValLookAhead' — @lookAhead getWord8@ dispatch
--   * 'getBValDirect'    — direct @getWord8@ threading (fast baseline)
module Bencode
  ( BVal(..)
  , getBValAlt
  , getBValLookAhead
  , getBValDirect
  , putBVal
  ) where

import Control.Applicative ((<|>), many)
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Binary (Binary)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Int (Int64)
import Data.Word (Word8)
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- Bencode AST

data BVal
  = BInt !Int64
  | BBytes !B.ByteString
  | BList ![BVal]
  | BDict ![(B.ByteString, BVal)]
  deriving (Eq, Show, Generic)

instance NFData BVal
instance Binary BVal

------------------------------------------------------------------------
-- Helpers

word8 :: Word8 -> Get ()
word8 expected = do
  b <- getWord8
  when (b /= expected) $ fail ("expected byte " ++ show expected)
{-# INLINE word8 #-}

getSignedDec :: Word8 -> Get Int64
getSignedDec term = do
  b <- getWord8
  if b == 45 -- '-'
    then negate <$> go 0 True
    else goDigit b 0
  where
    go !acc !first = do
      b <- getWord8
      if b == term
        then if first then fail "empty integer" else return acc
        else goDigit b acc
    goDigit b !acc
      | b >= 48 && b <= 57 = go (acc * 10 + fromIntegral (b - 48)) False
      | otherwise          = fail ("expected digit, got " ++ show b)
{-# INLINE getSignedDec #-}

getUnsignedDecFrom :: Word8 -> Word8 -> Get Int
getUnsignedDecFrom firstByte term
  | firstByte >= 48 && firstByte <= 57 = go (fromIntegral (firstByte - 48))
  | otherwise = fail ("expected digit, got " ++ show firstByte)
  where
    go !acc = do
      b <- getWord8
      if b == term
        then return acc
        else if b >= 48 && b <= 57
          then go (acc * 10 + fromIntegral (b - 48))
          else fail ("expected digit, got " ++ show b)
{-# INLINE getUnsignedDecFrom #-}

------------------------------------------------------------------------
-- 1. Idiomatic (<|>) + many + label style

getBValAlt :: Get BVal
getBValAlt =
      label "BInt"   getBIntAlt
  <|> label "BList"  getBListAlt
  <|> label "BDict"  getBDictAlt
  <|> label "BBytes" getBBytesAlt

getBIntAlt :: Get BVal
getBIntAlt = do
  word8 105 -- 'i'
  BInt <$> getSignedDec 101 -- 'e'

getBListAlt :: Get BVal
getBListAlt = do
  word8 108 -- 'l'
  xs <- many getBValAlt
  word8 101 -- 'e'
  return (BList xs)

getBDictAlt :: Get BVal
getBDictAlt = do
  word8 100 -- 'd'
  kvs <- many ((,) <$> getRawBytesAlt <*> getBValAlt)
  word8 101 -- 'e'
  return (BDict kvs)

getRawBytesAlt :: Get B.ByteString
getRawBytesAlt = do
  b <- getWord8
  len <- getUnsignedDecFrom b 58 -- ':'
  getByteString len

getBBytesAlt :: Get BVal
getBBytesAlt = BBytes <$> getRawBytesAlt

------------------------------------------------------------------------
-- 2. lookAhead style

getBValLookAhead :: Get BVal
getBValLookAhead = do
  b <- lookAhead getWord8
  case b of
    105 -> do -- 'i'
      _ <- getWord8
      BInt <$> getSignedDec 101
    108 -> do -- 'l'
      _ <- getWord8
      BList <$> goList []
    100 -> do -- 'd'
      _ <- getWord8
      BDict <$> goDict []
    _ -> BBytes <$> getRawBytesAlt
  where
    goList acc = do
      b <- lookAhead getWord8
      if b == 101 -- 'e'
        then getWord8 >> return (reverse acc)
        else do
          !v <- getBValLookAhead
          goList (v : acc)
    goDict acc = do
      b <- lookAhead getWord8
      if b == 101 -- 'e'
        then getWord8 >> return (reverse acc)
        else do
          !k <- getRawBytesAlt
          !v <- getBValLookAhead
          goDict ((k, v) : acc)

------------------------------------------------------------------------
-- 3. Direct getWord8 style (baseline without <|> or lookAhead)

getBValDirect :: Get BVal
getBValDirect = getWord8 >>= getBValFrom

getBValFrom :: Word8 -> Get BVal
getBValFrom b = case b of
  105 -> BInt <$> getSignedDec 101 -- 'i'
  108 -> BList <$> goList []       -- 'l'
  100 -> BDict <$> goDict []       -- 'd'
  _   -> BBytes <$> (getUnsignedDecFrom b 58 >>= getByteString)
  where
    goList acc = do
      c <- getWord8
      if c == 101 -- 'e'
        then return $! reverse acc
        else do
          !v <- getBValFrom c
          goList (v : acc)
    goDict acc = do
      c <- getWord8
      if c == 101 -- 'e'
        then return $! reverse acc
        else do
          !len <- getUnsignedDecFrom c 58
          !k <- getByteString len
          !v <- getBValDirect
          goDict ((k, v) : acc)

------------------------------------------------------------------------
-- Encoder

putBVal :: BVal -> Put
putBVal (BInt n) =
  putWord8 105 <> putByteString (C8.pack (show n)) <> putWord8 101
putBVal (BBytes bs) =
  putByteString (C8.pack (show (B.length bs))) <> putWord8 58 <> putByteString bs
putBVal (BList xs) =
  putWord8 108 <> foldMap putBVal xs <> putWord8 101
putBVal (BDict kvs) =
  putWord8 100
    <> foldMap (\(k, v) -> putBVal (BBytes k) <> putBVal v) kvs
    <> putWord8 101
