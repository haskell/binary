{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Codec.Compression.GZip as GZip
import Control.Applicative ((<|>))
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.Binary as Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8)
import Test.Tasty.Bench

import Bencode

rechunk :: Int -> B.ByteString -> L.ByteString
rechunk n = L.fromChunks . go
  where
    go bs
      | B.null bs = []
      | otherwise = let (h, t) = B.splitAt n bs in h : go t

------------------------------------------------------------------------
-- Attoparsec Bencode parser (<|> + many' + <?>) for direct comparison

attoSignedDec :: Word8 -> A.Parser Int64
attoSignedDec term = do
  b <- A.anyWord8
  if b == 45
    then negate <$> go 0 True
    else goDigit b 0
  where
    go !acc !first = do
      b <- A.anyWord8
      if b == term
        then if first then fail "empty integer" else return acc
        else goDigit b acc
    goDigit b !acc
      | b >= 48 && b <= 57 = go (acc * 10 + fromIntegral (b - 48)) False
      | otherwise          = fail "expected digit"

attoRawBytes :: A.Parser B.ByteString
attoRawBytes = do
  b <- A.anyWord8
  if b >= 48 && b <= 57
    then go (fromIntegral (b - 48)) >>= A.take
    else fail "expected digit"
  where
    go !acc = do
      b <- A.anyWord8
      if b == 58 -- ':'
        then return acc
        else if b >= 48 && b <= 57
          then go (acc * 10 + fromIntegral (b - 48))
          else fail "expected digit"

attoBVal :: A.Parser BVal
attoBVal =
      ((A.word8 105 *> (BInt <$> attoSignedDec 101)) A.<?> "BInt")
  <|> ((A.word8 108 *> (BList <$> A.many' attoBVal) <* A.word8 101) A.<?> "BList")
  <|> ((A.word8 100 *> (BDict <$> A.many' ((,) <$> attoRawBytes <*> attoBVal)) <* A.word8 101) A.<?> "BDict")
  <|> ((BBytes <$> attoRawBytes) A.<?> "BBytes")

runAttoL :: AL.Parser a -> L.ByteString -> a
runAttoL p lbs = case AL.parseOnly p lbs of
  Right a  -> a
  Left err -> error err

------------------------------------------------------------------------

main :: IO ()
main = do
  compressed <- L.readFile "benchmarks/sample.bencode.gz"
  let !rawStrict   = L.toStrict (GZip.decompress compressed)
      !rawOneChunk = L.fromStrict rawStrict
      !raw64B      = rechunk 64 rawStrict
      !valDirect   = runGet getBValDirect rawOneChunk
      !valAlt      = runGet getBValAlt rawOneChunk
      !valLA       = runGet getBValLookAhead rawOneChunk
      !valAtto     = runAttoL attoBVal rawOneChunk
      !reencoded   = runPut (putBVal valDirect)
      !genEncoded  = L.fromStrict (L.toStrict (Binary.encode valDirect))

  -- Correctness checks before benchmarking
  unless (reencoded == rawOneChunk) $
    error "BencodeBench: byte-for-byte round-trip failed!"
  unless (valAlt == valDirect) $
    error "BencodeBench: Alt decoder mismatch!"
  unless (valLA == valDirect) $
    error "BencodeBench: LookAhead decoder mismatch!"
  unless (valAtto == valDirect) $
    error "BencodeBench: Attoparsec decoder mismatch!"
  unless (runGet getBValAlt raw64B == valDirect) $
    error "BencodeBench: chunked Alt decode mismatch!"
  unless (runGet getBValLookAhead raw64B == valDirect) $
    error "BencodeBench: chunked LookAhead decode mismatch!"
  unless (Binary.decode genEncoded == valDirect) $
    error "BencodeBench: Generic Binary round-trip mismatch!"

  evaluate $ rnf [rnf rawOneChunk, rnf raw64B, rnf valDirect, rnf genEncoded]

  defaultMain
    [ bgroup "bencode/decode (one chunk)"
        [ bench "binary: direct"                $ nf (runGet getBValDirect) rawOneChunk
        , bench "binary: lookAhead"             $ nf (runGet getBValLookAhead) rawOneChunk
        , bench "binary: <|> + many + label"    $ nf (runGet getBValAlt) rawOneChunk
        , bench "attoparsec: <|> + many' + <?>" $ nf (runAttoL attoBVal) rawOneChunk
        ]
    , bgroup "bencode/decode (64B chunks)"
        [ bench "binary: direct"                $ nf (runGet getBValDirect) raw64B
        , bench "binary: lookAhead"             $ nf (runGet getBValLookAhead) raw64B
        , bench "binary: <|> + many + label"    $ nf (runGet getBValAlt) raw64B
        , bench "attoparsec: <|> + many' + <?>" $ nf (runAttoL attoBVal) raw64B
        ]
    , bgroup "bencode/encode"
        [ bench "putBVal" $ whnf (L.length . runPut . putBVal) valDirect
        ]
    , bgroup "bencode/generic"
        [ bench "encode BVal" $ whnf (L.length . Binary.encode) valDirect
        , bench "decode BVal" $ nf (Binary.decode :: L.ByteString -> BVal) genEncoded
        ]
    ]
