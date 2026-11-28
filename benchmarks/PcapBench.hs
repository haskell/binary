{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Codec.Compression.GZip as GZip
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (unless)
import System.IO.Error (catchIOError)
import qualified Data.Binary as Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Test.Tasty.Bench

import Pcap

rechunk :: Int -> B.ByteString -> L.ByteString
rechunk n = L.fromChunks . go
  where
    go bs
      | B.null bs = []
      | otherwise = let (h, t) = B.splitAt n bs in h : go t

main :: IO ()
main = do
  compressed <- L.readFile "benchmarks/sample.pcap.gz"
    `catchIOError` (\_ -> L.readFile "../benchmarks/sample.pcap.gz")
  let !rawStrict   = L.toStrict (GZip.decompress compressed)
      !rawOneChunk = L.fromStrict rawStrict
      !raw4KB      = rechunk 4096 rawStrict
      !raw64B      = rechunk 64 rawStrict
      !pcap        = runGet getPcapFile rawOneChunk
      !reencoded   = runPut (putPcapFile pcap)
      !genEncoded  = L.fromStrict (L.toStrict (Binary.encode pcap))

  -- Correctness checks before benchmarking
  unless (reencoded == rawOneChunk) $
    error "PcapBench: byte-for-byte round-trip failed!"
  unless (runGet getPcapFile raw64B == pcap) $
    error "PcapBench: chunked decode mismatch!"
  unless (Binary.decode genEncoded == pcap) $
    error "PcapBench: Generic Binary round-trip mismatch!"

  evaluate $ rnf [rnf rawOneChunk, rnf raw4KB, rnf raw64B, rnf pcap, rnf genEncoded]

  defaultMain
    [ bgroup "pcap/decode"
        [ bench "full, one chunk (2000 pkts)"    $ nf (runGet getPcapFile) rawOneChunk
        , bench "full, 4KB chunks (2000 pkts)"   $ nf (runGet getPcapFile) raw4KB
        , bench "full, 64B chunks (2000 pkts)"   $ nf (runGet getPcapFile) raw64B
        , bench "headers only, skip payload"     $ nf (runGet getPcapHeadersOnly) rawOneChunk
        ]
    , bgroup "pcap/encode"
        [ bench "full (2000 pkts)" $ whnf (L.length . runPut . putPcapFile) pcap
        ]
    , bgroup "pcap/generic"
        [ bench "encode PcapFile" $ whnf (L.length . Binary.encode) pcap
        , bench "decode PcapFile" $ nf (Binary.decode :: L.ByteString -> PcapFile) genEncoded
        ]
    ]
