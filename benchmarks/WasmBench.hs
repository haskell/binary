{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Codec.Compression.GZip as GZip
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Binary as Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Test.Tasty.Bench

import Wasm

rechunk :: Int -> B.ByteString -> L.ByteString
rechunk n = L.fromChunks . go
  where
    go bs
      | B.null bs = []
      | otherwise = let (h, t) = B.splitAt n bs in h : go t

main :: IO ()
main = do
  compressed <- L.readFile "benchmarks/zstd.wasm.gz"
  let !rawStrict   = L.toStrict (GZip.decompress compressed)
      !rawOneChunk = L.fromStrict rawStrict
      !raw4KB      = rechunk 4096 rawStrict
      !raw64B      = rechunk 64 rawStrict
      !wasmMod     = runGet getModule rawOneChunk
      !reencoded   = runPut (putModule wasmMod)
      !genEncoded  = L.fromStrict (L.toStrict (Binary.encode wasmMod))

  -- Correctness checks before benchmarking.
  -- Note: wasm-ld emits 5-byte padded LEB128 section/body lengths, whereas
  -- putModule emits minimal canonical LEB128; verify AST round-trip and
  -- canonical byte-for-byte round-trip.
  let !wasmMod2 = runGet getModule reencoded
  unless (wasmMod2 == wasmMod) $
    error "WasmBench: AST round-trip failed!"
  unless (runPut (putModule wasmMod2) == reencoded) $
    error "WasmBench: canonical byte-for-byte round-trip failed!"
  unless (runGet getModule raw64B == wasmMod) $
    error "WasmBench: chunked decode mismatch!"
  unless (Binary.decode genEncoded == wasmMod) $
    error "WasmBench: Generic Binary round-trip mismatch!"

  evaluate $ rnf [rnf rawOneChunk, rnf raw4KB, rnf raw64B, rnf wasmMod, rnf genEncoded]

  defaultMain
    [ bgroup "wasm/decode"
        [ bench "full, one chunk (90KB)"     $ nf (runGet getModule) rawOneChunk
        , bench "full, 4KB chunks (90KB)"    $ nf (runGet getModule) raw4KB
        , bench "full, 64B chunks (90KB)"    $ nf (runGet getModule) raw64B
        , bench "shallow, skip code (90KB)"  $ nf (runGet getModuleShallow) rawOneChunk
        ]
    , bgroup "wasm/encode"
        [ bench "full (90KB)" $ whnf (L.length . runPut . putModule) wasmMod
        ]
    , bgroup "wasm/generic"
        [ bench "encode Module" $ whnf (L.length . Binary.encode) wasmMod
        , bench "decode Module" $ nf (Binary.decode :: L.ByteString -> Module) genEncoded
        ]
    ]
