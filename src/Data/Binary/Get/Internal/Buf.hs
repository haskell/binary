{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash, UnboxedTuples #-}
#endif
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get.Internal.Buf
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Internal buffer representation threaded through 'Data.Binary.Get.Internal.Get'.
--
-- On GHC (unless compiled with @-DBINARY_PORTABLE_GET@), the current strict
-- 'B.ByteString' chunk is unboxed as @(\# Addr\#, Int\#, ForeignPtrContents \#)@,
-- avoiding 'B.ByteString' re-boxing on every read. On other compilers (such as
-- MicroHs) or when @-DBINARY_PORTABLE_GET@ is defined, 'Buf' is a plain
-- 'B.ByteString'.
--
-- Both implementations are compiled and exported so the test suite can verify
-- them directly and differentially in a single run.
--
-- This is an internal module and is not part of the public API.
-----------------------------------------------------------------------------

module Data.Binary.Get.Internal.Buf
    ( -- * Active buffer representation
      Buf
    , packBuf
    , unpackBuf
    , bufLength
    , bufNull
    , bufHasN
    , bufTake
    , bufDrop
    , bufReadWith

      -- * Portable implementation (exported for testing)
    , PortableBuf
    , packPortableBuf
    , unpackPortableBuf
    , portableBufLength
    , portableBufNull
    , portableBufHasN
    , portableBufTake
    , portableBufDrop
    , portableBufReadWith

#ifdef __GLASGOW_HASKELL__
      -- * Unboxed GHC implementation (exported for testing)
    , UnboxedBuf
    , packUnboxedBuf
    , unpackUnboxedBuf
    , unboxedBufLength
    , unboxedBufNull
    , unboxedBufHasN
    , unboxedBufTake
    , unboxedBufDrop
    , unboxedBufReadWith
#endif
    ) where

import Foreign (Ptr, castPtr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
#ifdef __GLASGOW_HASKELL__
import qualified Data.ByteString.Internal as BI
import GHC.Exts (Addr#, Int#, Int(..), Ptr(..), (-#), (>=#), (<=#), isTrue#, plusAddr#, touch#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents)
import GHC.IO (IO(..))
#endif

import Data.Binary.Internal (accursedUnutterablePerformIO)

------------------------------------------------------------------------
-- Portable ByteString-backed buffer

type PortableBuf = B.ByteString

packPortableBuf :: PortableBuf -> B.ByteString
packPortableBuf = id
{-# INLINE packPortableBuf #-}

unpackPortableBuf :: B.ByteString -> PortableBuf
unpackPortableBuf = id
{-# INLINE unpackPortableBuf #-}

portableBufLength :: PortableBuf -> Int
portableBufLength = B.length
{-# INLINE portableBufLength #-}

portableBufNull :: PortableBuf -> Bool
portableBufNull = B.null
{-# INLINE portableBufNull #-}

portableBufHasN :: Int -> PortableBuf -> Bool
portableBufHasN n s = B.length s >= n
{-# INLINE portableBufHasN #-}

portableBufTake :: Int -> PortableBuf -> B.ByteString
portableBufTake = B.unsafeTake
{-# INLINE portableBufTake #-}

portableBufDrop :: Int -> PortableBuf -> PortableBuf
portableBufDrop = B.unsafeDrop
{-# INLINE portableBufDrop #-}

portableBufReadWith :: (Ptr a -> IO a) -> PortableBuf -> a
portableBufReadWith f s =
  accursedUnutterablePerformIO $ B.unsafeUseAsCString s (f . castPtr)
{-# INLINE portableBufReadWith #-}

------------------------------------------------------------------------
-- Unboxed GHC buffer

#ifdef __GLASGOW_HASKELL__

type UnboxedBuf = (# Addr#, Int#, ForeignPtrContents #)

packUnboxedBuf :: UnboxedBuf -> B.ByteString
#if MIN_VERSION_bytestring(0,11,0)
packUnboxedBuf (# p#, len#, fpc #) = BI.BS (ForeignPtr p# fpc) (I# len#)
#else
packUnboxedBuf (# p#, len#, fpc #) = BI.PS (ForeignPtr p# fpc) 0 (I# len#)
#endif
{-# INLINE packUnboxedBuf #-}

unpackUnboxedBuf :: B.ByteString -> UnboxedBuf
#if MIN_VERSION_bytestring(0,11,0)
unpackUnboxedBuf (BI.BS (ForeignPtr p# fpc) (I# len#)) = (# p#, len#, fpc #)
#else
unpackUnboxedBuf (BI.PS (ForeignPtr p# fpc) (I# off#) (I# len#)) = (# plusAddr# p# off#, len#, fpc #)
#endif
{-# INLINE unpackUnboxedBuf #-}

unboxedBufLength :: UnboxedBuf -> Int
unboxedBufLength (# _, len#, _ #) = I# len#
{-# INLINE unboxedBufLength #-}

unboxedBufNull :: UnboxedBuf -> Bool
unboxedBufNull (# _, len#, _ #) = isTrue# (len# <=# 0#)
{-# INLINE unboxedBufNull #-}

unboxedBufHasN :: Int -> UnboxedBuf -> Bool
unboxedBufHasN (I# n#) (# _, len#, _ #) = isTrue# (len# >=# n#)
{-# INLINE unboxedBufHasN #-}

unboxedBufTake :: Int -> UnboxedBuf -> B.ByteString
unboxedBufTake (I# n#) (# p#, _, fpc #) = packUnboxedBuf (# p#, n#, fpc #)
{-# INLINE unboxedBufTake #-}

unboxedBufDrop :: Int -> UnboxedBuf -> UnboxedBuf
unboxedBufDrop (I# n#) (# p#, len#, fpc #) = (# plusAddr# p# n#, len# -# n#, fpc #)
{-# INLINE unboxedBufDrop #-}

-- | Read from the buffer's current pointer while keeping its
-- 'ForeignPtrContents' alive across the read.
--
-- We intentionally use 'touch#' rather than @GHC.Exts.keepAlive#@ here:
--
-- * @keepAlive#@ pushes a @stg_keepAlive_frame@ and acts as a Core
--   optimization barrier that prevents GHC from unboxing the return value
--   (boxing every @Word8@/@Word16@/@Word32@/@Word64@ read and slowing
--   decoding by ~40-115%).
-- * 'touch#' is sound here because 'unboxedBufReadWith' is only called from
--   'Data.Binary.Get.Internal.readNWith', whose contract requires @f@ to be
--   deterministic, side-effect-free, non-diverging, and non-throwing (e.g.
--   a single memory-load primop or @unsafe@ FFI read). None of the
--   divergence/exception/GC failure modes of 'touch#' (GHC #14346/#17746)
--   can occur under that contract — matching @bytestring@'s own use of
--   'touch#' in @unsafeHead@/@unsafeIndex@.
unboxedBufReadWith :: (Ptr a -> IO a) -> UnboxedBuf -> a
unboxedBufReadWith f (# p#, _, fpc #) =
  accursedUnutterablePerformIO $ do
    x <- f (Ptr p#)
    IO $ \s -> case touch# fpc s of s' -> (# s', () #)
    return x
{-# INLINE unboxedBufReadWith #-}

#endif

------------------------------------------------------------------------
-- Active buffer selection

#if defined(__GLASGOW_HASKELL__) && !defined(BINARY_PORTABLE_GET)

type Buf = UnboxedBuf

packBuf :: Buf -> B.ByteString
packBuf = packUnboxedBuf
{-# INLINE packBuf #-}

unpackBuf :: B.ByteString -> Buf
unpackBuf = unpackUnboxedBuf
{-# INLINE unpackBuf #-}

bufLength :: Buf -> Int
bufLength = unboxedBufLength
{-# INLINE bufLength #-}

bufNull :: Buf -> Bool
bufNull = unboxedBufNull
{-# INLINE bufNull #-}

bufHasN :: Int -> Buf -> Bool
bufHasN = unboxedBufHasN
{-# INLINE bufHasN #-}

bufTake :: Int -> Buf -> B.ByteString
bufTake = unboxedBufTake
{-# INLINE bufTake #-}

bufDrop :: Int -> Buf -> Buf
bufDrop = unboxedBufDrop
{-# INLINE bufDrop #-}

bufReadWith :: (Ptr a -> IO a) -> Buf -> a
bufReadWith = unboxedBufReadWith
{-# INLINE bufReadWith #-}

#else

type Buf = PortableBuf

packBuf :: Buf -> B.ByteString
packBuf = packPortableBuf
{-# INLINE packBuf #-}

unpackBuf :: B.ByteString -> Buf
unpackBuf = unpackPortableBuf
{-# INLINE unpackBuf #-}

bufLength :: Buf -> Int
bufLength = portableBufLength
{-# INLINE bufLength #-}

bufNull :: Buf -> Bool
bufNull = portableBufNull
{-# INLINE bufNull #-}

bufHasN :: Int -> Buf -> Bool
bufHasN = portableBufHasN
{-# INLINE bufHasN #-}

bufTake :: Int -> Buf -> B.ByteString
bufTake = portableBufTake
{-# INLINE bufTake #-}

bufDrop :: Int -> Buf -> Buf
bufDrop = portableBufDrop
{-# INLINE bufDrop #-}

bufReadWith :: (Ptr a -> IO a) -> Buf -> a
bufReadWith = portableBufReadWith
{-# INLINE bufReadWith #-}

#endif
