{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}

-- CPP C style pre-precessing, the #if defined lines
-- RankNTypes forall r. statement
-- MagicHash the (# unboxing #), also needs GHC.primitives

module Data.Binary.Get (

    -- * The Get type
      Get
    , I.Result(..)
    , runGet
    , runGetPartial
    , runGetState -- DEPRECATED

    , feed
    , eof

    -- * Parsing
    , skip
    -- , lookAhead

    -- * Utility
    , bytesRead
    , remaining
    , getBytes
    , isEmpty

    -- * Parsing particular types
    , getWord8
    
    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    -- , getLazyByteStringNul
    -- , getRemainingLazyByteString

    -- ** Big-endian reads
    , getWord16be
    , getWord32be
    , getWord64be

    -- ** Little-endian reads
    , getWord16le
    , getWord32le
    , getWord64le

    -- ** Host-endian, unaligned reads
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host


    ) where

import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative

import Data.Binary.Get.Internal hiding ( Result(..) )
import qualified Data.Binary.Get.Internal as I

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
-- needed for (# unboxing #) with magic hash
import GHC.Base
import GHC.Word
-- import GHC.Int
#endif

-- | DEPRECATED. Provides compatibility with previous versions of this library.
-- Run a 'Get' monad and provide both all the input and an initial position.
-- Additional to the result of get it returns the number of consumed bytes
-- and the unconsumed input.
--
{-# DEPRECATED runGetState "Use runGetPartial instead. This function will be removed." #-}
runGetState :: Get a -> L.ByteString -> Int64 -> (a, L.ByteString, Int64)
runGetState g lbs p = go (runCont g B.empty p (\i p a -> I.Done i p a))
                         (L.toChunks lbs)
  where
  go (I.Done s p a) lbs   = (a, L.fromChunks (s:lbs), p)
  go (I.Partial f) (x:xs) = go (f $ Just x) xs
  go (I.Partial f) []     = go (f Nothing) []
  go (I.Fail _ _ msg)   _ = error ("Data.Binary.Get.runGetState: " ++ msg)


-- | The simplest interface to run a 'Get' parser, also compatible with
-- previous versions of the binary library. If the parser runs into an
-- error, calling 'fail' or running out of input, it will call 'error'.
runGet :: Get a -> L.ByteString -> a
runGet g bs = feedAll (runGetPartial g) chunks
  where
  chunks = L.toChunks bs
  feedAll (I.Done _ _ r) _ = r
  feedAll (I.Partial c) (x:xs) = feedAll (c (Just x)) xs
  feedAll (I.Partial c) [] = feedAll (c Nothing) []
  feedAll (I.Fail _ _ msg) _ = error msg

-- | Feed a 'Result' with more input. If the 'Result' is 'Done' or 'Fail' it
-- will add the input to 'ByteString' of unconsumed input.
--
-- @
--    'runGetPartial' myParser `feed` myInput1 `feed` myInput2
-- @
feed :: I.Result a -> B.ByteString -> I.Result a
feed r inp =
  case r of
    I.Done inp0 p a -> I.Done (inp0 `B.append` inp) p a
    I.Partial f -> f (Just inp)
    I.Fail inp0 p s -> I.Fail (inp0 `B.append` inp) p s

-- | Tell a 'Result' that there is no more input.
eof :: I.Result a -> I.Result a
eof r =
  case r of
    I.Done _ _ _ -> r
    I.Partial f -> f Nothing
    I.Fail _ _ _ -> r
 
------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString.

getPtr :: Storable a => Int -> Get a
getPtr n = readNWith n peek
{-# INLINE getPtr #-}

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = readN 1 B.unsafeHead
{-# INLINE getWord8 #-}

-- force GHC to inline getWordXX
{-# RULES
"getWord8/readN" getWord8 = readN 1 B.unsafeHead
"getWord16be/readN" getWord16be = readN 2 word16be
"getWord16le/readN" getWord16le = readN 2 word16le
"getWord32be/readN" getWord32be = readN 4 word32be
"getWord32le/readN" getWord32le = readN 4 word32le
"getWord64be/readN" getWord64be = readN 8 word64be
"getWord64le/readN" getWord64le = readN 8 word64le
 #-}

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = readN 2 word16be

word16be :: B.ByteString -> Word16
word16be = \s ->
        (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8) .|.
        (fromIntegral (s `B.unsafeIndex` 1))
{-# INLINE getWord16be #-}
{-# INLINE word16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = readN 2 word16le

word16le :: B.ByteString -> Word16
word16le = \s ->
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord16le #-}
{-# INLINE word16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = readN 4 word32be

word32be :: B.ByteString -> Word32
word32be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )
{-# INLINE getWord32be #-}
{-# INLINE word32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = readN 4 word32le

word32le :: B.ByteString -> Word32
word32le = \s ->
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord32le #-}
{-# INLINE word32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = readN 8 word64be

word64be :: B.ByteString -> Word64
word64be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )
{-# INLINE getWord64be #-}
{-# INLINE word64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = readN 8 word64le

word64le :: B.ByteString -> Word64
word64le = \s ->
              (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord64le #-}
{-# INLINE word64le #-}

------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get Word
getWordhost = getPtr (sizeOf (undefined :: Word))
{-# INLINE getWordhost #-}

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Get Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))
{-# INLINE getWord16host #-}

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Get Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))
{-# INLINE getWord32host #-}

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Get Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))
{-# INLINE getWord64host #-}

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
