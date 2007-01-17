{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : unstable
-- Portability : Portable. Requires MPTCs
--
-- The Get monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings
--
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__)
#include "MachDeps.h"
#endif

module Data.Binary.Get (

    -- * The Get type
      Get
    , runGet

    -- * Parsing
    , skip
    , lookAhead
    , remaining
    , isEmpty

    -- * Parsing particular types
    , getWord8

    -- ** ByteStrings
    , getByteString
    , getLazyByteString

    -- ** Big-endian reads
    , getWord16be
    , getWord16le
    , getWord32be

    -- ** Little-endian reads
    , getWord32le
    , getWord64be
    , getWord64le

  ) where

import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import qualified Data.ByteString.Lazy as L

import Foreign

#if defined(__GLASGOW_HASKELL__)
import GHC.Base
import GHC.Word
import GHC.Int
#endif

-- | The parse state
data S = S {-# UNPACK #-} !L.ByteString  -- the rest of the input
           {-# UNPACK #-} !Int64        -- bytes read

-- | The Get monad is just a State monad carrying around the input ByteString
newtype Get a = Get { unGet :: State S a }

instance Monad Get where
    return a      = Get (return a)
    (Get m) >>= k = Get (m >>= unGet . k)
    fail          = failDesc

instance MonadState S Get where
    get         = Get get
    put f       = Get (put f)

instance Functor Get where
    fmap f (Get m) = Get (fmap f m)

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> L.ByteString -> a
runGet (Get m) str = evalState m (S str 0)

failDesc :: String -> Get a
failDesc err = do
    S _ bytes <- get
    Get (fail (err ++ ". Failed reading at byte position " ++ show bytes))

-- | Skip ahead @n@ bytes
skip :: Int -> Get ()
skip n = readN n (const ())

-- | Get the next @n@ bytes as a lazy ByteString, without consuming them. 
-- Fails if not enough bytes are available.
lookAhead :: Int -> Get L.ByteString
lookAhead n = do
    ensureLeft n
    S s _ <- get
    return (L.take (fromIntegral n) s)

-- | Get the number of remaining unparsed bytes.
-- Useful for checking whether all input has been consumed.
-- Note that this forces the rest of the input.
remaining :: Get Int64
remaining = do
    S s _ <- get
    return (L.length s)

-- | Test whether all input has been consumed,
-- i.e. there are no remaining unparsed bytes.
isEmpty :: Get Bool
isEmpty = do
    S s _ <- get
    return (L.null s)

------------------------------------------------------------------------
-- Helpers

-- Check that there are more bytes left in the input
ensureLeft :: Int -> Get ()
ensureLeft n = do
    S (B.LPS strs) _ <- get
    worker n strs
  where
    worker :: Int -> [B.ByteString] -> Get ()
    worker i _ | i `seq` False = undefined
    worker i _ | i <= 0 = return ()
    worker i []         =
        fail $ concat [ "Data.Binary.Get.ensureLeft: End of input. Wanted "
                      , show n
                      , " bytes, found "
                      , show (n - i)
                      , "." ]
    worker i (x:xs)     = worker (i - fromIntegral (B.length x)) xs
    {-# INLINE worker #-}
{-# INLINE ensureLeft #-}

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value
readN :: Int -> (L.ByteString -> a) -> Get a
readN n f = do
    ensureLeft n
    S s bytes <- get
    let (consuming, rest) = L.splitAt (fromIntegral n) s
    put $! S rest (bytes + (fromIntegral n))
    return (f consuming)
{-# INLINE readN #-}
-- ^ important

------------------------------------------------------------------------

-- | An efficient 'get' method for strict ByteStrings
getByteString :: Int -> Get B.ByteString
getByteString n = readN (fromIntegral n) (B.concat . L.toChunks)
{-# INLINE getByteString #-}

-- | An efficient 'get' method for lazy ByteStrings
getLazyByteString :: Int -> Get L.ByteString
getLazyByteString n = readN n id
{-# INLINE getLazyByteString #-}

------------------------------------------------------------------------
-- Primtives

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = readN 1 L.head
{-# INLINE getWord8 #-}

-- XXX readN k 

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w1 `shiftl_w16` 8 .|. w2
{-# INLINE getWord16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w2 `shiftl_w16` 8 .|. w1
{-# INLINE getWord16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    w3 <- liftM fromIntegral getWord8
    w4 <- liftM fromIntegral getWord8
    return $! (w1 `shiftl_w32` 24) .|.
              (w2 `shiftl_w32` 16) .|.
              (w3 `shiftl_w32`  8) .|.
              (w4)
{-# INLINE getWord32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    w3 <- liftM fromIntegral getWord8
    w4 <- liftM fromIntegral getWord8
    return $! (w4 `shiftl_w32` 24) .|.
              (w3 `shiftl_w32` 16) .|.
              (w2 `shiftl_w32`  8) .|.
              (w1)
{-# INLINE getWord32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    w3 <- liftM fromIntegral getWord8
    w4 <- liftM fromIntegral getWord8
    w5 <- liftM fromIntegral getWord8
    w6 <- liftM fromIntegral getWord8
    w7 <- liftM fromIntegral getWord8
    w8 <- liftM fromIntegral getWord8
    return $! (w1 `shiftl_w64` 56) .|.
              (w2 `shiftl_w64` 48) .|.
              (w3 `shiftl_w64` 40) .|.
              (w4 `shiftl_w64` 32) .|.
              (w5 `shiftl_w64` 24) .|.
              (w6 `shiftl_w64` 16) .|.
              (w7 `shiftl_w64`  8) .|.
              (w8)
{-# INLINE getWord64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    w3 <- liftM fromIntegral getWord8
    w4 <- liftM fromIntegral getWord8
    w5 <- liftM fromIntegral getWord8
    w6 <- liftM fromIntegral getWord8
    w7 <- liftM fromIntegral getWord8
    w8 <- liftM fromIntegral getWord8
    return $! (w8 `shiftl_w64` 56) .|.
              (w7 `shiftl_w64` 48) .|.
              (w6 `shiftl_w64` 40) .|.
              (w5 `shiftl_w64` 32) .|.
              (w4 `shiftl_w64` 24) .|.
              (w3 `shiftl_w64` 16) .|.
              (w2 `shiftl_w64`  8) .|.
              (w1)
{-# INLINE getWord64le #-}

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

foreign import ccall unsafe "stg_uncheckedShiftL64"     
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
