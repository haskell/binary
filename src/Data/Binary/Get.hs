-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : unstable
-- Portability : FFI, flexible instances
--
-- The Get monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings
--
-----------------------------------------------------------------------------

module Data.Binary.Get (

    -- * The Get type
      Get
    , runGet

    -- * Primitives
    , getByteString
    , getLazyByteString

    , getWord8
    , getWord16be
    , getWord16le
    , getWord32be
    , getWord32le
    , getWord64be
    , getWord64le
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Trans

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import qualified Data.ByteString.Lazy as L

import Foreign.ForeignPtr
import Foreign

import System.IO.Unsafe

import GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Int

data S = S L.ByteString  -- the rest of the input
           !Int64        -- bytes read

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
    fail ("Failed reading at byte position " ++ show bytes)

------------------------------------------------------------------------
-- Helpers

-- Check that there are more bytes left in the input
ensureLeft :: Int64 -> Get ()
ensureLeft n = do
    S (B.LPS strs) _ <- get
    worker n strs
  where
    worker :: Int64 -> [B.ByteString] -> Get ()
    worker n _ | n <= 0 = return ()
    worker _ []         = fail "not enough bytestring left"
    worker n (x:xs)     = worker (n - fromIntegral (B.length x)) xs

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value
readN :: Int64 -> (L.ByteString -> a) -> Get a
readN n f = do
    ensureLeft n
    S s bytes <- get
    let (consuming, rest) = L.splitAt n s
    put $ S rest (bytes + n)
    return (f consuming)

------------------------------------------------------------------------

-- | An efficient 'get' method for strict ByteStrings
getByteString :: Int -> Get B.ByteString
getByteString n = readN (fromIntegral n) (B.concat . L.toChunks)

-- | An efficient 'get' method for lazy ByteStrings
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n = readN n id

------------------------------------------------------------------------
-- Primtives

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = readN 1 L.head
{-# INLINE getWord8 #-}

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w1 `unsafeShiftL_Word16` 8 .|. w2
{-# INLINE getWord16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w2 `unsafeShiftL_Word16` 8 .|. w1
{-# INLINE getWord16le #-}

-- helper
unsafeShiftL_Word16 (W16# x#) (I# i#) = W16# (narrow16Word# (x# `shiftL#` i#))

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    w1 <- liftM fromIntegral getWord16be
    w2 <- liftM fromIntegral getWord16be
    return $! w1 `shiftL` 16 .|. w2
{-# INLINE getWord32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    w1 <- liftM fromIntegral getWord16le
    w2 <- liftM fromIntegral getWord16le
    return $! w2 `shiftL` 16 .|. w1
{-# INLINE getWord32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    w1 <- liftM fromIntegral getWord32be
    w2 <- liftM fromIntegral getWord32be
    return $! w1 `shiftL` 32 .|. w2
{-# INLINE getWord64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    w1 <- liftM fromIntegral getWord32le
    w2 <- liftM fromIntegral getWord32le
    return $! w2 `shiftL` 32 .|. w1
{-# INLINE getWord64le #-}

------------------------------------------------------------------------

{-# RULES
 "ensureLeft/combine" forall a b.
        ensureLeft a >> ensureLeft b = ensureLeft (max a b)
 #-}

{-# *IGNORE* RULES "readN/combine" forall s1 s2 f1 f2 k.  readN s1 f1 >>= \w1 -> readN s2 f2 >>= \w2 -> k = readN (s1+s2) (\s -> f1 s >>= \w1 -> f2 (L.drop s1 s)) #-}
