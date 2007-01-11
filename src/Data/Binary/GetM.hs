-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.GetM
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : unstable
-- Portability : FFI + (currently) flexible instances
--
-----------------------------------------------------------------------------

module Data.Binary.GetM
    ( GetM
    , runGetM
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

type S = L.ByteString

newtype GetM a = GetM { unGetM :: State S a }

instance Monad GetM where
    return a        = GetM (return a)
    (GetM m) >>= k  = GetM (m >>= unGetM . k)
    fail a          = GetM (fail a)

instance MonadState S GetM where
    get         = GetM get
    put f       = GetM (put f)

instance Functor GetM where
    fmap f (GetM m) = GetM (fmap f m)

runGetM :: GetM a -> L.ByteString -> a
runGetM (GetM m) str = evalState m str

ensureLeft :: Int64 -> GetM ()
ensureLeft n = do
    (B.LPS strs) <- get
    worker n strs
  where
    worker :: Int64 -> [B.ByteString] -> GetM ()
    worker n _ | n <= 0 = return ()
    worker _ []         = fail "not enough bytestring left"
    worker n (x:xs)     = worker (n - fromIntegral (B.length x)) xs

readN :: Int64 -> (L.ByteString -> a) -> GetM a
readN n f = do
    ensureLeft n
    s <- get
    let (consuming, rest) = L.splitAt n s
    put rest
    return (f consuming)

getByteString :: Int -> GetM B.ByteString
getByteString n = readN (fromIntegral n) (B.concat . L.toChunks)

getLazyByteString :: Int64 -> GetM L.ByteString
getLazyByteString n = readN n id

{-# INLINE getWord8 #-}
getWord8 :: GetM Word8
getWord8 = readN 1 L.head

{-# INLINE getWord16be #-}
getWord16be :: GetM Word16
getWord16be = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w1 `unsafeShiftL_Word16` 8 .|. w2

{-# INLINE getWord16le #-}
getWord16le :: GetM Word16
getWord16le = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w2 `unsafeShiftL_Word16` 8 .|. w1

unsafeShiftL_Word16 (W16# x#) (I# i#) = W16# (narrow16Word# (x# `shiftL#` i#))

{-# INLINE getWord32be #-}
getWord32be :: GetM Word32
getWord32be = do
    w1 <- liftM fromIntegral getWord16be
    w2 <- liftM fromIntegral getWord16be
    return $! w1 `shiftL` 16 .|. w2

{-# INLINE getWord32le #-}
getWord32le :: GetM Word32
getWord32le = do
    w1 <- liftM fromIntegral getWord16le
    w2 <- liftM fromIntegral getWord16le
    return $! w2 `shiftL` 16 .|. w1

{-# INLINE getWord64be #-}
getWord64be :: GetM Word64
getWord64be = do
    w1 <- liftM fromIntegral getWord32be
    w2 <- liftM fromIntegral getWord32be
    return $! w1 `shiftL` 32 .|. w2

{-# INLINE getWord64le #-}
getWord64le :: GetM Word64
getWord64le = do
    w1 <- liftM fromIntegral getWord32le
    w2 <- liftM fromIntegral getWord32le
    return $! w2 `shiftL` 32 .|. w1 

{-# *IGNORE* RULES "readN/combine" forall s1 s2 f1 f2 k.  readN s1 f1 >>= \w1 -> readN s2 f2 >>= \w2 -> k = readN (s1+s2) (\s -> f1 s >>= \w1 -> f2 (L.drop s1 s)) #-}

{-# RULES
 "ensureLeft/combine" forall a b.
        ensureLeft a >> ensureLeft b = ensureLeft (max a b)
 #-}
