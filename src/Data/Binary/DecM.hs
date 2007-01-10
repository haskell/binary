-----------------------------------------------------------------------------
-- |
-- Module      :
-- Copyright   : 
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Binary.DecM
    ( DecM
    , runDecM
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

-- import Data.ByteString.Binary.Shift
import GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Int


type S = L.ByteString

newtype DecM a = DecM { unDecM :: State S a }

instance Monad DecM where
    return a = DecM (return a)
    (DecM m) >>= k = DecM (m >>= unDecM . k)
    fail a = DecM (fail a)

instance MonadState S DecM where
    get = DecM get
    put f = DecM (put f)

runDecM :: DecM a -> L.ByteString -> a
runDecM (DecM m) str = evalState m str

ensureLeft :: Int64 -> DecM ()
ensureLeft n = do
    (B.LPS strs) <- get
    worker n strs
  where
    worker :: Int64 -> [B.ByteString] -> DecM ()
    worker n _ | n <= 0 = return ()
    worker _ []         = fail "not enough bytestring left"
    worker n (x:xs)     = worker (n - fromIntegral (B.length x)) xs

takeN :: Int64 -> DecM B.ByteString
takeN n = readN n (\s -> let (B.LPS ls) = L.take n s in B.concat ls)

readN :: Int64 -> (L.ByteString -> a) -> DecM a
readN n f = do
    ensureLeft n
    s <- get
    put $ L.drop n s
    return $! (f s)

{-# INLINE getWord8 #-}
getWord8 :: DecM Word8
getWord8 = readN 1 L.head

{-# INLINE getWord16be #-}
getWord16be :: DecM Word16
getWord16be = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w1 `unsafeShiftL_Word16` 8 .|. w2

{-# INLINE getWord16le #-}
getWord16le :: DecM Word16
getWord16le = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    return $! w2 `unsafeShiftL_Word16` 8 .|. w1 

unsafeShiftL_Word16 (W16# x#) (I# i#) = W16# (narrow16Word# (x# `shiftL#` i#))

{-# INLINE getWord32be #-}
getWord32be :: DecM Word32
getWord32be = do
    w1 <- liftM fromIntegral getWord16be
    w2 <- liftM fromIntegral getWord16be
    return $! w1 `shiftL` 16 .|. w2

{-# INLINE getWord32le #-}
getWord32le :: DecM Word32
getWord32le = do
    w1 <- liftM fromIntegral getWord16le
    w2 <- liftM fromIntegral getWord16le
    return $! w2 `shiftL` 16 .|. w1

{-# INLINE getWord64be #-}
getWord64be :: DecM Word64
getWord64be = do
    w1 <- liftM fromIntegral getWord32be
    w2 <- liftM fromIntegral getWord32be
    return $! w1 `shiftL` 32 .|. w2

{-# INLINE getWord64le #-}
getWord64le :: DecM Word64
getWord64le = do
    w1 <- liftM fromIntegral getWord32le
    w2 <- liftM fromIntegral getWord32le
    return $! w2 `shiftL` 32 .|. w1 

{-# *IGNORE* RULES "readN/combine" forall s1 s2 f1 f2 k.  readN s1 f1 >>= \w1 -> readN s2 f2 >>= \w2 -> k = readN (s1+s2) (\s -> f1 s >>= \w1 -> f2 (L.drop s1 s)) #-}

{-# RULES
 "ensureLeft/combine" forall a b.
        ensureLeft a >> ensureLeft b = ensureLeft (max a b)
 #-}
