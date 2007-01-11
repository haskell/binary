-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Put
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : stable
-- Portability : FFI, flexible instances
--
-- The Put monad. A monad for efficiently constructing lazy bytestrings.
--
-----------------------------------------------------------------------------

module Data.Binary.Put (

    -- * The Put type
      Put
    , runPut

    -- * Flushing the implicit parse state
    , flush

    -- * Primitives
    , putWord8
    , putByteString
    , putLazyByteString

    -- * Big-endian primitives
    , putWord16be
    , putWord32be
    , putWord64be

    -- * Little-endian primitives
    , putWord16le
    , putWord32le
    , putWord64le

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

data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

-- | The Put monad abstracts over the construction of a lazy bytestring
-- by filling byte arrays piece by piece. The 'put' method of class
-- Binary implicitly fills a buffer, threaded through the Put monad. As
-- each buffer is filled, it is 'popped' off, to become a new chunk of the
-- resulting lazy ByteString. All this is hidden from the user of class
-- Binary.
--
newtype Put a = Put { unPut :: ContT [B.ByteString] (StateT Buffer IO) a }

instance Monad Put where
    return a      = Put (return a)
    (Put m) >>= k = Put (m >>= unPut . k)
    (>>)          = bindP
    fail a        = Put (fail a)

--
-- A bind for which we control the inlining
--
bindP :: Put a -> Put b -> Put b
bindP (Put a) (Put b) = Put (a >> b)
{-# INLINE [1] bindP #-}

instance Functor Put where
    fmap f (Put m) = Put (fmap f m)

instance MonadState Buffer Put where
    get     = Put get
    put f   = Put (put f)

--
-- copied from Data.ByteString.Lazy
--
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

initS :: IO Buffer
initS = do
  fp <- B.mallocByteString defaultSize
  return $! Buffer fp 0 0 defaultSize

runPut :: Put () -> L.ByteString
runPut m = unsafePerformIO $ do
    i <- initS
    liftM B.LPS $ evalStateT (runContT (unPut $ m >> flush) (\c -> return [])) i

-- Lift an IO action
unsafeLiftIO :: IO a -> Put a
unsafeLiftIO = Put . liftIO

-- | Take a strict bytestring, and add it to the list of chunks in the
-- monad's lazy bytestring state.
--
-- Does a 'unsafeInterleaveIO' trick, which will lazely suspend the rest of
-- the computation till that ByteString has been consumed.
--
yield :: B.ByteString -> Put ()
yield bs = Put . ContT $ \c -> do
    s@(Buffer _ _ u _) <- get
    assert (u == 0) $ do

    -- this truly is a beautiful piece of magic
    bss <- liftIO $ unsafeInterleaveIO $ evalStateT (c ()) s
    return (bs:bss)

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flush :: Put ()
flush = do
    Buffer p o u l <- get
    when (u /= 0) $ do
        put $ Buffer p (o+u) 0 l
        yield $ B.PS p o u
{-# INLINE [1] flush #-}

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> Put ()
ensureFree n = do
    Buffer _ _ _ l <- get
    when (n > l) $ do
        flush
        let newsize = max n defaultSize
        fp <- unsafeLiftIO $ B.mallocByteString newsize
        put $ Buffer fp 0 0 newsize
{-# INLINE [1] ensureFree #-}

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- bytes into the memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Put ()
writeN n f = do
    ensureFree n
    Buffer fp o u l <- get
    unsafeLiftIO $
        withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    put $ Buffer fp o (u+n) (l-n)
{-# INLINE [1] writeN #-}

------------------------------------------------------------------------

-- | Write a byte into the Put monad's output buffer
putWord8 :: Word8 -> Put ()
putWord8 = writeN 1 . flip poke
{-# INLINE putWord8 #-}

-- | Write a strict ByteString efficiently
putByteString :: B.ByteString -> Put ()
putByteString bs     = flush >> yield bs

-- | Write a lazy ByteString efficiently 
putLazyByteString :: L.ByteString -> Put ()
putLazyByteString bs = flush >> mapM_ yield (L.toChunks bs)

------------------------------------------------------------------------

-- | Write a Word16 in big endian format
putWord16be :: Word16 -> Put ()
putWord16be w16 = do
    let (w1, w2) = divMod w16 0x0100
    putWord8 (fromIntegral w1)
    putWord8 (fromIntegral w2)
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le :: Word16 -> Put ()
putWord16le w16 = do
    let (w2, w1) = divMod w16 0x0100
    putWord8 (fromIntegral w1)
    putWord8 (fromIntegral w2)
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Put ()
putWord32be w32 = do
    let (w1, w2) = divMod w32 0x00010000
    putWord16be (fromIntegral w1)
    putWord16be (fromIntegral w2)
{-# INLINE putWord32be #-}

-- | Write a Word32 in big endian format
putWord32le :: Word32 -> Put ()
putWord32le w32 = do
    let (w2, w1) = divMod w32 0x00010000
    putWord16le (fromIntegral w1)
    putWord16le (fromIntegral w2)
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Put ()
putWord64be w64 = do
    let (w1, w2) = divMod w64 0x0000000100000000
    putWord32be (fromIntegral w1)
    putWord32be (fromIntegral w2)
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le :: Word64 -> Put ()
putWord64le w64 = do
    let (w2, w1) = divMod w64 0x0000000100000000
    putWord32le (fromIntegral w1)
    putWord32le (fromIntegral w2)
{-# INLINE putWord64le #-}

------------------------------------------------------------------------
-- Some nice rules for put 

{-# RULES

"writeN/combine" forall s1 s2 f1 f2 .
        bindP (writeN s1 f1) (writeN s2 f2) =
        writeN (s1+s2) (\p -> f1 p >> f2 (p `plusPtr` s1))

"ensureFree/combine" forall a b .
        bindP (ensureFree a) (ensureFree b) =
        ensureFree (max a b)

"flush/combine"
        bindP flush flush = flush

 #-}
