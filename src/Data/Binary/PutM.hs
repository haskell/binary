-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.PutM
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : stable
-- Portability : FFI + flexibile instances
--
-----------------------------------------------------------------------------

module Data.Binary.PutM
    ( PutM
    , runPutM
    , unsafeLiftIO
    , yield
    , pop
    , ensureFree
    , writeN

    , putByteString
    , putLazyByteString

    , putWord8
    , putWord16be
    , putWord32be
    , putWord64be
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
                     {-# UNPACK #-} !Int                -- ^ offset
                     {-# UNPACK #-} !Int                -- ^ used bytes
                     {-# UNPACK #-} !Int                -- ^ length left

newtype PutM a = PutM { unPutM :: ContT [B.ByteString] (StateT Buffer IO) a }

instance Monad PutM where
    return a        = PutM (return a)
    (PutM m) >>= k  = PutM (m >>= unPutM . k)
    (>>)            = bPutM
    fail a          = PutM (fail a)

instance Functor PutM where
    fmap f (PutM m) = PutM (fmap f m)

--
-- A bind for which we control the inlining
--
bPutM :: PutM a -> PutM b -> PutM b
bPutM (PutM a) (PutM b) = PutM (a >> b)
{-# INLINE [1] bPutM #-}

instance MonadState Buffer PutM where
    get     = PutM get
    put f   = PutM (put f)

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

runPutM :: PutM () -> L.ByteString
runPutM m = unsafePerformIO $ do
    i <- initS
    liftM B.LPS $ evalStateT (runContT (unPutM $ m >> pop) (\c -> return [])) i

unsafeLiftIO :: IO a -> PutM a
unsafeLiftIO = PutM . liftIO

-- | Add a ByteString as output.
--
-- Does a 'unsafeInterleaveIO' trick, which will lazely suspend the rest of
-- the computation till that ByteString has been consumed.
--
yield :: B.ByteString -> PutM ()
yield bs = PutM . ContT $ \c -> do
    s@(Buffer _ _ u _) <- get
    assert (u == 0) $ do

    -- this truly is a beautiful piece of magic
    bss <- liftIO $ unsafeInterleaveIO $ evalStateT (c ()) s

    return (bs:bss)

-- | Pop the ByteString we have constructed so far, if any.
pop :: PutM ()
pop = do
    Buffer p o u l <- get
    when (u /= 0) $ do
        put $ Buffer p (o+u) 0 l
        yield $ B.PS p o u
{-# INLINE [1] pop #-}

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> PutM ()
ensureFree n = do
    Buffer _ _ _ l <- get
    when (n > l) $ do
        pop
        let newsize = max n defaultSize
        fp <- unsafeLiftIO $ B.mallocByteString newsize
        put $ Buffer fp 0 0 newsize
{-# INLINE [1] ensureFree #-}

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- bytes into the memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> PutM ()
writeN n f = do
    ensureFree n
    Buffer fp o u l <- get
    unsafeLiftIO $
        withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    put $ Buffer fp o (u+n) (l-n)
{-# INLINE [1] writeN #-}

putByteString :: B.ByteString -> PutM ()
putByteString bs = pop >> yield bs

putLazyByteString :: L.ByteString -> PutM ()
putLazyByteString bs = pop >> mapM_ yield (L.toChunks bs)

putWord8 :: Word8 -> PutM ()
putWord8 = writeN 1 . flip poke
{-# INLINE putWord8 #-}

putWord16be :: Word16 -> PutM ()
putWord16be w16 = do
    let (w1, w2) = divMod w16 0x0100
    putWord8 (fromIntegral w1)
    putWord8 (fromIntegral w2)
{-# INLINE putWord16be #-}

putWord16le :: Word16 -> PutM ()
putWord16le w16 = do
    let (w2, w1) = divMod w16 0x0100
    putWord8 (fromIntegral w1)
    putWord8 (fromIntegral w2)
{-# INLINE putWord16le #-}

putWord32be :: Word32 -> PutM ()
putWord32be w32 = do
    let (w1, w2) = divMod w32 0x00010000
    putWord16be (fromIntegral w1)
    putWord16be (fromIntegral w2)
{-# INLINE putWord32be #-}

putWord32le :: Word32 -> PutM ()
putWord32le w32 = do
    let (w2, w1) = divMod w32 0x00010000
    putWord16le (fromIntegral w1)
    putWord16le (fromIntegral w2)
{-# INLINE putWord32le #-}

putWord64be :: Word64 -> PutM ()
putWord64be w64 = do
    let (w1, w2) = divMod w64 0x0000000100000000
    putWord32be (fromIntegral w1)
    putWord32be (fromIntegral w2)
{-# INLINE putWord64be #-}

putWord64le :: Word64 -> PutM ()
putWord64le w64 = do
    let (w2, w1) = divMod w64 0x0000000100000000
    putWord32le (fromIntegral w1)
    putWord32le (fromIntegral w2)
{-# INLINE putWord64le #-}

{-# RULES "writeN/combine" forall s1 s2 f1 f2. bPutM (writeN s1 f1) (writeN s2 f2) = writeN (s1+s2) (\p -> f1 p >> f2 (p `plusPtr` s1)) #-}
{-# RULES "ensureFree/combine" forall a b. bPutM (ensureFree a) (ensureFree b) = ensureFree (max a b) #-}
{-# RULES "pop/combine" bPutM pop pop = pop #-}
