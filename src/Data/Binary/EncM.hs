module Data.Binary.EncM
    ( EncM
    , runEncM
    , unsafeLiftIO
    , yield
    , pop
    , ensureFree
    , writeN
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

newtype EncM a = EncM { unEncM :: ContT [B.ByteString] (StateT Buffer IO) a }

instance Monad EncM where
    return a = EncM (return a)
    (EncM m) >>= k = EncM (m >>= unEncM . k)
    (>>) = bEncM
    fail a = EncM (fail a)

instance Functor EncM where
    fmap f (EncM m) = EncM (fmap f m)

-- A bind for which we control the inlining
{-# INLINE [1] bEncM #-}
bEncM :: EncM a -> EncM b -> EncM b
bEncM (EncM a) (EncM b) = EncM (a >> b)

instance MonadState Buffer EncM where
    get = EncM get
    put f = EncM (put f)

defaultSize = 32 * k - overhead 
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int) 

initS :: IO Buffer
initS = do
  fp <- B.mallocByteString defaultSize 
  return $! Buffer fp 0 0 defaultSize

runEncM :: EncM () -> L.ByteString
runEncM m = unsafePerformIO $ do
    i <- initS
    liftM B.LPS $ evalStateT (runContT (unEncM $ m >> pop) (\c -> return [])) i

unsafeLiftIO :: IO a -> EncM a
unsafeLiftIO = EncM . liftIO

-- |Add a ByteString as output.
-- Does a 'unsafeInterleaveIO' trick, which will lazely suspend the rest of
-- the computation till that ByteString has been consumed.
yield :: B.ByteString -> EncM ()
yield bs = EncM . ContT $ \c -> do
    s@(Buffer _ _ u _) <- get
    assert (u == 0) $ do
    -- this truly is a beautyful piece of magic
    bss <- liftIO $ unsafeInterleaveIO $ evalStateT (c ()) s 
    return (bs:bss)

-- |Pop the ByteString we have constructed so far, if any.
{-# INLINE [1] pop #-}
pop :: EncM ()
pop = do
    Buffer p o u l <- get
    when (u /= 0) $ do
        put $ Buffer p (o+u) 0 l
        yield $ B.PS p o u 

-- |Ensure that there are at least @n@ many bytes available.
{-# INLINE [1] ensureFree #-}
ensureFree :: Int -> EncM ()
ensureFree n = do
    Buffer _ _ _ l <- get
    when (n > l) $ do
        pop
        let newsize = max n defaultSize
        fp <- unsafeLiftIO $ B.mallocByteString newsize
        put $ Buffer fp 0 0 newsize

-- |Ensure that @n@ many bytes are available, and then use @f@ to write some
-- bytes into the memory.
{-# INLINE [1] writeN #-}
writeN :: Int -> (Ptr Word8 -> IO ()) -> EncM ()
writeN n f = do
    ensureFree n
    Buffer fp o u l <- get
    unsafeLiftIO $
        withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    put $ Buffer fp o (u+n) (l-n)

{-# INLINE putWord8 #-}
putWord8 :: Word8 -> EncM ()
putWord8 = writeN 1 . flip poke

{-# INLINE putWord16be #-}
putWord16be :: Word16 -> EncM ()
putWord16be w16 = do
    let (w1, w2) = divMod w16 0x0100
    putWord8 (fromIntegral w1)
    putWord8 (fromIntegral w2)

{-# INLINE putWord16le #-}
putWord16le :: Word16 -> EncM ()
putWord16le w16 = do
    let (w2, w1) = divMod w16 0x0100
    putWord8 (fromIntegral w1)
    putWord8 (fromIntegral w2)

{-# INLINE putWord32be #-}
putWord32be :: Word32 -> EncM ()
putWord32be w32 = do
    let (w1, w2) = divMod w32 0x00010000
    putWord16be (fromIntegral w1)
    putWord16be (fromIntegral w2)

{-# INLINE putWord32le #-}
putWord32le :: Word32 -> EncM ()
putWord32le w32 = do
    let (w2, w1) = divMod w32 0x00010000
    putWord16le (fromIntegral w1)
    putWord16le (fromIntegral w2)

{-# INLINE putWord64be #-}
putWord64be :: Word64 -> EncM ()
putWord64be w64 = do
    let (w1, w2) = divMod w64 0x0000000100000000
    putWord32be (fromIntegral w1)
    putWord32be (fromIntegral w2)

{-# INLINE putWord64le #-}
putWord64le :: Word64 -> EncM ()
putWord64le w64 = do
    let (w2, w1) = divMod w64 0x0000000100000000
    putWord32le (fromIntegral w1)
    putWord32le (fromIntegral w2)

{-# RULES "writeN/combine" forall s1 s2 f1 f2. bEncM (writeN s1 f1) (writeN s2 f2) = writeN (s1+s2) (\p -> f1 p >> f2 (p `plusPtr` s1)) #-}
{-# RULES "ensureFree/combine" forall a b. bEncM (ensureFree a) (ensureFree b) = ensureFree (max a b) #-}
{-# RULES "pop/combine" bEncM pop pop = pop #-}
