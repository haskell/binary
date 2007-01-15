-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Put
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : stable
-- Portability : Portable to Hugs and GHC. Requires MPTCs
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

import Control.Monad.Writer

import Foreign
import System.IO.Unsafe

import Data.Monoid
import Data.Word
import qualified Data.ByteString.Base as S
import qualified Data.ByteString.Lazy as L

type Put = Writer Builder ()

runPut              :: Put -> L.ByteString
runPut              = runBuilder . execWriter

flush               :: Put
flush               = tell flushB

putWord8            :: Word8 -> Put
putWord8            = tell . putWord8B

putByteString       :: S.ByteString -> Put
putByteString       = tell . putByteStringB

putLazyByteString   :: L.ByteString -> Put
putLazyByteString   = tell . putLazyByteStringB

-- | Write a Word16 in big endian format
putWord16be         :: Word16 -> Put
putWord16be         = tell . putWord16beB

-- | Write a Word16 in little endian format
putWord16le         :: Word16 -> Put
putWord16le         = tell . putWord16leB

-- | Write a Word32 in big endian format
putWord32be         :: Word32 -> Put
putWord32be         = tell . putWord32beB

-- | Write a Word32 in little endian format
putWord32le         :: Word32 -> Put
putWord32le         = tell . putWord32leB

-- | Write a Word64 in big endian format
putWord64be         :: Word64 -> Put
putWord64be         = tell . putWord64beB

-- | Write a Word64 in little endian format
putWord64le         :: Word64 -> Put
putWord64le         = tell . putWord64leB

-- ---------------------------------------------------------------------
--
-- | The Builder monoid for efficiently constructing lazy bytestrings.
--

data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

-- | The Builder monoid abstracts over the construction of a lazy
-- bytestring by filling byte arrays piece by piece.  The 'put' method
-- of class Binary implicitly fills a buffer, threaded through the
-- Builder monoid.  As each buffer is filled, it is \'popped\' off,
-- to become a new chunk of the resulting lazy ByteString.  All this is
-- hidden from the user of class Binary.
--
newtype Builder = Builder {
        unBuilder :: (Buffer -> [S.ByteString]) -> Buffer -> [S.ByteString] 
    }

instance Monoid Builder where
    mempty = empty
    Builder f `mappend` Builder g = Builder (f . g)

empty :: Builder
empty = Builder id
{-# INLINE [1] empty #-}

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)
{-# INLINE [1] append #-}

--
-- copied from Data.ByteString.Lazy
--
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

--
-- Run the builder monoid
--
runBuilder :: Builder -> L.ByteString
runBuilder m = S.LPS $ unsafePerformIO $ do
    buf <- newBuffer defaultSize
    return (unBuilder (m `append` flushB) (const []) buf)

-- | Sequence an IO operation on the buffer
unsafeLiftIO :: (Buffer -> IO Buffer) -> Builder
unsafeLiftIO f =  Builder $ \ k buf -> unsafePerformIO $ do
    buf' <- f buf
    return (k buf')

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) ->
    unBuilder (f l) k buf

-- | Map the resulting list of bytestrings.
mapBuilder :: ([S.ByteString] -> [S.ByteString]) -> Builder
mapBuilder f = Builder (f .)

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flushB :: Builder
flushB = Builder $ \ k buf@(Buffer p o u l) ->
    if u == 0
      then k buf
      else S.PS p o u : k (Buffer p (o+u) 0 l)
{-# INLINE [1] flush #-}

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> Builder
ensureFree n = withSize $ \ l ->
    if n <= l then empty else
        flushB `append` unsafeLiftIO (const (newBuffer (max n defaultSize)))
{-# INLINE [1] ensureFree #-}

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- bytes into the memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN n f = ensureFree n `append` unsafeLiftIO (writeNBuffer n f)
{-# INLINE [1] writeN #-}

writeNBuffer :: Int -> (Ptr Word8 -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size

------------------------------------------------------------------------

-- | Write a byte into the Builder's output buffer
putWord8B :: Word8 -> Builder
putWord8B = writeN 1 . flip poke
{-# INLINE putWord8 #-}

-- | Write a strict ByteString efficiently
putByteStringB :: S.ByteString -> Builder
putByteStringB bs = flushB `append` mapBuilder (bs :)

-- | Write a lazy ByteString efficiently 
putLazyByteStringB :: L.ByteString -> Builder
putLazyByteStringB bs = flushB `append` mapBuilder (L.toChunks bs ++)

------------------------------------------------------------------------

-- | Write a Word16 in big endian format
putWord16beB :: Word16 -> Builder
putWord16beB w16 =
    let w1 = shiftR w16 8
        w2 = w16 .&. 0xff
    in
    putWord8B (fromIntegral w1) `append`
    putWord8B (fromIntegral w2)
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16leB :: Word16 -> Builder
putWord16leB w16 =
    let w2 = shiftR w16 8
        w1 = w16 .&. 0xff
    in
    putWord8B (fromIntegral w1) `append`
    putWord8B (fromIntegral w2)
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32beB :: Word32 -> Builder
putWord32beB w32 =
    let w1 = (w32 `shiftR` 24)
        w2 = (w32 `shiftR` 16) .&. 0xff
        w3 = (w32 `shiftR`  8) .&. 0xff
        w4 =  w32              .&. 0xff
    in
    putWord8B (fromIntegral w1) `append`
    putWord8B (fromIntegral w2) `append`
    putWord8B (fromIntegral w3) `append`
    putWord8B (fromIntegral w4)
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32leB :: Word32 -> Builder
putWord32leB w32 =
    let w4 = (w32 `shiftR` 24)
        w3 = (w32 `shiftR` 16) .&. 0xff
        w2 = (w32 `shiftR`  8) .&. 0xff
        w1 =  w32              .&. 0xff
    in
    putWord8B (fromIntegral w1) `append`
    putWord8B (fromIntegral w2) `append`
    putWord8B (fromIntegral w3) `append`
    putWord8B (fromIntegral w4)
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64beB :: Word64 -> Builder
putWord64beB w64 =
    let w1 = shiftR w64 32
        w2 = w64 .&. 0xffffffff
    in
    putWord32beB (fromIntegral w1) `append`
    putWord32beB (fromIntegral w2)
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64leB :: Word64 -> Builder
putWord64leB w64 =
    let w2 = shiftR w64 32
        w1 = w64 .&. 0xffffffff
    in
    putWord32leB (fromIntegral w1) `append`
    putWord32leB (fromIntegral w2)
{-# INLINE putWord64le #-}

------------------------------------------------------------------------
-- Some nice rules for Builder

{-# TRICKY RULES

"writeN/combine" forall s1 s2 f1 f2 .
        bindP (writeN s1 f1) (writeN s2 f2) =
        writeN (s1+s2) (\p -> f1 p >> f2 (p `plusPtr` s1))

"ensureFree/combine" forall a b .
        bindP (ensureFree a) (ensureFree b) =
        ensureFree (max a b)

"flush/combine"
        bindP flush flush = flush

 #-}
