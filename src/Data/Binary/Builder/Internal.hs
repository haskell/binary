{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Builder.Internal
-- Copyright   : Lennart Kolmodin, Ross Paterson
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- A module containing semi-public 'Builder' internals that exposes
-- low level construction functions.  Modules which extend the
-- 'Builder' system will need to use this module while ideally most
-- users will be able to make do with the public interface modules.
--
-----------------------------------------------------------------------------

module Data.Binary.Builder.Internal (
    -- * Low-level construction of Builders
    writeN
    ) where

import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)

import Data.Binary.Builder.Common

-- | Sequence an IO operation on the buffer
withBuffer :: (Buffer -> IO Buffer) -> Builder
withBuffer f = Builder $ \ k buf -> f buf >>= k
{-# INLINE withBuffer #-}

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) ->
    runBuilder (f l) k buf

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> Builder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` withBuffer (const (newBuffer (max n defaultSize)))
{-# INLINE [0] ensureFree #-}

writeNBuffer :: Int -> (Ptr Word8 -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))
{-# INLINE writeNBuffer #-}

-- | Ensure that @n@ bytes are available, and then use @f@ to write
-- exactly @n@ bytes into memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN n f = ensureFree n `append` withBuffer (writeNBuffer n f)
{-# INLINE [0] writeN #-}

------------------------------------------------------------------------
-- Some nice rules for Builder

#if __GLASGOW_HASKELL__ >= 700
-- In versions of GHC prior to 7.0 these rules would make GHC believe
-- that 'writeN' and 'ensureFree' are recursive and the rules wouldn't
-- fire.
{-# RULES

"append/writeN" forall a b (f::Ptr Word8 -> IO ())
                           (g::Ptr Word8 -> IO ()) ws.
        append (writeN a f) (append (writeN b g) ws) =
            append (writeN (a+b) (\p -> f p >> g (p `plusPtr` a))) ws

"writeN/writeN" forall a b (f::Ptr Word8 -> IO ())
                           (g::Ptr Word8 -> IO ()).
        append (writeN a f) (writeN b g) =
            writeN (a+b) (\p -> f p >> g (p `plusPtr` a))

"ensureFree/ensureFree" forall a b .
        append (ensureFree a) (ensureFree b) = ensureFree (max a b)
 #-}
#endif
