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
    , writeAtMost
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

-- | Ensure that @n@ bytes are available, and then use @f@ to write
-- exactly @n@ bytes into memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN n f = writeAtMost n (\ p -> f p >> return n)
{-# INLINE writeN #-}

writeBuffer :: (Ptr Word8 -> IO Int) -> Buffer -> IO Buffer
writeBuffer f (Buffer fp o u l) = do
    n <- withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return $! Buffer fp o (u+n) (l-n)
{-# INLINE writeBuffer #-}

-- | Ensure that @n@ bytes are available, and then use @f@ to write at
-- most @n@ bytes into memory.  @f@ must return the actual number of
-- bytes written.
writeAtMost :: Int -> (Ptr Word8 -> IO Int) -> Builder
writeAtMost n f = ensureFree n `append` withBuffer (writeBuffer f)
{-# INLINE [0] writeAtMost #-}

------------------------------------------------------------------------
-- Some nice rules for Builder

#if __GLASGOW_HASKELL__ >= 700
-- In versions of GHC prior to 7.0 these rules would make GHC believe
-- that 'writeN' and 'ensureFree' are recursive and the rules wouldn't
-- fire.
{-# RULES

"append/writeAtMost" forall a b (f::Ptr Word8 -> IO Int)
                           (g::Ptr Word8 -> IO Int) ws.
        append (writeAtMost a f) (append (writeAtMost b g) ws) =
            append (writeAtMost (a+b) (\p -> f p >>= \n ->
                                        g (p `plusPtr` n) >>= \m ->
                                        let s = n+m in s `seq` return s)) ws

"writeAtMost/writeAtMost" forall a b (f::Ptr Word8 -> IO Int)
                           (g::Ptr Word8 -> IO Int).
        append (writeAtMost a f) (writeAtMost b g) =
            writeAtMost (a+b) (\p -> f p >>= \n ->
                                g (p `plusPtr` n) >>= \m ->
                                let s = n+m in s `seq` return s)

"ensureFree/ensureFree" forall a b .
        append (ensureFree a) (ensureFree b) = ensureFree (max a b)
 #-}
#endif
