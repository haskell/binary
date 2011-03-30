{-# LANGUAGE BangPatterns, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Builder.Internal
-- Copyright   : Lennart Kolmodin, Ross Paterson, Johan Tibell
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- A module containing semi-public 'Builder' internals, like low level
-- construction functions.  Modules which extend the 'Builder' system
-- will need to use this module while ideally most users will be able
-- to make do with the public interface modules.
--
-----------------------------------------------------------------------------

module Data.Binary.Builder.Internal
    ( -- * Low level construction of Builders
      writeN
    , writeAtMost
    ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)

import Data.Binary.Builder.Types

------------------------------------------------------------------------
-- * Creating 'Builder's

-- | Sequence an IO operation on the buffer
withBuffer :: (Buffer -> IO Buffer) -> Builder
withBuffer f = Builder $ \ k buf -> f buf >>= k
{-# INLINE withBuffer #-}

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer fp ep) ->
    let !l = ep `minusPtr` fp
    in runBuilder (f l) k buf
{-# INLINE withSize #-}

-- | Ensure that at least @n@ bytes are available, and then use @f@ to
-- write exactly @n@ bytes into memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN !n f = writeAtMost n (\ p -> f p >> return n)
{-# INLINE writeN #-}

-- | Ensure that @n@ bytes are available, and then use @f@ to write at
-- most @n@ bytes into memory.  @f@ must return the actual number of
-- bytes written.
writeAtMost :: Int -> (Ptr Word8 -> IO Int) -> Builder
writeAtMost !n f = withSize $ \ l ->
    if n <= l
    then withBuffer (writeBuffer f)
    else Builder $ \ k (Buffer fp _) -> return $! Full fp n (step k)
  where
    step = runBuilder (withBuffer (writeBuffer f))
    {-# INLINE step #-}
{-# INLINE [0] writeAtMost #-}

-- | The first argument is a function that writes zero or more bytes
-- to the buffer and returns the number of bytes written.  The buffer
-- must have enough free space.  This is not checked.
writeBuffer :: (Ptr Word8 -> IO Int) -> Buffer -> IO Buffer
writeBuffer f (Buffer fp ep) = do
    n <- f fp
    return $! Buffer (fp `plusPtr` n) ep
{-# INLINE writeBuffer #-}

#if __GLASGOW_HASKELL__ >= 700
-- In versions of GHC prior to 7.0 these rules would make GHC believe
-- that these functions are recursive and the rules wouldn't fire.
{-# RULES

"append/writeAtMost" forall a b (f::Ptr Word8 -> IO Int)
                           (g::Ptr Word8 -> IO Int) ws.
        append (writeAtMost a f) (append (writeAtMost b g) ws) =
            append (writeAtMost (a+b) (\p -> f p >>= \ n -> g (p `plusPtr` n))) ws

"writeAtMost/writeAtMost" forall a b (f::Ptr Word8 -> IO Int)
                           (g::Ptr Word8 -> IO Int).
        append (writeAtMost a f) (writeAtMost b g) =
            writeAtMost (a+b) (\p -> f p >>= \ n -> g (p `plusPtr` n))

 #-}
#endif
