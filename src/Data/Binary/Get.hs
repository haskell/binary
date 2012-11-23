{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
-- The Get monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings.
--
-----------------------------------------------------------------------------


module Data.Binary.Get (

    -- * The Get type
      Get

    -- * The lazy input interface
    -- $lazyinterface
    , runGet 
    , runGetOrFail
    , ByteOffset

    -- * The incremental input interface
    -- $incrementalinterface
    , Decoder(..)
    , runGetIncremental

    -- ** Providing input
    , pushChunk
    , pushChunks
    , pushEndOfInput

    -- * Parsing
    , skip
    , isEmpty
    , bytesRead
    -- , lookAhead

    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    , getLazyByteStringNul
    , getRemainingLazyByteString

    -- ** Decoding words
    , getWord8

    -- *** Big-endian decoding
    , getWord16be
    , getWord32be
    , getWord64be

    -- *** Little-endian decoding
    , getWord16le
    , getWord32le
    , getWord64le

    -- *** Host-endian, unaligned decoding
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host

    -- * Deprecated functions
    , runGetState -- DEPRECATED
    , remaining -- DEPRECATED
    , getBytes -- DEPRECATED
    ) where

import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative

import Data.Binary.Get.Internal hiding ( Decoder(..), runGetIncremental )
import qualified Data.Binary.Get.Internal as I

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
-- needed for (# unboxing #) with magic hash
import GHC.Base
import GHC.Word
#endif

-- $lazyinterface
-- The lazy interface consumes a single lazy 'L.ByteString'. It's the easiest
-- interface to get started with, but it doesn't support interleaving I\/O and
-- parsing, unless lazy I/O is used.
-- 
-- There is no way to provide more input other than the initial data. To be
-- able to incrementally give more data, see the incremental input interface.

-- $incrementalinterface
-- The incremental interface gives you more control over how input is
-- provided during parsing. This lets you e.g. interleave parsing and
-- I\/O.
--
-- The incremental interface consumes a strict 'B.ByteString' at a time, each
-- being part of the total amount of input. If your decoder needs more input to
-- finish it will return a 'Partial' with a continuation.
-- If there is no more input, provide it 'Nothing'.
--
-- 'Fail' will be returned if it runs into an error, together with a message,
-- the position and the remaining input.
-- If it succeeds it will return 'Done' with the resulting value,
-- the position and the remaining input.

-- | A decoder procuced by running a 'Get' monad.
data Decoder a = Fail !B.ByteString {-# UNPACK #-} !ByteOffset String
              -- ^ The decoder ran into an error. The decoder either used
              -- 'fail' or was not provided enough input. Contains any
              -- unconsumed input and the number of bytes consumed.
              | Partial (Maybe B.ByteString -> Decoder a)
              -- ^ The decoder has consumed the available input and needs
              -- more to continue. Provide 'Just' if more input is available
              -- and 'Nothing' otherwise, and you will get a new 'Decoder'.
              | Done !B.ByteString {-# UNPACK #-} !ByteOffset a
              -- ^ The decoder has successfully finished. Except for the
              -- output value you also get any unused input as well as the
              -- number of bytes consumed.

-- | Run a 'Get' monad. See 'Decoder' for what to do next, like providing
-- input, handling decoder errors and to get the output value.
-- Hint: Use the helper functions 'pushChunk', 'pushChunks' and
-- 'pushEndOfInput'.
runGetIncremental :: Get a -> Decoder a
runGetIncremental = calculateOffset . I.runGetIncremental

calculateOffset :: I.Decoder a -> Decoder a
calculateOffset r0 = go r0 0
  where
  go r !acc = case r of
                I.Done inp a -> Done inp (acc - fromIntegral (B.length inp)) a
                I.Fail inp s -> Fail inp (acc - fromIntegral (B.length inp)) s
                I.Partial k ->
                    Partial $ \ms ->
                      case ms of
                        Nothing -> go (k Nothing) acc
                        Just i -> go (k ms) (acc + fromIntegral (B.length i))
                I.BytesRead unused k ->
                    go (k $! (acc - unused)) acc

-- | DEPRECATED. Provides compatibility with previous versions of this library.
-- Run a 'Get' monad and return a tuple with thee values.
-- The first value is the result of the decoder. The second and third are the
-- unused input, and the number of consumed bytes.
{-# DEPRECATED runGetState "Use runGetPartial instead. This function will be removed." #-}
runGetState :: Get a -> L.ByteString -> ByteOffset -> (a, L.ByteString, ByteOffset)
runGetState g lbs0 pos' = go (runGetIncremental g) (L.toChunks lbs0)
  where
  go (Done s pos a) lbs = (a, L.fromChunks (s:lbs), pos+pos')
  go (Partial k) (x:xs) = go (k $ Just x) xs
  go (Partial k) []     = go (k Nothing) []
  go (Fail _ pos msg) _ =
    error ("Data.Binary.Get.runGetState at position " ++ show pos ++ ": " ++ msg)

-- | Run a 'Get' monad and return 'Left' on failure and 'Right' on
-- success. In both cases any unconsumed input and the number of bytes
-- consumed is returned. In the case of failure, a human-readable
-- error message is included as well.
runGetOrFail :: Get a -> L.ByteString
             -> Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, a)
runGetOrFail g bs = feedAll (runGetIncremental g) chunks
  where
  chunks = L.toChunks bs
  feedAll (Done x pos r) xs = Right ((L.fromChunks (x:xs)), pos, r)
  feedAll (Partial k) (x:xs) = feedAll (k (Just x)) xs
  feedAll (Partial k) [] = feedAll (k Nothing) []
  feedAll (Fail x pos msg) xs = Left ((L.fromChunks (x:xs)), pos, msg)

-- | An offset, counted in bytes.
type ByteOffset = Int64

-- | The simplest interface to run a 'Get' decoder. If the decoder runs into
-- an error, calls 'fail', or runs out of input, it will call 'error'.
runGet :: Get a -> L.ByteString -> a
runGet g bs = feedAll (runGetIncremental g) chunks
  where
  chunks = L.toChunks bs
  feedAll (Done _ _ r) _ = r
  feedAll (Partial k) (x:xs) = feedAll (k (Just x)) xs
  feedAll (Partial k) [] = feedAll (k Nothing) []
  feedAll (Fail _ pos msg) _ =
    error ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)


-- | Feed a 'Decoder' with more input. If the 'Decoder' is 'Done' or 'Fail' it
-- will add the input to 'B.ByteString' of unconsumed input.
--
-- @
--    'runGetPartial' myParser \`pushChunk\` myInput1 \`pushChunk\` myInput2
-- @
pushChunk :: Decoder a -> B.ByteString -> Decoder a
pushChunk r inp =
  case r of
    Done inp0 p a -> Done (inp0 `B.append` inp) p a
    Partial k -> k (Just inp)
    Fail inp0 p s -> Fail (inp0 `B.append` inp) p s


-- | Feed a 'Decoder' with more input. If the 'Decoder' is 'Done' or 'Fail' it -- will add the input to 'ByteString' of unconsumed input.
--
-- @
--    'runGetPartial' myParser \`pushChunks\` myLazyByteString
-- @
pushChunks :: Decoder a -> L.ByteString -> Decoder a
pushChunks r0 = go r0 . L.toChunks
  where
  go r [] = r
  go r (x:xs) = go (pushChunk r x) xs

-- | Tell a 'Decoder' that there is no more input. This passes 'Nothing' to a
-- 'Partial' decoder, otherwise returns the decoder unchanged.
pushEndOfInput :: Decoder a -> Decoder a
pushEndOfInput r =
  case r of
    Done _ _ _ -> r
    Partial k -> k Nothing
    Fail _ _ _ -> r
 
-- | An efficient get method for lazy ByteStrings. Fails if fewer than @n@
-- bytes are left in the input.
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n0 = L.fromChunks <$> go n0
  where
  consume n str
    | fromIntegral (B.length str) >= n = Right (B.splitAt (fromIntegral n) str)
    | otherwise = Left (fromIntegral (B.length str))
  go n = do
    str <- get
    case consume n str of
      Left used -> do
        put B.empty
        demandInput
        fmap (str:) (go (n - used))
      Right (want,rest) -> do
        put rest
        return [want]

-- | Get a lazy ByteString that is terminated with a NUL byte.
-- The returned string does not contain the NUL byte. Fails
-- if it reaches the end of input without finding a NUL.
getLazyByteStringNul :: Get L.ByteString
getLazyByteStringNul = L.fromChunks <$> go
  where
  findNull str =
    case B.break (==0) str of
      (want,rest) | B.null rest -> Nothing
                  | otherwise -> Just (want, B.drop 1 rest)
  go = do
    str <- get
    case findNull str of
      Nothing -> do
        put B.empty
        demandInput
        fmap (str:) go
      Just (want,rest) -> do
        put rest
        return [want]
 
-- | Get the remaining bytes as a lazy ByteString.
-- Note that this can be an expensive function to use as it forces reading
-- all input and keeping the string in-memory.
getRemainingLazyByteString :: Get L.ByteString
getRemainingLazyByteString = L.fromChunks <$> go
  where
  go = do
    str <- get
    put B.empty
    done <- isEmpty
    if done
      then return [str]
      else fmap (str:) go

------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString.

getPtr :: Storable a => Int -> Get a
getPtr n = readNWith n peek
{-# INLINE getPtr #-}

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = readN 1 B.unsafeHead
{-# INLINE getWord8 #-}

-- force GHC to inline getWordXX
{-# RULES
"getWord8/readN" getWord8 = readN 1 B.unsafeHead
"getWord16be/readN" getWord16be = readN 2 word16be
"getWord16le/readN" getWord16le = readN 2 word16le
"getWord32be/readN" getWord32be = readN 4 word32be
"getWord32le/readN" getWord32le = readN 4 word32le
"getWord64be/readN" getWord64be = readN 8 word64be
"getWord64le/readN" getWord64le = readN 8 word64le
 #-}

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = readN 2 word16be

word16be :: B.ByteString -> Word16
word16be = \s ->
        (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8) .|.
        (fromIntegral (s `B.unsafeIndex` 1))
{-# INLINE getWord16be #-}
{-# INLINE word16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = readN 2 word16le

word16le :: B.ByteString -> Word16
word16le = \s ->
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord16le #-}
{-# INLINE word16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = readN 4 word32be

word32be :: B.ByteString -> Word32
word32be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )
{-# INLINE getWord32be #-}
{-# INLINE word32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = readN 4 word32le

word32le :: B.ByteString -> Word32
word32le = \s ->
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord32le #-}
{-# INLINE word32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = readN 8 word64be

word64be :: B.ByteString -> Word64
word64be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )
{-# INLINE getWord64be #-}
{-# INLINE word64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = readN 8 word64le

word64le :: B.ByteString -> Word64
word64le = \s ->
              (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord64le #-}
{-# INLINE word64le #-}

------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get Word
getWordhost = getPtr (sizeOf (undefined :: Word))
{-# INLINE getWordhost #-}

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Get Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))
{-# INLINE getWord16host #-}

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Get Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))
{-# INLINE getWord32host #-}

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Get Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))
{-# INLINE getWord64host #-}

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
