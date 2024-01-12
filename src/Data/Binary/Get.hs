{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}
{-# LANGUAGE Trustworthy #-}

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
-- The 'Get' monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings.
--
-- Primitives are available to decode words of various sizes, both big and
-- little endian.
--
-- Let's decode binary data representing illustrated here.
-- In this example the values are in little endian.
--
-- > +------------------+--------------+-----------------+
-- > | 32 bit timestamp | 32 bit price | 16 bit quantity |
-- > +------------------+--------------+-----------------+
--
-- A corresponding Haskell value looks like this:
--
-- @
--data Trade = Trade
--  { timestamp :: !'Word32'
--  , price     :: !'Word32'
--  , qty       :: !'Word16'
--  } deriving ('Show')
-- @
--
-- The fields in @Trade@ are marked as strict (using @!@) since we don't need
-- laziness here. In practise, you would probably consider using the UNPACK
-- pragma as well.
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unpack-pragma>
--
-- Now, let's have a look at a decoder for this format.
--
-- @
--getTrade :: 'Get' Trade
--getTrade = do
--  timestamp <- 'getWord32le'
--  price     <- 'getWord32le'
--  quantity  <- 'getWord16le'
--  return '$!' Trade timestamp price quantity
-- @
--
-- Or even simpler using applicative style:
--
-- @
--getTrade' :: 'Get' Trade
--getTrade' = Trade '<$>' 'getWord32le' '<*>' 'getWord32le' '<*>' 'getWord16le'
-- @
--
-- There are two kinds of ways to execute this decoder, the lazy input
-- method and the incremental input method. Here we will use the lazy
-- input method.
--
-- Let's first define a function that decodes many @Trade@s.
--
-- @
--getTrades :: Get [Trade]
--getTrades = do
--  empty <- 'isEmpty'
--  if empty
--    then return []
--    else do trade <- getTrade
--            trades <- getTrades
--            return (trade:trades)
-- @
--
-- Finally, we run the decoder:
--
-- @
--lazyIOExample :: IO [Trade]
--lazyIOExample = do
--  input <- BL.readFile \"trades.bin\"
--  return ('runGet' getTrades input)
-- @
--
-- This decoder has the downside that it will need to read all the input before
-- it can return. On the other hand, it will not return anything until
-- it knows it could decode without any decoder errors.
--
-- You could also refactor to a left-fold, to decode in a more streaming fashion,
-- and get the following decoder. It will start to return data without knowing
-- that it can decode all input.
--
-- @
--incrementalExample :: BL.ByteString -> [Trade]
--incrementalExample input0 = go decoder input0
--  where
--    decoder = 'runGetIncremental' getTrade
--    go :: 'Decoder' Trade -> BL.ByteString -> [Trade]
--    go ('Done' leftover _consumed trade) input =
--      trade : go decoder (BL.chunk leftover input)
--    go ('Partial' k) input                     =
--      go (k . takeHeadChunk $ input) (dropHeadChunk input)
--    go ('Fail' _leftover _consumed msg) _input =
--      error msg
--
--takeHeadChunk :: BL.ByteString -> Maybe BS.ByteString
--takeHeadChunk lbs =
--  case lbs of
--    (BL.Chunk bs _) -> Just bs
--    _ -> Nothing
--
--dropHeadChunk :: BL.ByteString -> BL.ByteString
--dropHeadChunk lbs =
--  case lbs of
--    (BL.Chunk _ lbs') -> lbs'
--    _ -> BL.Empty
-- @
--
-- The @lazyIOExample@ uses lazy I/O to read the file from the disk, which is
-- not suitable in all applications, and certainly not if you need to read
-- from a socket which has higher likelihood to fail. To address these needs,
-- use the incremental input method like in @incrementalExample@.
-- For an example of how to read incrementally from a Handle,
-- see the implementation of 'Data.Binary.decodeFileOrFail'.
-----------------------------------------------------------------------------


module Data.Binary.Get (

    -- * The Get monad
      Get

    -- * The lazy input interface
    -- $lazyinterface
    , runGet
    , runGetOrFail
    , ByteOffset

    -- * The incremental input interface
    -- $incrementalinterface
    , Decoder(..)
    , Resupply(..)
    , runGetIncremental

    -- ** Providing input
    , pushChunk
    , pushChunks
    , pushEndOfInput

    -- * Decoding
    , skip
    , isEmpty
    , bytesRead
    , isolate
    , lookAhead
    , lookAheadM
    , lookAheadE
    , label

    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    , getLazyByteStringNul
    , getRemainingLazyByteString

    -- ** Decoding Words
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

    -- ** Decoding Ints
    , getInt8

    -- *** Big-endian decoding
    , getInt16be
    , getInt32be
    , getInt64be

    -- *** Little-endian decoding
    , getInt16le
    , getInt32le
    , getInt64le

    -- *** Host-endian, unaligned decoding
    , getInthost
    , getInt16host
    , getInt32host
    , getInt64host

    -- ** Decoding Floats/Doubles
    , getFloatbe
    , getFloatle
    , getFloathost
    , getDoublebe
    , getDoublele
    , getDoublehost

    -- * Deprecated functions
    , runGetState -- DEPRECATED
    , remaining -- DEPRECATED
    , getBytes -- DEPRECATED
    ) where
#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L (ByteString (..))

import Data.Binary.Get.Internal

-- needed for casting words to float/double
import Data.Binary.FloatCast (wordToFloat, wordToDouble)

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



-- | Feed a 'Decoder' with more input. If the 'Decoder' is 'Done' or 'Fail' it
-- will add the input to 'B.ByteString' of unconsumed input.
--
-- @
--    'runGetIncremental' myParser \`pushChunk\` myInput1 \`pushChunk\` myInput2
-- @
pushChunk :: Decoder a -> B.ByteString -> Decoder a
pushChunk r inp =
  case r of
    Done inp0 p a -> Done (inp0 `L.append` L.fromStrict inp) p a
    Fail inp0 p s -> Fail (inp0 `L.append` L.fromStrict inp) p s

    Partial k
      | B.null inp -> r
      | otherwise  -> k (Supply inp L.Empty)



-- | Feed a 'Decoder' with more input. If the 'Decoder' is 'Done' or 'Fail' it
-- will add the input to 'L.ByteString' of unconsumed input.
--
-- @
--    'runGetIncremental' myParser \`pushChunks\` myLazyByteString
-- @
pushChunks :: Decoder a -> L.ByteString -> Decoder a
pushChunks r ins =
  case r of
    Done inp0 p a -> Done (inp0 `L.append` ins) p a
    Fail inp0 p s -> Fail (inp0 `L.append` ins) p s
    Partial k     ->
      k $ case ins of
            L.Chunk bs lbs -> Supply bs lbs
            L.Empty        -> EndOfInput

-- | Tell a 'Decoder' that there is no more input. This passes 'Nothing' to a
-- 'Partial' decoder, otherwise returns the decoder unchanged.
pushEndOfInput :: Decoder a -> Decoder a
pushEndOfInput r =
  case r of
    Done _ _ _ -> r
    Partial k  -> k EndOfInput
    Fail _ _ _ -> r



------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString.

getPtr :: Storable a => Int -> Get a
getPtr = accursedRead peek
{-# INLINE getPtr #-}

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = unsafeRead B.unsafeHead 1
{-# INLINE getWord8 #-}

-- | Read an Int8 from the monad state
getInt8 :: Get Int8
getInt8 = unsafeRead (fromIntegral . B.unsafeHead) 1
{-# INLINE getInt8 #-}


-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = unsafeRead word16be 2

word16be :: B.ByteString -> Word16
word16be = \s ->
        (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 8) .|.
        (fromIntegral (s `B.unsafeIndex` 1))
{-# INLINE getWord16be #-}
{-# INLINE word16be #-}


-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = unsafeRead word16le 2

word16le :: B.ByteString -> Word16
word16le = \s ->
              (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord16le #-}
{-# INLINE word16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = unsafeRead word32be 4

word32be :: B.ByteString -> Word32
word32be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )
{-# INLINE getWord32be #-}
{-# INLINE word32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = unsafeRead word32le 4

word32le :: B.ByteString -> Word32
word32le = \s ->
              (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord32le #-}
{-# INLINE word32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = unsafeRead word64be 8

word64be :: B.ByteString -> Word64
word64be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )
{-# INLINE getWord64be #-}
{-# INLINE word64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = unsafeRead word64le 8

word64le :: B.ByteString -> Word64
word64le = \s ->
              (fromIntegral (s `B.unsafeIndex` 7) `unsafeShiftL` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE getWord64le #-}
{-# INLINE word64le #-}


-- | Read an Int32 in big endian format.
getInt16be :: Get Int16
getInt16be = unsafeRead (fromIntegral . word16be) 2
{-# INLINE getInt16be #-}

-- | Read an Int32 in big endian format.
getInt32be :: Get Int32
getInt32be = unsafeRead (fromIntegral . word32be) 4
{-# INLINE getInt32be #-}

-- | Read an Int64 in big endian format.
getInt64be :: Get Int64
getInt64be = unsafeRead (fromIntegral . word64be) 8
{-# INLINE getInt64be #-}


-- | Read an Int16 in little endian format.
getInt16le :: Get Int16
getInt16le = unsafeRead (fromIntegral . word16le) 2
{-# INLINE getInt16le #-}

-- | Read an Int32 in little endian format.
getInt32le :: Get Int32
getInt32le = unsafeRead (fromIntegral . word32le) 4
{-# INLINE getInt32le #-}

-- | Read an Int64 in little endian format.
getInt64le :: Get Int64
getInt64le = unsafeRead (fromIntegral . word64le) 8
{-# INLINE getInt64le #-}


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

-- | /O(1)./ Read a single native machine word in native host
-- order. It works in the same way as 'getWordhost'.
getInthost :: Get Int
getInthost = getPtr (sizeOf (undefined :: Int))
{-# INLINE getInthost #-}

-- | /O(1)./ Read a 2 byte Int16 in native host order and host endianness.
getInt16host :: Get Int16
getInt16host = getPtr (sizeOf (undefined :: Int16))
{-# INLINE getInt16host #-}

-- | /O(1)./ Read an Int32 in native host order and host endianness.
getInt32host :: Get Int32
getInt32host = getPtr  (sizeOf (undefined :: Int32))
{-# INLINE getInt32host #-}

-- | /O(1)./ Read an Int64 in native host order and host endianess.
getInt64host   :: Get Int64
getInt64host = getPtr  (sizeOf (undefined :: Int64))
{-# INLINE getInt64host #-}


------------------------------------------------------------------------
-- Double/Float reads

-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: Get Float
getFloatbe = unsafeRead (wordToFloat . word32be) (sizeOf (undefined :: Word32))
{-# INLINE getFloatbe #-}

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: Get Float
getFloatle = unsafeRead (wordToFloat . word32le) (sizeOf (undefined :: Word32))
{-# INLINE getFloatle #-}

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: Get Float
getFloathost = accursedRead (\ptr -> wordToFloat <$> peek ptr) (sizeOf (undefined :: Word32))
{-# INLINE getFloathost #-}

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: Get Double
getDoublebe = unsafeRead (wordToDouble . word64be) (sizeOf (undefined :: Word64))
{-# INLINE getDoublebe #-}

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: Get Double
getDoublele = unsafeRead (wordToDouble . word64le) (sizeOf (undefined :: Word64))
{-# INLINE getDoublele #-}

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: Get Double
getDoublehost =
  accursedRead (\ptr -> wordToDouble <$> peek ptr) (sizeOf (undefined :: Word64))
{-# INLINE getDoublehost #-}
