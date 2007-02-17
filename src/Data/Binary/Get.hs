{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
-- The Get monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings
--
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Binary.Get (

    -- * The Get type
      Get
    , runGet

    -- * Parsing
    , skip
    , uncheckedSkip
    , lookAhead
    , lookAheadM
    , lookAheadE
    , uncheckedLookAhead

    , getBytes
    , remaining
    , isEmpty

    -- * Parsing particular types
    , getWord8

    -- ** ByteStrings
    , getByteString
    , getLazyByteString

    -- ** Big-endian reads
    , getWord16be
    , getWord32be
    , getWord64be

    -- ** Little-endian reads
    , getWord16le
    , getWord32le
    , getWord64le

    -- ** Host-endian, unaligned reads
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host

  ) where

import Control.Monad (when)
import Data.Maybe (isNothing)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import qualified Data.ByteString.Lazy as L

import Foreign

-- used by splitAtST
import Control.Monad.ST
import Data.STRef

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
import GHC.Int
#endif

-- | The parse state
data S = S {-# UNPACK #-} !B.ByteString  -- current chunk
           L.ByteString                  -- the rest of the input
           {-# UNPACK #-} !Int64         -- bytes read

-- | The Get monad is just a State monad carrying around the input ByteString
newtype Get a = Get { unGet :: S -> (a, S) }

instance Functor Get where
    fmap f m = Get (\s -> let (a, s') = unGet m s
                          in (f a, s'))

instance Monad Get where
    return a  = Get (\s -> (a, s))
    m >>= k   = Get (\s -> let (a, s') = unGet m s
                           in unGet (k a) s')
    fail      = failDesc

------------------------------------------------------------------------

get :: Get S
get   = Get (\s -> (s, s))

put :: S -> Get ()
put s = Get (\_ -> ((), s))

------------------------------------------------------------------------

initState :: L.ByteString -> S
initState (B.LPS xs) =
    case xs of
      []     -> S B.empty L.empty 0
      (x:xs') -> S x (B.LPS xs') 0
{-# INLINE initState #-}

mkState :: L.ByteString -> Int64 -> S
mkState (B.LPS xs) =
    case xs of
        [] -> S B.empty L.empty
        (x:xs') -> S x (B.LPS xs')
{-# INLINE mkState #-}

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> L.ByteString -> a
runGet m str = case unGet m (initState str) of (a, _) -> a

------------------------------------------------------------------------

failDesc :: String -> Get a
failDesc err = do
    S _ _ bytes <- get
    Get (error (err ++ ". Failed reading at byte position " ++ show bytes))

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = readN (fromIntegral n) (const ())

-- | Skip ahead @n@ bytes. No error if there isn't enough bytes.
uncheckedSkip :: Int64 -> Get ()
uncheckedSkip n = do
    S s ss bytes <- get
    if fromIntegral (B.length s) >= n
      then put (S (B.drop (fromIntegral n) s) ss (bytes + n))
      else do
        let rest = L.drop (n - fromIntegral (B.length s)) ss
        put $! mkState rest (bytes + n)

-- | Run @ga@, but return without consuming its input.
-- Fails if @ga@ fails.
lookAhead :: Get a -> Get a
lookAhead ga = do
    s <- get
    a <- ga
    put s
    return a

-- | Like 'lookAhead', but consume the input if @gma@ returns 'Just _'.
-- Fails if @gma@ fails.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM gma = do
    s <- get
    ma <- gma
    when (isNothing ma) $
        put s
    return ma

-- | Like 'lookAhead', but consume the input if @gea@ returns 'Right _'.
-- Fails if @gea@ fails.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE gea = do
    s <- get
    ea <- gea
    case ea of
        Left _ -> put s
        _      -> return ()
    return ea

-- | Get the next up to @n@ bytes as a lazy ByteString, without consuming them. 
uncheckedLookAhead :: Int64 -> Get L.ByteString
uncheckedLookAhead n = do
    S s ss _ <- get
    if n <= fromIntegral (B.length s)
        then return (L.fromChunks [B.take (fromIntegral n) s])
        else return $ L.take n (s `join` ss)

-- | Get the number of remaining unparsed bytes.
-- Useful for checking whether all input has been consumed.
-- Note that this forces the rest of the input.
remaining :: Get Int64
remaining = do
    S s ss _ <- get
    return (fromIntegral (B.length s) + L.length ss)

-- | Test whether all input has been consumed,
-- i.e. there are no remaining unparsed bytes.
isEmpty :: Get Bool
isEmpty = do
    S s ss _ <- get
    return (B.null s && L.null ss)

------------------------------------------------------------------------
-- Helpers

{-
-- | Fail if the ByteString does not have the right size.
takeExactly :: Int -> B.ByteString -> Get B.ByteString
takeExactly n bs
    | l == n    = return bs
    | otherwise = fail $ concat [ "Data.Binary.Get.takeExactly: Wanted "
                                , show n, " bytes, found ", show l, "." ]
  where l = B.length bs
{-# INLINE takeExactly #-}
-}

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get B.ByteString
getBytes n = do
    S s ss bytes <- get
    if n <= B.length s
        then do let (consume,rest) = B.splitAt n s
                put $! S rest ss (bytes + fromIntegral n)
                return $! consume
        else
              case L.splitAt (fromIntegral n) (s `join` ss) of
                (consuming, rest) ->
                    do let now = B.concat . L.toChunks $ consuming
                       put $! mkState rest (bytes + fromIntegral n)
                       -- forces the next chunk before this one is returned
                       when (B.length now < n) $
                         fail "too few bytes"
                       return now
{-# INLINE getBytes #-}
-- ^ important

join :: B.ByteString -> L.ByteString -> L.ByteString
join bb lb = L.fromChunks [bb] `L.append` lb
{-# INLINE join #-}

-- | Split a ByteString. If the first result is consumed before the --
-- second, this runs in constant heap space.
--
-- You must force the returned tuple for that to work, e.g.
-- 
-- > case splitAtST n xs of
-- >    (ys,zs) -> consume ys ... consume zs
--
splitAtST :: Int64 -> L.ByteString -> (L.ByteString, L.ByteString)
splitAtST i p        | i <= 0 = (L.empty, p)
splitAtST i (B.LPS ps) = runST (
     do r <- newSTRef undefined
        xs <- first r i ps
        ys <- unsafeInterleaveST (readSTRef r)
        return (B.LPS xs, B.LPS ys))
  where first r 0 xs     = writeSTRef r xs >> return []
        first r _ []     = writeSTRef r [] >> return []
        first r n (x:xs)
          | n < l     = do writeSTRef r (B.drop (fromIntegral n) x : xs)
                           return [B.take (fromIntegral n) x]
          | otherwise = do writeSTRef r (L.toChunks (L.drop (n - l) (B.LPS xs)))
                           fmap (x:) $ unsafeInterleaveST (first r (n - l) xs)
         where l = fromIntegral (B.length x) 
{-# INLINE splitAtST #-}

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value. If less than @n@ bytes are available, fail with an
-- error. This wraps @getBytes@.
readN :: Int -> (B.ByteString -> a) -> Get a
readN n f = fmap f $ getBytes n
{-# INLINE readN #-}
-- ^ important

------------------------------------------------------------------------

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input.
getByteString :: Int -> Get B.ByteString
getByteString n = readN n id
{-# INLINE getByteString #-}

-- | An efficient 'get' method for lazy ByteStrings. Does not fail if fewer than
-- @n@ bytes are left in the input.
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n = do
    S s ss bytes <- get
    let big = s `join` ss
    case splitAtST n big of
      (consume, rest) -> do put $ mkState rest (bytes + n)
                            return consume
{-# INLINE getLazyByteString #-}

------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp,o,_) <- readN n B.toForeignPtr
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

------------------------------------------------------------------------

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = getPtr (sizeOf (undefined :: Word8))
{-# INLINE getWord8 #-}

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 1))
{-# INLINE getWord16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 0) )
{-# INLINE getWord16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 3) )
{-# INLINE getWord32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 0) )
{-# INLINE getWord32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 7) )
{-# INLINE getWord64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 0) )
{-# INLINE getWord64le #-}

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

foreign import ccall unsafe "stg_uncheckedShiftL64"     
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
