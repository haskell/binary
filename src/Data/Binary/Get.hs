{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}

-- CPP C style pre-precessing, the #if defined lines
-- RankNTypes forall r. statement
-- MagicHash the (# unboxing #), also needs GHC.primitives

module Data.Binary.Get (

    -- * The Get type
      Get
    , Result(..)
    , runGet
    , runGetPush

    -- * Parsing
    -- , skip

    -- * Utility
    -- , bytesRead
    -- , remaining
    -- , isEmpty

    -- * Parsing particular types
    , getWord8

    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    -- , getLazyByteStringNul
    -- , getRemainingLazyByteString

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

import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
-- needed for (# unboxing #) with magic hash
import GHC.Base
import GHC.Word
import GHC.Int
#endif

newtype S = S B.ByteString deriving Show

data Result a = Fail S String
              | Partial (Maybe B.ByteString -> Result a)
              | Done S a

-- unrolled codensity/state monad
newtype Get a = C { runCont :: forall r. S -> (S -> a -> Result r) -> Result r }

instance Monad Get where
  return = returnG
  (>>=) = bindG

returnG :: a -> Get a
returnG a = C $ \s k -> k s a

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $ \s k -> c s (\s' a -> runCont (f a) s' k)

apG :: Get (a -> b) -> Get a -> Get b
apG d e = do
  b <- d
  a <- e
  return (b a)

fmapG :: (a -> b) -> Get a -> Get b
fmapG f m = C $ \s0 k -> runCont m s0 (\s a -> k s (f a))

instance Applicative Get where
  pure = returnG
  (<*>) = apG

instance Functor Get where
  fmap = fmapG

instance Functor Result where
  fmap f (Done s a) = Done s (f a)
  fmap f (Partial c) = Partial (\bs -> fmap f (c bs))
  fmap f (Fail s msg) = Fail s msg

instance (Show a) => Show (Result a) where
  show (Fail _ msg) = "Fail: " ++ msg
  show (Partial _) = "Partial _"
  show (Done _ a) = "Done: " ++ show a

runGetPush :: Get a -> Result a
runGetPush g = runCont g (S B.empty) (\s a -> Done s a)

runGet :: Get a -> L.ByteString -> a
runGet g bs = feed (runGetPush g) chunks
  where
  chunks = L.toChunks bs
  feed (Done _ r) _ = r
  feed r@(Partial c) (x:xs) = feed (c (Just x)) xs 
  feed r@(Partial c) [] = feed (c Nothing) []
  feed (Fail _ msg) _ = error msg
 
-- | Need more data, at least @n@ bytes.
needMore :: Get ()
needMore = C $ \st0@(S s) k -> 
  Partial $ \sm -> 
    case sm of
      Nothing -> Fail st0 "not enough bytes"
      Just s' -> k (S (B.append s s')) ()

getS :: Get B.ByteString
getS = C $ \st0@(S s) k -> k st0 s

setS :: B.ByteString -> Get ()
setS s = C $ \_ k -> k (S s) ()

------------------------------------------------------------------------
-- ByteStrings
--

getByteString :: Int -> Get B.ByteString
getByteString n = B.take n <$> readN n

remainingInCurrentChunk :: Get Int
remainingInCurrentChunk = C $ \(S s) k -> k (S s) (B.length s)

getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n0 =
  let loop n = do
        left <- remainingInCurrentChunk
        if fromIntegral left >= n
          then fmap (:[]) (getByteString (fromIntegral n))
          else do now <- getByteString left
                  needMore
                  remaining <- loop (n - fromIntegral left)
                  return (now:remaining)
  in fmap L.fromChunks (loop n0)

-- | Return at least @n@ bytes, maybe more. If not enough data is available
-- it will escape with @Partial@.
readN :: Int -> Get B.ByteString
readN n = C $ \ st0@(S s) k -> do
  if B.length s >= n
    then k (S (B.unsafeDrop n s)) s
    else runCont (needMore >> readN n) (S s) k

------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp,o,_) <- fmap B.toForeignPtr (readN n)
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = do
  s <- readN 1
  return $! B.unsafeHead s

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = do
    s <- readN 2
    return $! (fromIntegral (s `B.index` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 1))

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    s <- readN 2
    return $! (fromIntegral (s `B.index` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 0) )

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    s <- readN 4
    return $! (fromIntegral (s `B.index` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 3) )

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    s <- readN 4
    return $! (fromIntegral (s `B.index` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 0) )

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    s <- readN 8
    return $! (fromIntegral (s `B.index` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 7) )

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    s <- readN 8
    return $! (fromIntegral (s `B.index` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 0) )

------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get Word
getWordhost = getPtr (sizeOf (undefined :: Word))

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Get Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Get Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Get Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))

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
