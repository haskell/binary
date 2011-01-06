{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}

-- CPP C style pre-precessing, the #if defined lines
-- RankNTypes forall r. statement
-- MagicHash the (# unboxing #), also needs GHC.primitives

module Data.Binary.Get (

    -- * The Get type
      Get
    , Result(..)
    , runGet
    , runGetPartial

    -- * Parsing
    , try
    , plus
    , skip

    -- * Utility
    -- , bytesRead
    -- , remaining
    , getBytes
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

data S = S { input      :: !B.ByteString
           , next_input :: !B.ByteString
           , read_all   :: !Bool
           } deriving Show

-- Kolmodin 20100427: at zurihack we discussed of having partial take a
-- "Maybe ByteString" and implemented it in this way.
-- The reasoning was that you could accidently provide an empty bytestring,
-- and it should not terminate the parsing (empty would mean eof).
-- However, I'd say that it's also a risk that you get stuck in a loop,
-- where you keep providing an empty string. Anyway, no new input should be
-- rare, as the RTS should only wake you up if you actually have some data
-- to read from your fd.

data Result a = Fail S [String] String
              | Partial (Maybe B.ByteString -> Result a)
              | Done S a

-- unrolled codensity/state monad
newtype Get a = C { runCont :: forall r.
                               S -> 
                               Failure   r ->
                               Success a r ->
                               Result    r }

type Failure   r = S -> [String] -> String -> Result r
type Success a r = S -> a -> Result r

instance Monad Get where
  return = returnG
  (>>=) = bindG
  fail = failG

returnG :: a -> Get a
returnG a = C $ \s kf ks -> ks s a
{-# INLINE returnG #-}

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $ \s kf ks -> c s kf (\s' a -> runCont (f a) s' kf ks)
{-# INLINE bindG #-}

failG :: String -> Get a
failG str = C $ \st0 kf _ks -> kf st0 [] ("failed reading:" ++ str)

plus :: Get a -> Get a -> Get a
plus a b = C $ \st0 kf ks ->
  let kf' st2 _ _ = runCont b (addS st0 st2) kf ks
      !st1 = noNext st0
  in runCont a st1 kf' ks
{-# INLINE plus #-}  

apG :: Get (a -> b) -> Get a -> Get b
apG d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apG #-}

fmapG :: (a -> b) -> Get a -> Get b
fmapG f m = C $ \s0 kf ks -> runCont m s0 kf (\s a -> ks s (f a))
{-# INLINE fmapG #-}

instance Applicative Get where
  pure = returnG
  (<*>) = apG

instance Functor Get where
  fmap = fmapG

instance Functor Result where
  fmap f (Done s a) = Done s (f a)
  fmap f (Partial c) = Partial (\bs -> fmap f (c bs))
  fmap f (Fail s stack msg) = Fail s stack msg

instance (Show a) => Show (Result a) where
  show (Fail _ _ msg) = "Fail: " ++ msg
  show (Partial _) = "Partial _"
  show (Done _ a) = "Done: " ++ show a

initState = S B.empty B.empty False

runGetPartial :: Get a -> Result a
runGetPartial g = runCont g initState (\s stack msg -> Fail s stack msg) (\s a -> Done s a)

addS :: S -> S -> S
addS (S inp0 next0 eof0) (S _inp1 next1 eof1) = S (inp0 +++ next1) (next0 +++ next1) (eof0 || eof1)
{-# INLINE addS #-}

noNext :: S -> S
noNext (S inp _next0 eof) = S inp B.empty eof
{-# INLINE noNext #-}

(+++) :: B.ByteString -> B.ByteString -> B.ByteString
(+++) = B.append
{-# INLINE (+++) #-}

try :: Get a -> Get a
try g = C $ \st0 kf ks ->
  runCont g (noNext st0) (kf . addS st0) ks

runGet :: Get a -> L.ByteString -> a
runGet g bs = feed (runGetPartial g) chunks
  where
  chunks = L.toChunks bs
  feed (Done _ r) _ = r
  feed r@(Partial c) (x:xs) = feed (c (Just x)) xs 
  feed r@(Partial c) [] = feed (c Nothing) []
  feed (Fail _ _ msg) _ = error msg
 
-- | Need more data.
needMore :: Get ()
needMore = C $ \st0@(S inp next eof) kf ks -> 
  if eof then kf st0 [] "not enough bytes"
    else
      let loop = 
            Partial $ \sm -> 
              case sm of
                Nothing -> kf (S inp next True) [] "not enough bytes"
                Just s -> if B.null s
                            then loop -- attention: don't get stuck forever!
                            else ks (S (B.append inp s) (B.append next s) eof) ()
      in loop

skip :: Int -> Get ()
skip n = C $ \s@(S inp next eof) kf ks ->
  if B.length inp >= n
    then ks (S (B.unsafeDrop n inp) next eof) ()
    else runCont (needMore >> skip (n - (B.length inp))) (S B.empty next eof) kf ks


{-# DEPRECATED getBytes "Use 'getByteString' instead of 'getBytes'" #-}
getBytes :: Int -> Get B.ByteString
getBytes = getByteString

getS :: Get B.ByteString
getS = C $ \st@(S inp next eof) kf ks -> ks st inp

putS :: B.ByteString -> Get ()
putS inp = C $ \(S _inp next eof) kf ks -> ks (S inp next eof) ()

------------------------------------------------------------------------
-- ByteStrings
--

getByteString :: Int -> Get B.ByteString
getByteString n = B.take n <$> readN n

remainingInCurrentChunk :: Get Int
remainingInCurrentChunk = C $ \st@(S inp _ _) kf ks -> ks st (B.length inp)

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
readN n = C $ \ st0@(S inp next eof) kf ks -> do
  if B.length inp >= n
    then ks (S (B.unsafeDrop n inp) next eof) inp
    else runCont (needMore >> readN n) st0 kf ks

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
