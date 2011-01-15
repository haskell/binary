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
    , skip

    -- * Utility
    -- , bytesRead
    , remaining
    , getBytes
    , isEmpty
    , getS
    , putS
    , feed
    , eof

    , lookAhead

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

    , (<?>) -- labels

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
-- import GHC.Int
#endif

-- Kolmodin 20100427: at zurihack we discussed of having partial take a
-- "Maybe ByteString" and implemented it in this way.
-- The reasoning was that you could accidently provide an empty bytestring,
-- and it should not terminate the parsing (empty would mean eof).
-- However, I'd say that it's also a risk that you get stuck in a loop,
-- where you keep providing an empty string. Anyway, no new input should be
-- rare, as the RTS should only wake you up if you actually have some data
-- to read from your fd.

data Result a = Fail B.ByteString [String] String
              | Partial (Maybe B.ByteString -> Result a)
              | Done B.ByteString a

-- unrolled codensity/state monad
newtype Get a = C { runCont :: forall r.
                               B.ByteString ->
                               Failure   r ->
                               Success a r ->
                               Result    r }

type Failure   r = B.ByteString -> [String] -> String -> Result r
type Success a r = B.ByteString -> a -> Result r

instance Monad Get where
  return = returnG
  (>>=) = bindG
  fail = failG

(<?>) :: Get a -> String -> Get a
p <?> msg = C $ \inp kf ks -> runCont p inp (\inp' ss s -> kf inp' (msg:ss) s) ks

returnG :: a -> Get a
returnG a = C $ \s _kf ks -> ks s a
{-# INLINE returnG #-}

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $ \i kf ks -> c i kf (\i' a -> runCont (f a) i' kf ks)
{-# INLINE bindG #-}

failG :: String -> Get a
failG str = C $ \i kf _ks -> kf i [] ("failed reading:" ++ str)

apG :: Get (a -> b) -> Get a -> Get b
apG d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apG #-}

fmapG :: (a -> b) -> Get a -> Get b
fmapG f m = C $ \i kf ks -> runCont m i kf (\i' a -> ks i' (f a))
{-# INLINE fmapG #-}

instance Applicative Get where
  pure = returnG
  (<*>) = apG

instance Functor Get where
  fmap = fmapG

instance Functor Result where
  fmap f (Done s a) = Done s (f a)
  fmap f (Partial c) = Partial (\bs -> fmap f (c bs))
  fmap _ (Fail s stack msg) = Fail s stack msg

instance (Show a) => Show (Result a) where
  show (Fail _ msgs msg) = "Fail: " ++ show msgs ++ ": " ++ msg
  show (Partial _) = "Partial _"
  show (Done _ a) = "Done: " ++ show a

runGetPartial :: Get a -> Result a
runGetPartial g = noMeansNo $
  runCont g B.empty (\i stack msg -> Fail i stack msg) (\i a -> Done i a)

-- | Make sure we don't have to pass Nothing to a Partial twice.
-- This way we don't need to pass around an EOF value in the Get monad, it
-- can safely ask several times if it needs to.
noMeansNo :: Result a -> Result a
noMeansNo r0 = go r0
  where
  go r =
    case r of
      Partial f -> Partial $ \ms ->
                    case ms of
                      Just _ -> go (f ms)
                      Nothing -> neverAgain (f ms)
      _ -> r
  neverAgain r =
    case r of
      Partial f -> neverAgain (f Nothing)
      _ -> r

runGet :: Get a -> L.ByteString -> a
runGet g bs = feedAll (runGetPartial g) chunks
  where
  chunks = L.toChunks bs
  feedAll (Done _ r) _ = r
  feedAll (Partial c) (x:xs) = feedAll (c (Just x)) xs
  feedAll (Partial c) [] = feedAll (c Nothing) []
  feedAll (Fail _ _ msg) _ = error msg

feed :: Result a -> B.ByteString -> Result a
feed r inp =
  case r of
    Done inp0 a -> Done (inp0 `B.append` inp) a
    Partial f -> f (Just inp)
    Fail inp0 ss s -> Fail (inp0 `B.append` inp) ss s

eof :: Result a -> Result a
eof r =
  case r of
    Done _ _ -> r
    Partial f -> f Nothing
    Fail _ _ _ -> r
 
prompt :: B.ByteString -> Result a -> (B.ByteString -> Result a) -> Result a
prompt inp kf ks =
    let loop =
         Partial $ \sm ->
           case sm of
             Just s | B.null s -> loop
                    | otherwise -> ks (inp `B.append` s)
             Nothing -> kf
    in loop

-- | Need more data.
demandInput :: Get ()
demandInput = C $ \inp kf ks ->
  prompt inp (kf inp ["demandInput"] "not enough bytes") (\inp' -> ks inp' ())

skip :: Int -> Get ()
skip n = C $ \inp kf ks ->
  if B.length inp >= n
    then ks (B.unsafeDrop n inp) ()
    else runCont (demandInput >> skip (n - (B.length inp))) B.empty kf ks

isEmpty :: Get Bool
isEmpty = C $ \inp _kf ks ->
    if B.null inp
      then prompt inp (ks inp True) (`ks` False)
      else ks inp False

{-# DEPRECATED getBytes "Use 'getByteString' instead of 'getBytes'" #-}
getBytes :: Int -> Get B.ByteString
getBytes = getByteString

getS :: Get B.ByteString
getS = C $ \inp _kf ks -> ks inp inp

putS :: B.ByteString -> Get ()
putS inp = C $ \_inp _kf ks -> ks inp ()

lookAhead :: Get a -> Get a
lookAhead g = C $ \inp kf ks ->
  let r0 = runGetPartial (g <?> "lookAhead") `feed` inp
      go acc r = case r of
                    Done _ a -> ks (B.concat (inp : reverse acc)) a
                    Partial f -> Partial $ \minp -> go (maybe acc (:acc) minp) (f minp)
                    Fail inp' ss s -> kf inp' ss s
  in go [] r0

remaining :: Get Int
remaining = C $ \ inp kf ks ->
  let loop acc = Partial $ \ minp ->
                  case minp of
                    Nothing -> let all = B.concat (inp : (reverse acc)) in ks all (B.length all)
                    Just inp' -> loop (inp':acc)
  in loop []

------------------------------------------------------------------------
-- ByteStrings
--

getByteString :: Int -> Get B.ByteString
getByteString n = B.take n <$> readN n

remainingInCurrentChunk :: Get Int
remainingInCurrentChunk = C $ \inp _kf ks -> ks inp $! (B.length inp)

getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n0 =
  let loop n = do
        left <- remainingInCurrentChunk
        if fromIntegral left >= n
          then fmap (:[]) (getByteString (fromIntegral n))
          else do now <- getByteString left
                  demandInput
                  remaining <- loop (n - fromIntegral left)
                  return (now:remaining)
  in fmap L.fromChunks (loop n0)

-- | Return at least @n@ bytes, maybe more. If not enough data is available
-- it will escape with @Partial@.
readN :: Int -> Get B.ByteString
readN n = C $ \inp kf ks -> do
  if B.length inp >= n
    then ks (B.unsafeDrop n inp) inp
    else runCont (demandInput >> readN n) inp kf ks

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
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 1))


-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    s <- readN 2
    return $! (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    s <- readN 4
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    s <- readN 4
    return $! (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    s <- readN 8
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    s <- readN 8
    return $! (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

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
