{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}

-- CPP C style pre-precessing, the #if defined lines
-- RankNTypes forall r. statement
-- MagicHash the (# unboxing #), also needs GHC.primitives

module Data.Binary.Get.Internal (

    -- * The Get type
      Get
    , runCont
    , Result(..)
    , runGetPartial

    , readN
    , readNWith

    -- * Parsing
    , skip
    
    , get
    , put
    , demandInput
    , ensureN

    -- * Utility
    , remaining
    , getBytes
    , isEmpty

    -- ** ByteStrings
    , getByteString

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

-- Kolmodin 20100427: at zurihac we discussed of having partial take a
-- "Maybe ByteString" and implemented it in this way.
-- The reasoning was that you could accidently provide an empty bytestring,
-- and it should not terminate the parsing (empty would mean eof).
-- However, I'd say that it's also a risk that you get stuck in a loop,
-- where you keep providing an empty string. Anyway, no new input should be
-- rare, as the RTS should only wake you up if you actually have some data
-- to read from your fd.

-- | The result of parsing.
data Result a = Fail B.ByteString String
              -- ^ The parser ran into an error. The parser either used
              -- 'fail' or was not provided enough input.
              | Partial (Maybe B.ByteString -> Result a)
              -- ^ The parser has consumed the available input and needs
              -- more to continue. Provide 'Just' if more input is available
              -- and 'Nothing' otherwise, and you will get a new 'Result'.
              | Done B.ByteString a
              -- ^ The parser has successfully finished. Except for the
              -- output value you also get the unused input.

-- unrolled codensity/state monad
newtype Get a = C { runCont :: forall r.
                               B.ByteString ->
                               Success a r ->
                               Result    r }

type Success a r = B.ByteString -> a -> Result r

instance Monad Get where
  return = returnG
  (>>=) = bindG
  fail = failG

returnG :: a -> Get a
returnG a = C $ \s ks -> ks s a
{-# INLINE [0] returnG #-}

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $ \i ks -> c i (\i' a -> (runCont (f a)) i' ks)
{-# INLINE bindG #-}

failG :: String -> Get a
failG str = C $ \i _ks -> Fail i str

apG :: Get (a -> b) -> Get a -> Get b
apG d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE [0] apG #-}

fmapG :: (a -> b) -> Get a -> Get b
fmapG f m = C $ \i ks -> runCont m i (\i' a -> ks i' (f a))
{-# INLINE fmapG #-}

instance Applicative Get where
  pure = returnG
  {-# INLINE pure #-}
  (<*>) = apG
  {-# INLINE (<*>) #-}

instance Functor Get where
  fmap = fmapG

instance Functor Result where
  fmap f (Done s a) = Done s (f a)
  fmap f (Partial c) = Partial (\bs -> fmap f (c bs))
  fmap _ (Fail s msg) = Fail s msg

instance (Show a) => Show (Result a) where
  show (Fail _ msg) = "Fail: " ++ msg
  show (Partial _) = "Partial _"
  show (Done _ a) = "Done: " ++ show a

-- | Run a 'Get' monad. See 'Result' for what to do next, like providing
-- input, handling parser errors and to get the output value.
runGetPartial :: Get a -> Result a
runGetPartial g = noMeansNo $
  runCont g B.empty (\i a -> Done i a)

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

prompt :: B.ByteString -> Result a -> (B.ByteString -> Result a) -> Result a
prompt inp kf ks =
    let loop =
         Partial $ \sm ->
           case sm of
             Just s | B.null s -> loop
                    | otherwise -> ks (inp `B.append` s)
             Nothing -> kf
    in loop

-- | Demand more input. If none available, fail.
demandInput :: Get ()
demandInput = C $ \inp ks ->
  prompt inp (Fail inp "demandInput: not enough bytes") (\inp' -> ks inp' ())

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = readN n (const ())
{-# INLINE skip #-}

-- | Test whether all input has been consumed, i.e. there are no remaining
-- unparsed bytes.
isEmpty :: Get Bool
isEmpty = C $ \inp ks ->
    if B.null inp
      then prompt inp (ks inp True) (\inp' -> ks inp' False)
      else ks inp False

{-# DEPRECATED getBytes "Use 'getByteString' instead of 'getBytes'" #-}
getBytes :: Int -> Get B.ByteString
getBytes = getByteString
{-# INLINE getBytes #-}

instance Alternative Get where
  empty = C $ \inp ks -> Fail inp "Data.Binary.Get(Alternative).empty"
  (<|>) f g = C $ \inp ks ->
    let r0 = runCont (try f) inp (\inp' a -> Done inp' a)
        go r = case r of
                  Done inp' a -> ks inp' a
                  Partial f -> Partial (go . f)
                  Fail inp' str -> runCont g inp' ks
    in go r0

-- | Try to execute a Get. If it fails, the consumed input will be restored.
try :: Get a -> Get a
try g = C $ \inp ks ->
  let r0 = runGetPartial g `feed` inp
      go !acc r = case r of
                    Done inp' a -> ks inp' a
                    Partial f -> Partial $ \minp -> go (maybe acc (:acc) minp) (f minp)
                    Fail _ s -> Fail (B.concat (inp : reverse acc)) s
  in go [] r0
  where
  feed r inp =
    case r of
      Done inp0 a -> Done (inp0 `B.append` inp) a
      Partial f -> f (Just inp)
      Fail inp0 s -> Fail (inp0 `B.append` inp) s

-- | Get the number of bytes of remaining input. Note that this is an
-- expensive function to use as in order to calculate how much input
-- remains, all input has to be read and kept in-memory.
remaining :: Get Int64
remaining = C $ \ inp ks ->
  let loop acc = Partial $ \ minp ->
                  case minp of
                    Nothing -> let all_inp = B.concat (inp : (reverse acc))
                               in ks all_inp (fromIntegral $ B.length all_inp)
                    Just inp' -> loop (inp':acc)
  in loop []

------------------------------------------------------------------------
-- ByteStrings
--

-- | An efficient get method for strict ByteStrings. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getByteString :: Int -> Get B.ByteString
getByteString n | n > 0 = readN n (B.unsafeTake n)
                | otherwise = return B.empty
{-# INLINE getByteString #-}

-- | Get the current chunk.
get :: Get B.ByteString
get = C $ \inp ks -> ks inp inp

-- | Replace the current chunk.
put :: B.ByteString -> Get ()
put s = C $ \_inp ks -> ks s ()

-- | Return at least @n@ bytes, maybe more. If not enough data is available
-- the computation will escape with 'Partial'.
readN :: Int -> (B.ByteString -> a) -> Get a
readN !n f = ensureN n >> unsafeReadN n f
{-# INLINE [0] readN #-}

{-# RULES

"<$> to <*>" forall f g.
  (<$>) f g = returnG f <*> g

"readN/readN merge" forall n m f g.
  apG (readN n f) (readN m g) = readN (n+m) (\bs -> f bs $ g (B.unsafeDrop n bs))

"returnG/readN swap" [~1] forall f.
  returnG f = readN 0 (const f)

"readN 0/returnG swapback" [1] forall f.
  readN 0 f = returnG (f B.empty)
 #-}

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
ensureN :: Int -> Get ()
ensureN !n = C $ \inp ks -> do
  if B.length inp >= n
    then ks inp ()
    else runCont (go n) inp ks
  where -- might look a bit funny, but plays very well with GHC's inliner.
        -- GHC won't inline recursive functions, so we make ensureN non-recursive
    go n = C $ \inp ks -> do
      if B.length inp >= n
        then ks inp ()
        else runCont (demandInput >> go n) inp ks
{-# INLINE ensureN #-}

unsafeReadN :: Int -> (B.ByteString -> a) -> Get a
unsafeReadN !n f = C $ \inp ks -> do
  ks (B.unsafeDrop n inp) (f inp)
{- INLINE unsafeReadN -}

readNWith :: Int -> (Ptr a -> IO a) -> Get a
readNWith n f = do
    readN n $ \s -> B.inlinePerformIO $ B.unsafeUseAsCString s (f . castPtr)
{-# INLINE readNWith #-}
