{-# LANGUAGE CPP, RankNTypes, BangPatterns #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE UnboxedTuples #-}
#endif

module Data.Binary.Get.Internal (

    -- * The Get type
      Get
    , runCont
    , Decoder(..)
    , runGetIncremental

    , readN
    , readNWith

    , failGet

    -- * Parsing
    , bytesRead
    , isolate

    -- * With input chunks
    , withInputChunks
    , Consume
    , failOnEOF

    , get
    , put
    , ensureN

    -- * Utility
    , remaining
    , getBytes
    , isEmpty
    , lookAhead
    , lookAheadM
    , lookAheadE
    , label

    -- ** ByteStrings
    , getByteString

    ) where

import Foreign
import qualified Data.ByteString as B

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

import Data.Binary.Get.Internal.Buf

-- Kolmodin 20100427: at zurihac we discussed of having partial take a
-- "Maybe ByteString" and implemented it in this way.
-- The reasoning was that you could accidentally provide an empty bytestring,
-- and it should not terminate the decoding (empty would mean eof).
-- However, I'd say that it's also a risk that you get stuck in a loop,
-- where you keep providing an empty string. Anyway, no new input should be
-- rare, as the RTS should only wake you up if you actually have some data
-- to read from your fd.

-- | A decoder produced by running a 'Get' monad.
data Decoder a = Fail !B.ByteString String
              -- ^ The decoder ran into an error. The decoder either used
              -- 'fail' or was not provided enough input.
              | Partial (Maybe B.ByteString -> Decoder a)
              -- ^ The decoder has consumed the available input and needs
              -- more to continue. Provide 'Just' if more input is available
              -- and 'Nothing' otherwise, and you will get a new 'Decoder'.
              | Done !B.ByteString a
              -- ^ The decoder has successfully finished. Except for the
              -- output value you also get the unused input.
              | BytesRead {-# UNPACK #-} !Int64 (Int64 -> Decoder a)
              -- ^ The decoder needs to know the current position in the input.
              -- Given the number of bytes remaining in the decoder, the outer
              -- decoder runner needs to calculate the position and
              -- resume the decoding.

doneBuf :: Buf -> a -> Decoder a
doneBuf s a = Done (packBuf s) a

------------------------------------------------------------------------
-- The Get monad

-- unrolled codensity/state monad
newtype Get a = C { unGet :: forall r.
                             Buf ->
                             Success a r ->
                             Decoder r }

type Success a r = Buf -> a -> Decoder r

runCont :: Get a -> B.ByteString -> (B.ByteString -> a -> Decoder r) -> Decoder r
runCont (C c) bs ks = c (unpackBuf bs) (\s' a -> ks (packBuf s') a)
{-# INLINE runCont #-}

fromCont :: (forall r. B.ByteString -> (B.ByteString -> a -> Decoder r) -> Decoder r) -> Get a
fromCont f = C $ \s ks -> f (packBuf s) (\bs' a -> ks (unpackBuf bs') a)
{-# INLINE fromCont #-}

instance Monad Get where
  return = pure
  (>>=) = bindG
#if !(MIN_VERSION_base(4,9,0))
  fail = failGet -- base < 4.9
#elif !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail -- base < 4.13
#endif
-- NB: Starting with base-4.13, the `fail` method
--     has been removed from the `Monad`-class
--     according to the MonadFail proposal (MFP) schedule
--     which completes the process that started with base-4.9.

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail Get where
  fail = failGet
#endif

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $ \s ks -> c s (\s' a -> unGet (f a) s' ks)
{-# INLINE bindG #-}

-- | Let the 'Get' action fail with an error message.
--
-- > failGet = fail @Get
--
-- @since x.x.x.x
failGet :: String -> Get a
failGet str = C $ \s _ks -> Fail (packBuf s) str

apG :: Get (a -> b) -> Get a -> Get b
apG d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apG #-}

fmapG :: (a -> b) -> Get a -> Get b
fmapG f (C m) = C $ \s ks -> m s (\s' a -> ks s' (f a))
{-# INLINE fmapG #-}

instance Applicative Get where
  pure = \x -> C $ \s ks -> ks s x
  {-# INLINE pure #-}
  (<*>) = apG
  {-# INLINE (<*>) #-}

-- | @since 0.7.1.0
instance MonadPlus Get where
  mzero = empty
  mplus = (<|>)

instance Functor Get where
  fmap = fmapG

instance Functor Decoder where
  fmap f (Done s a) = Done s (f a)
  fmap f (Partial k) = Partial (fmap f . k)
  fmap _ (Fail s msg) = Fail s msg
  fmap f (BytesRead b k) = BytesRead b (fmap f . k)

instance (Show a) => Show (Decoder a) where
  show (Fail _ msg) = "Fail: " ++ msg
  show (Partial _) = "Partial _"
  show (Done _ a) = "Done: " ++ show a
  show (BytesRead _ _) = "BytesRead"

-- | Run a 'Get' monad. See 'Decoder' for what to do next, like providing
-- input, handling decoding errors and to get the output value.
runGetIncremental :: Get a -> Decoder a
runGetIncremental g = noMeansNo $
  runCont g B.empty (\i a -> Done i a)

-- | Make sure we don't have to pass Nothing to a Partial twice.
-- This way we don't need to pass around an EOF value in the Get monad, it
-- can safely ask several times if it needs to.
noMeansNo :: Decoder a -> Decoder a
noMeansNo r0 = go r0
  where
  go r =
    case r of
      Partial k -> Partial $ \ms ->
                    case ms of
                      Just _ -> go (k ms)
                      Nothing -> neverAgain (k ms)
      BytesRead n k -> BytesRead n (go . k)
      Done _ _ -> r
      Fail _ _ -> r
  neverAgain r =
    case r of
      Partial k -> neverAgain (k Nothing)
      BytesRead n k -> BytesRead n (neverAgain . k)
      Fail _ _ -> r
      Done _ _ -> r

prompt :: B.ByteString -> Decoder a -> (B.ByteString -> Decoder a) -> Decoder a
prompt inp kf ks = prompt' kf (\inp' -> ks (inp `B.append` inp'))

prompt' :: Decoder a -> (B.ByteString -> Decoder a) -> Decoder a
prompt' kf ks =
  let loop =
        Partial $ \sm ->
          case sm of
            Just s | B.null s -> loop
                   | otherwise -> ks s
            Nothing -> kf
  in loop

-- | Get the total number of bytes read to this point.
bytesRead :: Get Int64
bytesRead = C $ \s ks -> BytesRead (fromIntegral (bufLength s)) (ks s)

-- | Isolate a decoder to operate with a fixed number of bytes, and fail if
-- fewer bytes were consumed, or more bytes were attempted to be consumed.
-- If the given decoder fails, 'isolate' will also fail.
-- Offset from 'bytesRead' will be relative to the start of 'isolate', not the
-- absolute of the input.
--
-- @since 0.7.2.0
isolate :: Int   -- ^ The number of bytes that must be consumed
        -> Get a -- ^ The decoder to isolate
        -> Get a
isolate n0 act
  | n0 < 0 = fail "isolate: negative size"
  | otherwise = go n0 (runCont act B.empty Done)
  where
  go !n (Done left x)
    | n == 0 && B.null left = return x
    | otherwise = do
        pushFront left
        let consumed = n0 - n - B.length left
        fail $ "isolate: the decoder consumed " ++ show consumed ++ " bytes" ++
                 " which is less than the expected " ++ show n0 ++ " bytes"
  go 0 (Partial resume) = go 0 (resume Nothing)
  go n (Partial resume) = do
    inp <- fromCont $ \inp k -> do
      let takeLimited str =
            let (inp', out) = B.splitAt n str
            in k out (Just inp')
      case not (B.null inp) of
        True -> takeLimited inp
        False -> prompt inp (k B.empty Nothing) takeLimited
    case inp of
      Nothing -> go n (resume Nothing)
      Just str -> go (n - B.length str) (resume (Just str))
  go _ (Fail bs err) = pushFront bs >> fail err
  go n (BytesRead r resume) =
    go n (resume $! fromIntegral n0 - fromIntegral n - r)

type Consume s = s -> B.ByteString -> Either s (B.ByteString, B.ByteString)

withInputChunks :: s -> Consume s -> ([B.ByteString] -> b) -> ([B.ByteString] -> Get b) -> Get b
withInputChunks initS consume onSucc onFail = go initS []
  where
  go state acc = fromCont $ \inp ks ->
    case consume state inp of
      Left state' -> do
        let acc' = inp : acc
        prompt'
          (runCont (onFail (reverse acc')) B.empty ks)
          (\str' -> runCont (go state' acc') str' ks)
      Right (want,rest) -> do
        ks rest (onSucc (reverse (want:acc)))

failOnEOF :: [B.ByteString] -> Get a
failOnEOF bs = C $ \_ _ -> Fail (B.concat bs) "not enough bytes"

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
isEmpty :: Get Bool
isEmpty = C $ \s ks ->
    if bufNull s
      then prompt B.empty (ks s True) (\inp' -> ks (unpackBuf inp') False)
      else ks s False
{-# INLINE isEmpty #-}

-- | DEPRECATED. Same as 'getByteString'.
{-# DEPRECATED getBytes "Use 'getByteString' instead of 'getBytes'." #-}
getBytes :: Int -> Get B.ByteString
getBytes = getByteString
{-# INLINE getBytes #-}

-- | @since 0.7.0.0
instance Alternative Get where
  empty = C $ \s _ks -> Fail (packBuf s) "Data.Binary.Get(Alternative).empty"
  {-# INLINE empty #-}
  (<|>) f g = C $ \s ks ->
    case unGet f s doneBuf of
      Done inp' a -> ks (unpackBuf inp') a
      Fail _ _    -> unGet g s ks
      r0          -> altSlow (packBuf s) g ks r0
  {-# INLINE (<|>) #-}
  some p = (:) <$> p <*> many p
  {-# INLINE some #-}
  many p = do
    v <- (Just <$> p) <|> pure Nothing
    case v of
      Nothing -> pure []
      Just x -> (:) x <$> many p
  {-# INLINEABLE many #-} -- many will never inline because it's recursive, so mark it INLINEABLE instead.

altSlow :: B.ByteString -> Get a -> Success a r -> Decoder a -> Decoder r
altSlow inp g ks = go []
  where
    go !acc r = case r of
                  Done inp' a        -> ks (unpackBuf inp') a
                  Fail _ _           -> unGet g (unpackBuf (concatChunks inp acc)) ks
                  Partial k          -> Partial $ \minp -> go (maybe acc (:acc) minp) (k minp)
                  BytesRead unused k -> BytesRead unused (go acc . k)
{-# NOINLINE altSlow #-}

concatChunks :: B.ByteString -> [B.ByteString] -> B.ByteString
concatChunks inp []  = inp
concatChunks inp [c] = if B.null inp then c else B.append inp c
concatChunks inp acc = B.concat (if B.null inp then reverse acc else inp : reverse acc)
{-# INLINE concatChunks #-}

pushFront :: B.ByteString -> Get ()
pushFront bs = C $ \s ks ->
  if B.null bs
    then ks s ()
    else ks (unpackBuf (B.append bs (packBuf s))) ()
{-# INLINE pushFront #-}

-- | Run the given decoder, but without consuming its input. If the given
-- decoder fails, then so will this function.
--
-- @since 0.7.0.0
lookAhead :: Get a -> Get a
lookAhead g = C $ \s ks ->
  case unGet g s doneBuf of
    Done _ a    -> ks s a
    Fail inp' e -> Fail inp' e
    r0          -> lookAheadSlow (packBuf s) ks r0
{-# INLINE lookAhead #-}

lookAheadSlow :: B.ByteString -> Success a r -> Decoder a -> Decoder r
lookAheadSlow inp ks = go []
  where
    go !acc r = case r of
                  Done _ a           -> ks (unpackBuf (concatChunks inp acc)) a
                  Fail inp' s'       -> Fail inp' s'
                  Partial k          -> Partial $ \minp -> go (maybe acc (:acc) minp) (k minp)
                  BytesRead unused k -> BytesRead unused (go acc . k)
{-# NOINLINE lookAheadSlow #-}

-- | Run the given decoder, and only consume its input if it returns 'Just'.
-- If 'Nothing' is returned, the input will be unconsumed.
-- If the given decoder fails, then so will this function.
--
-- @since 0.7.0.0
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM g = do
  let g' = maybe (Left ()) Right <$> g
  either (const Nothing) Just <$> lookAheadE g'

-- | Run the given decoder, and only consume its input if it returns 'Right'.
-- If 'Left' is returned, the input will be unconsumed.
-- If the given decoder fails, then so will this function.
--
-- @since 0.7.1.0
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE g = C $ \s ks ->
  case unGet g s doneBuf of
    Done _ (Left x)     -> ks s (Left x)
    Done inp' (Right x) -> ks (unpackBuf inp') (Right x)
    Fail inp' e         -> Fail inp' e
    r0                  -> lookAheadESlow (packBuf s) ks r0
{-# INLINE lookAheadE #-}

lookAheadESlow :: B.ByteString -> Success (Either a b) r -> Decoder (Either a b) -> Decoder r
lookAheadESlow inp ks = go []
  where
    go !acc r = case r of
                  Done _ (Left x)     -> ks (unpackBuf (concatChunks inp acc)) (Left x)
                  Done inp' (Right x) -> ks (unpackBuf inp') (Right x)
                  Fail inp' e         -> Fail inp' e
                  Partial k           -> Partial $ \minp -> go (maybe acc (:acc) minp) (k minp)
                  BytesRead unused k  -> BytesRead unused (go acc . k)
{-# NOINLINE lookAheadESlow #-}

-- | Label a decoder. If the decoder fails, the label will be appended on
-- a new line to the error message string.
--
-- @since 0.7.2.0
label :: String -> Get a -> Get a
label msg decoder = C $ \s ks ->
  case unGet decoder s doneBuf of
    Done inp' a -> ks (unpackBuf inp') a
    Fail inp' e -> Fail inp' (e ++ "\n" ++ msg)
    r0          -> labelSlow msg ks r0
{-# INLINE label #-}

labelSlow :: String -> Success a r -> Decoder a -> Decoder r
labelSlow msg ks = go
  where
    go r = case r of
             Done inp' a   -> ks (unpackBuf inp') a
             Partial k     -> Partial (go . k)
             Fail inp' e   -> Fail inp' (e ++ "\n" ++ msg)
             BytesRead u k -> BytesRead u (go . k)
{-# NOINLINE labelSlow #-}

-- | DEPRECATED. Get the number of bytes of remaining input.
-- Note that this is an expensive function to use as in order to calculate how
-- much input remains, all input has to be read and kept in-memory.
-- The decoder keeps the input as a strict bytestring, so you are likely better
-- off by calculating the remaining input in another way.
{-# DEPRECATED remaining "This will force all remaining input, don't use it." #-}
remaining :: Get Int64
remaining = fromCont $ \inp ks ->
  let loop acc = Partial $ \ minp ->
                  case minp of
                    Nothing -> let all_inp = B.concat (inp : (reverse acc))
                               in ks all_inp (fromIntegral $ B.length all_inp)
                    Just inp' -> loop (inp':acc)
  in loop []

------------------------------------------------------------------------
-- ByteStrings
--

-- | @getByteString n@ efficiently gets a strict `Data.ByteString.ByteString` of length @n@.
-- Fails if fewer than @n@ bytes are left in the input.
-- If @n <= 0@, the empty string is returned.
getByteString :: Int -> Get B.ByteString
getByteString n
  | n > 0 = C $ \s ks ->
      if bufHasN n s
        then ks (bufDrop n s) (bufTake n s)
        else unGet (ensureNSlow n >> C (\s' ks' -> ks' (bufDrop n s') (bufTake n s'))) s ks
  | otherwise = return B.empty
{-# INLINE getByteString #-}

-- | Get the current chunk.
get :: Get B.ByteString
get = C $ \s ks -> ks s (packBuf s)

-- | Replace the current chunk.
put :: B.ByteString -> Get ()
put bs = C $ \_ ks -> ks (unpackBuf bs) ()

-- | Return at least @n@ bytes, maybe more. If not enough data is available
-- the computation will escape with 'Partial'.
readN :: Int -> (B.ByteString -> a) -> Get a
readN !n f = C $ \s ks ->
  if bufHasN n s
    then ks (bufDrop n s) $! f (packBuf s)
    else unGet (ensureNSlow n >> unsafeReadN n f) s ks
{-# INLINE readN #-}

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
ensureN :: Int -> Get ()
ensureN !n0 = C $ \s ks ->
  if bufHasN n0 s
    then ks s ()
    else unGet (ensureNSlow n0) s ks
{-# INLINE ensureN #-}

ensureNSlow :: Int -> Get ()
ensureNSlow !n0 = withInputChunks n0 enoughChunks onSucc onFail >>= put
  where -- might look a bit funny, but plays very well with GHC's inliner.
        -- GHC won't inline recursive functions, so we make ensureN non-recursive
    enoughChunks n str
      | B.length str >= n = Right (str,B.empty)
      | otherwise = Left (n - B.length str)
    -- Sometimes we will produce leftovers lists of the form [B.empty, nonempty]
    -- where `nonempty` is a non-empty ByteString. In this case we can avoid a copy
    -- by simply dropping the empty prefix. In principle ByteString might want
    -- to gain this optimization as well
    onSucc = B.concat . dropWhile B.null
    onFail bss = C $ \_ _ -> Fail (B.concat bss) "not enough bytes"
{-# NOINLINE ensureNSlow #-}

unsafeReadN :: Int -> (B.ByteString -> a) -> Get a
unsafeReadN !n f = C $ \s ks ->
  ks (bufDrop n s) $! f (packBuf s)

-- | @readNWith n f@ where @f@ must be deterministic and not have side effects.
readNWith :: Int -> (Ptr a -> IO a) -> Get a
readNWith !n f = C $ \s ks ->
  if bufHasN n s
    then ks (bufDrop n s) $! bufReadWith f s
    else unGet (ensureNSlow n >> C (\s' ks' -> ks' (bufDrop n s') $! bufReadWith f s')) s ks
{-# INLINE readNWith #-}
