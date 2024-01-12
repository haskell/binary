{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns, TypeFamilies, UnboxedTuples#-}

-- CPP C style pre-precessing, the #if defined lines
-- RankNTypes forall r. statement
-- MagicHash the (# unboxing #), also needs GHC.primitives

module Data.Binary.Get.Internal (

    -- * The Get type
      ByteOffset
    , Resupply (..)
    , Decoder(..)
    , Location (..)
    , Policy (..)
    , Rollback
    , Success
    , Failure
    , Get (..)

    -- **
    , Result (..)

    -- * Runners

    -- ** Pure
    , parse
    , initiate
    , create

    -- ** Pre-@0.9.1@
    , runGet
    , runGetOrFail
    , runGetIncremental

    -- ** Pre-@0.6@
    , runGetState

    -- * Parsing
    , bytesRead
    , isolate
    , unsafeIsolate

    -- * Utility
    , remaining
    , getBytes
    , isEmpty
    , lookAhead
    , lookAheadM
    , lookAheadE
    , label
    , skip
    , unsafeSkip

    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    , getLazyByteStringNul
    , getRemainingLazyByteString

    , unsafeGetByteString
    , unsafeGetLazyByteString

    -- ** Fast unsafe access
    , unsafeRead
    , accursedRead

    ) where

import Foreign
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L (ByteString (..), chunk)
import qualified Data.ByteString.Unsafe as B

import Control.Applicative
import Control.Exception (Exception, throw)
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

import Data.Binary.Internal ( accursedUnutterablePerformIO )

-- Kolmodin 20100427: at zurihac we discussed of having partial take a
-- "Maybe ByteString" and implemented it in this way.
-- The reasoning was that you could accidently provide an empty bytestring,
-- and it should not terminate the decoding (empty would mean eof).
-- However, I'd say that it's also a risk that you get stuck in a loop,
-- where you keep providing an empty string. Anyway, no new input should be
-- rare, as the RTS should only wake you up if you actually have some data
-- to read from your fd.

-- | An offset, counted in bytes.
type ByteOffset = Int64

-- | Providing additional input to the decoder.
data Resupply = Supply
                  B.ByteString
                    -- ^ The first chunk of the input. It __should not__ be empty.
                    --
                    --   N.B.: 'L.ByteString''s have an internal "no empty chunks"
                    --   invariant. While this parser does not malfunction when
                    --   encountering an empty chunk, it does not purge empty chunks
                    --   and will faithfully relay them in 'L.ByteString's it produces.
                    --   This may in turn break downstream consumers.

                  L.ByteString -- ^ Remaining input chunks.

              | EndOfInput

-- | A decoder produced by running a 'Get' monad.
data Decoder a = Fail L.ByteString {-# UNPACK #-} !ByteOffset String
              -- ^ The decoder ran into an error. The decoder either used
              -- 'fail' or was not provided enough input.
              | Partial (Resupply -> Decoder a)
              -- ^ The decoder has consumed the available input and needs
              -- more to continue. Provide 'Supply' if more input is available
              -- and 'EndOfInput' otherwise, and you will get a new 'Decoder'.
              | Done L.ByteString {-# UNPACK #-} !ByteOffset a
              -- ^ The decoder has successfully finished. Except for the
              -- output value you also get the unused input.

-- | Offset of the start of the current chunk, in bytes.
type TotalOffset = Int64

-- | Offset inside the chunk, in bytes. Can be equal to the length of the chunk.
type ChunkOffset = Int

-- | Whether the parser is inside an alternative or not.
data Location = Free   -- ^ Default state.
              | Choice -- ^ Inside the left argument of an alternative.
                deriving Show

-- | Chunk retention policy.
data Policy = Drop -- ^ Do not keep the reference.
            | Keep -- ^ Keep the reference in the 'Rollback'.
              deriving Show

-- | A difference list of all the consumed chunks we may need in the future.
type Rollback = L.ByteString -> L.ByteString

-- | Whether more input can be supplied.
data More = More -- ^ Can prompt for more state.
          | NoMore  -- ^ NoMore has been reached.
            deriving Show

type Success a r = TotalOffset
                -> ChunkOffset
                -> B.ByteString
                -> L.ByteString
                -> More
                -> Location
                -> Policy
                -> Rollback
                -> a
                -> Decoder r

type Failure r = TotalOffset
              -> ChunkOffset
              -> B.ByteString
              -> L.ByteString
              -> More
              -> Rollback
              -> String
              -> Decoder r

newtype Get a =
          C { runCont
                :: forall r.
                   TotalOffset
                -> ChunkOffset
                -> B.ByteString
                -> L.ByteString
                -> More
                -> Location
                -> Policy
                -> Rollback
                -> Success a r
                -> Failure r
                -> Decoder r
            }

instance Monad Get where
  return = pure
  {-# INLINE return #-}
  (>>=) = bindG
  {-# INLINE (>>=) #-}
#if !(MIN_VERSION_base(4,9,0))
  fail = failG -- base < 4.9
#elif !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail -- base < 4.13
#endif
-- NB: Starting with base-4.13, the `fail` method
--     has been removed from the `Monad`-class
--     according to the MonadFail proposal (MFP) schedule
--     which completes the process that started with base-4.9.

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail Get where
  fail = failG
  {-# INLINE fail #-}
#endif

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f =
  C $ \i o bs lbs more loc pol roll yes no ->
    c i o bs lbs more loc pol roll
      ( \i' o' bs' lbs' more' loc' pol' roll' a ->
          runCont (f a) i' o' bs' lbs' more' loc' pol' roll' yes no
      ) no
{-# INLINE bindG #-}

failG :: String -> Get a
failG str = C $ \i o bs lbs more _loc _pol roll _yes no -> no i o bs lbs more roll str
{-# INLINE failG #-}

apG :: Get (a -> b) -> Get a -> Get b
apG d e = do
  b <- d
  a <- e
  pure (b a)
{-# INLINE apG #-}

fmapG :: (a -> b) -> Get a -> Get b
fmapG f m =
  C $ \i o bs lbs more loc pol roll yes ->
    runCont m i o bs lbs more loc pol roll $ \i' o' bs' lbs' more' loc' pol' roll' a ->
      yes i' o' bs' lbs' more' loc' pol' roll' (f a)
{-# INLINE fmapG #-}

instance Applicative Get where
  pure = \x -> C $ \i o bs lbs more loc pol roll yes _no ->
                 yes i o bs lbs more loc pol roll x
  {-# INLINE pure #-}
  (<*>) = apG
  {-# INLINE (<*>) #-}

-- | @since 0.7.1.0
instance MonadPlus Get where
  mzero = empty
  {-# INLINE mzero #-}

  mplus = (<|>)
  {-# INLINE mplus #-}

instance Functor Get where
  fmap = fmapG
  {-# INLINE fmap #-}

instance Functor Decoder where
  fmap f (Done s i a) = Done s i (f a)
  fmap f (Partial k) = Partial (fmap f . k)
  fmap _ (Fail s i msg) = Fail s i msg

instance (Show a) => Show (Decoder a) where
  show (Fail _ _ msg) = "Fail: " ++ msg
  show (Partial _) = "Partial _"
  show (Done _ _ a) = "Done: " ++ show a



data Result a = Success L.ByteString {-# UNPACK #-} !ByteOffset a
              | Failure L.ByteString {-# UNPACK #-} !ByteOffset String

instance Show a => Show (Result a) where
  show (Success _ _ a)   = "Success: " ++ show a
  show (Failure _ _ msg) = "Failure: " ++ msg

instance Functor Result where
  fmap f (Success lbs i a)   = Success lbs i (f a)
  fmap _ (Failure lbs i msg) = Failure lbs i msg



-- | The simplest interface to run a 'Get' decoder. If the decoder runs into
-- an error, calls 'fail', or runs out of input, it will call 'error'.
runGet :: Get a -> L.ByteString -> a
runGet g lbs =
  case parseLooping g lbs of
    Success _ _ a     -> a
    Failure _ pos msg ->
      error $ "Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg

-- | Run a 'Get' monad and return 'Left' on failure and 'Right' on
-- success. In both cases any unconsumed input and the number of bytes
-- consumed is returned. In the case of failure, a human-readable
-- error message is included as well.
--
-- @since 0.6.4.0
runGetOrFail :: Get a -> L.ByteString
             -> Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, a)
runGetOrFail g lbs0 =
  case parseLooping g lbs0 of
    Success lbs pos a   -> Right (lbs, pos, a)
    Failure lbs pos msg -> Left (lbs, pos, msg)

-- Backwards compatibility: previous versions had to be resupplied one chunk at a time,
-- so the 'UnexpectedPartial' side case did not exist.
parseLooping :: Get a -> L.ByteString -> Result a
parseLooping g lbs0 =
  let ~(bs, lbs) = case lbs0 of
                     L.Chunk bs' lbs' -> (bs'    , lbs')
                     L.Empty          -> (B.empty, L.Empty)

      go d =
        case d of
          Done lbs' i a -> Success lbs' i a
          Fail lbs' i a -> Failure lbs' i a
          Partial k     -> go $ k EndOfInput

  in go $ create g 0 bs lbs NoMore



-- | Run a 'Get' monad. See 'Decoder' for what to do next, like providing
-- input, handling decoding errors and to get the output value.
runGetIncremental :: Get a -> Decoder a
runGetIncremental g = create g 0 B.empty L.Empty More

-- | DEPRECATED. Provides compatibility with previous versions of this library.
-- Run a 'Get' monad and return a tuple with three values.
-- The first value is the result of the decoder. The second and third are the
-- unused input, and the number of consumed bytes.
{-# DEPRECATED runGetState "Use runGetIncremental instead. This function will be removed."
#-}
runGetState :: Get a -> L.ByteString -> ByteOffset -> (a, L.ByteString, ByteOffset)
runGetState g lbs0 i =
  let ~(bs, lbs) = case lbs0 of
                     L.Chunk bs' lbs' -> (bs'    , lbs')
                     L.Empty          -> (B.empty, L.Empty)

  in case create g i bs lbs NoMore of
       Done lbs' pos a  -> (a, lbs', pos)
       Partial _        -> error "Data.Binary.Get.runGet: partial result"
       Fail _ pos msg   ->
         error $ "Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg



-- | Helper exception for excluding the /hopefully/ impossible 'Partial' result of
--   a parser explicitly 'create'd with 'NoMore'.
--
--   @since 0.9.1
data UnexpectedPartial = UnexpectedPartial

instance Show UnexpectedPartial where
  show _ =
    "binary.Get: parser was instructed to never prompt for more input, yet prompted anyway"

instance Exception UnexpectedPartial

-- | Run a 'Get' monad by providing all of the input directly.
--
--   @since 0.9.1
parse :: Get a -> L.ByteString -> Result a
parse g lbs0 =
  let ~(bs, lbs) = case lbs0 of
                     L.Chunk bs' lbs' -> (bs'    , lbs')
                     L.Empty          -> (B.empty, L.Empty)

  in case create g 0 bs lbs NoMore of
       Done lbs' i a -> Success lbs' i a
       Fail lbs' i a -> Failure lbs' i a
       Partial _     -> throw UnexpectedPartial

-- | Run a 'Get' monad by providing some part of the input and supplying more later
--   by unwrapping a 'Decoder'.
--
--   @since 0.9.1
initiate
  :: Get a
  -> L.ByteString -- ^ Can be empty.
  -> Decoder a
initiate g lbs0 =
  let ~(bs, lbs) = case lbs0 of
                     L.Chunk bs' lbs' -> (bs'    , lbs')
                     L.Empty          -> (B.empty, L.Empty)

  in create g 0 bs lbs More



-- | Broadest useful 'Get' runner.
--
--   @since 0.9.1
create
  :: Get a
  -> ByteOffset   -- ^ Initial byte offset
  -> B.ByteString -- ^ First chunk of the input. It can be empty.
  -> L.ByteString -- ^ Known chunks after the first one
  -> More         -- ^ Whether more input exists
  -> Decoder a
create g i0 bs0 lbs0 more =
  runCont g i0 0 bs0 lbs0 more Free Drop id
    ( \i o bs lbs _ _ _ _ a   ->
        let !(# lbs' #) | o == B.length bs = (# lbs #)
                        | otherwise        = (# L.Chunk (B.unsafeDrop o bs) lbs #)

        in Done lbs' (i + fromIntegral o) a
    )
    ( \i o bs lbs _     _ msg ->
        let !(# lbs' #) | o == B.length bs = (# lbs #)
                        | otherwise        = (# L.Chunk (B.unsafeDrop o bs) lbs #)

        in Fail lbs' (i + fromIntegral o) msg
    )



notEnough
  :: TotalOffset
  -> ChunkOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Failure r
  -> Decoder r
notEnough i o bs lbs roll no = no i o bs lbs NoMore roll "not enough bytes"
{-# INLINE notEnough #-}



-- | Recycle the current chunk and try to proceed with the next one.
advance
  :: TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> More
  -> Location
  -> Policy
  -> Rollback
  -> (TotalOffset -> B.ByteString -> L.ByteString -> Policy -> Rollback -> Decoder r)
  -> Failure r
  -> Decoder r
advance i bs lbs more loc pol roll next no =
  case lbs of
    L.Chunk bs' lbs' ->
      let !i' = i + fromIntegral (B.length bs)
          !roll' = case pol of
                     Drop -> roll
                     Keep -> roll . L.Chunk bs'

      in next i' bs' lbs' pol roll'

    L.Empty          ->
      case more of
        More ->
          Partial $ \resupply ->
            case resupply of
              Supply bs' lbs' ->
                let !i' = i + fromIntegral (B.length bs)
                    !roll' =
                      case loc of
                        Free   -> roll
                        Choice -> roll . L.Chunk bs' . mappend lbs'

                in next i' bs' lbs' Drop roll'

              EndOfInput -> notEnough i (B.length bs) bs L.Empty roll no

        NoMore  -> notEnough i (B.length bs) bs L.Empty roll no
{-# INLINE advance #-}

-- | Prompts for more input until 'EndOfInput'.
flush
  :: TotalOffset
  -> Location
  -> Rollback
  -> (TotalOffset -> L.ByteString -> Rollback -> Decoder r)
  -> Decoder r
flush i0 loc roll0 next = partial i0 id roll0
  where
    partial i acc roll =
      Partial $ \resupply ->
        case resupply of
          Supply bs lbs ->
            let !i' = i + fromIntegral (B.length bs)

            in rummage (L.Chunk bs) i' lbs $ \i'' cur ->
                 let !roll' = case loc of
                                Free   -> roll
                                Choice -> roll . cur

                 in partial i'' (acc . cur) roll'

          EndOfInput -> next i (acc L.empty) roll



-- | Get the total number of bytes read to this point.
bytesRead :: Get ByteOffset
bytesRead =
  C $ \i o bs lbs more loc pol roll yes _no ->
    yes i o bs lbs more loc pol roll (i + fromIntegral o)
{-# INLINE bytesRead #-}



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
isolate n act =
  C $ \i o bs lbs more loc pol roll yes no ->
    if n < 0
      then no i o bs lbs more roll "isolate: negative size"
      else runCont (unsafeIsolate n act) i o bs lbs more loc pol roll yes no
{-# INLINE isolate #-}

-- | Isolate a decoder to operate with a fixed __non-negative__ number of bytes,
--   and fail if fewer bytes were consumed, or more bytes were attempted to be consumed.
--   If the given decoder fails, 'unsafeIsolate' will also fail.
--   Offset from 'bytesRead' will be relative to the start of 'isolate', not the
--   absolute of the input.
unsafeIsolate
  :: Int   -- ^ The number of bytes that must be consumed
  -> Get a -- ^ The decoder to isolate
  -> Get a
unsafeIsolate n0 act =
  C $ \i0 o0 bs0 lbs0 more loc pol0 roll0 yes no ->
    let o1 = o0 + n0
        n1 = o1 - B.length bs0

        lessThan a b =
         "isolate: the decoder consumed "
          ++ show a
          ++ " bytes which is less than the expected "
          ++ show b ++ " bytes"

    in if n1 <= 0
         then
           runCont act 0 0 (B.unsafeTake n0 $ B.unsafeDrop o0 bs0) L.Empty NoMore Free Drop roll0
             ( \_iR oR _bs _lbsR _moreR _locR _polR _rollR a ->
                 if oR == n0
                   then yes i0 o1 bs0 lbs0 more loc pol0 roll0 a
                   else
                     let !o' = o0 + oR
                     in no i0 o' bs0 lbs0 more roll0 $ lessThan oR n0
             )
             ( \_iR oR _bs _lbsR _moreR _rollR ->
                 let !o' = o0 + oR
                 in no i0 o' bs0 lbs0 more roll0
             )

         else
           ensureChunks n1 i0 bs0 lbs0 more loc pol0 roll0
             ( \i bs lbs pol roll lbsI ->
                 runCont act 0 0 (B.unsafeDrop o0 bs0) lbsI NoMore Choice Keep id
                   ( \iR oR bsR lbsR _moreR _locR _polR rollR a ->
                        if iR + fromIntegral oR == fromIntegral n0
                          then yes i 0 bs lbs more loc pol roll a

                          else
                            let !i' = i0 + fromIntegral o0 + iR
                                !roll' = case pol0 of
                                           Drop -> roll0
                                           Keep -> roll0 . rollR

                            in no i' oR bsR (lbsR <> L.chunk bs lbs) more roll' $
                                 lessThan (fromIntegral iR + oR) n0
                   )
                   ( \iR oR bsR lbsR _moreR rollR ->
                       let !i' = i0 + fromIntegral o0 + iR
                           !roll' = case pol0 of
                                      Drop -> roll0
                                      Keep -> roll0 . rollR

                       in no i' oR bsR (lbsR <> L.chunk bs lbs) more roll'
                   )
             )
             no
{-# INLINE unsafeIsolate #-}



type EnsureSuccess r = TotalOffset
                    -> B.ByteString
                    -> L.ByteString
                    -> Policy
                    -> Rollback
                    -> L.ByteString
                    -> Decoder r

ensureChunks
  :: Int
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> More
  -> Location
  -> Policy
  -> Rollback
  -> EnsureSuccess r
  -> Failure r
  -> Decoder r
ensureChunks n0 i0 bs0 lbs0 more loc pol0 roll0 yes no =
  let go i bs lbs pol roll acc n =
        advance i bs lbs more loc pol roll
          ( \i' bs' lbs' pol' roll' ->
              let n' = n - fromIntegral (B.length bs')
              in if n' <= 0
                   then yes (i' + fromIntegral n) (B.unsafeDrop n bs') lbs' pol' roll'
                          (acc $ L.Chunk (B.unsafeTake n bs') L.Empty)

                   else go i' bs' lbs' pol' roll' (acc . L.Chunk bs') n'
          )
          no

  in go i0 bs0 lbs0 pol0 roll0 id n0
{-# INLINE ensureChunks #-}



-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
isEmpty :: Get Bool
isEmpty =
  C $ \i o bs lbs more loc pol roll yes _no ->
    yes i o bs lbs more loc pol roll $
      o == B.length bs && L.null lbs && case more of
                                          NoMore -> True
                                          _      -> False
{-# INLINE isEmpty #-}



-- | DEPRECATED. Same as 'getByteString'.
{-# DEPRECATED getBytes "Use 'getByteString' instead of 'getBytes'." #-}
getBytes :: Int -> Get B.ByteString
getBytes = getByteString
{-# INLINE getBytes #-}



-- | @since 0.7.0.0
instance Alternative Get where
  empty = C $ \i o bs lbs more _loc _pol roll _yes no ->
            no i o bs lbs more roll "Data.Binary.Get(Alternative).empty"
  {-# INLINE empty #-}

  (<|>) f g =
    C $ \i0 o0 bs0 lbs0 more0 loc pol roll0 yes no ->
      runCont f i0 o0 bs0 lbs0 more0 Choice Keep id
        ( \i o bs lbs more _loc _pol roll a ->
            let !roll1 = case pol of
                            Drop -> roll0
                            Keep -> roll0 . roll

            in yes i o bs lbs more loc pol roll1 a
        )
        ( \_i _o _bs lbs more roll _msg ->
            let !lbs1 = roll lbs
            in runCont g i0 o0 bs0 lbs1 more loc pol roll0 yes no
        )
  {-# INLINE (<|>) #-}

  some p = (:) <$> p <*> many p
  {-# INLINE some #-}

  many p =
    C $ \i0 o0 bs0 lbs0 more0 loc pol roll0 yes0 _no ->
      let go i o bs lbs more roll yes =
            runCont p i o bs lbs more Choice Keep id
              ( \i1 o1 bs1 lbs1 more1 _loc _pol roll1 a ->
                  let !roll2 = case pol of
                                 Drop -> roll
                                 Keep -> roll . roll1

                  in go i1 o1 bs1 lbs1 more1 roll2
                       ( \iY oY bsY lbsY moreY locY polY rollY as ->
                           yes iY oY bsY lbsY moreY locY polY rollY (a:as)
                       )
              )
              ( \_i1 _o1 _bs1 lbs1 more1 roll1 _msg1 ->
                  let !lbs2 = roll1 lbs1
                  in yes i o bs lbs2 more1 loc pol roll1 []
              )

      in go i0 o0 bs0 lbs0 more0 roll0 yes0
  {-# INLINE many #-}



-- | Run the given decoder, but without consuming its input. If the given
-- decoder fails, then so will this function.
--
-- @since 0.7.0.0
lookAhead :: Get a -> Get a
lookAhead = lookAhead' (\_ -> Undo)
{-# INLINEABLE lookAhead #-}

-- | Run the given decoder, and only consume its input if it returns 'Just'.
-- If 'Nothing' is returned, the input will be unconsumed.
-- If the given decoder fails, then so will this function.
--
-- @since 0.7.0.0
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM = lookAhead' $ \res -> case res of
                                    Just _  -> Stay
                                    Nothing -> Undo
{-# INLINEABLE lookAheadM #-}

-- | Run the given decoder, and only consume its input if it returns 'Right'.
-- If 'Left' is returned, the input will be unconsumed.
-- If the given decoder fails, then so will this function.
--
-- @since 0.7.1.0
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE = lookAhead' $ \res -> case res of
                                    Right _ -> Stay
                                    Left _  -> Undo
{-# INLINEABLE lookAheadE #-}

data Verdict = Undo | Stay

lookAhead' :: (a -> Verdict) -> Get a -> Get a
lookAhead' undo g =
  C $ \i o bs lbs more loc pol roll yes no ->
    runCont g i o bs lbs more Choice Keep id
      ( \i' o' bs' lbs' more' _loc' _pol' roll' res ->
          case undo res of
            Undo -> yes i o bs (roll' lbs') more' loc pol roll res

            Stay ->
              let !roll'' = case pol of
                              Drop -> roll
                              Keep -> roll . roll'

              in yes i' o' bs' lbs' more' loc pol roll'' res
      )
      ( \i' o' bs' lbs' more' roll' msg ->
          let !roll'' = case pol of
                          Drop -> roll
                          Keep -> roll . roll'

          in no i' o' bs' lbs' more' roll'' msg
      )
{-# INLINE lookAhead' #-}



-- | Label a decoder. If the decoder fails, the label will be appended on
-- a new line to the error message string.
--
-- @since 0.7.2.0
label :: String -> Get a -> Get a
label msg decoder =
  C $ \i o bs lbs more loc pol roll yes no ->
    runCont decoder i o bs lbs more loc pol roll yes $ \i' o' bs' lbs' more' roll' s ->
      no i' o' bs' lbs' more' roll' (s ++ ('\n' : msg))
{-# INLINEABLE label #-}



-- | DEPRECATED. Get the number of bytes of remaining input.
-- Note that this is an expensive function to use as in order to calculate how
-- much input remains, all input has to be read and kept in-memory.
-- The decoder keeps the input as a strict bytestring, so you are likely better
-- off by calculating the remaining input in another way.
{-# DEPRECATED remaining "This will force all remaining input, don't use it." #-}
remaining :: Get Int64
remaining =
  C $ \i o bs lbs more loc pol roll yes _no ->
    case more of
      NoMore  ->
        let !n = fromIntegral (B.length bs - o) + L.length lbs
        in yes i o bs lbs more loc pol roll n

      More ->
        let !n = fromIntegral (B.length bs - o)
        in rummage id n lbs $ \n' cur ->
             flush n' loc roll $ \n'' lbs' roll' ->
               yes i o bs (cur lbs') NoMore loc Drop roll' n''
{-# INLINEABLE remaining #-}

------------------------------------------------------------------------
-- ByteStrings
--

-- | An efficient get method for strict ByteStrings. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getByteString :: Int -> Get B.ByteString
getByteString n =
  C $ \i o bs lbs more loc pol roll yes no ->
    if n <= 0
      then yes i o bs lbs more loc pol roll B.empty
      else runCont (unsafeGetByteString n) i o bs lbs more loc pol roll yes no
{-# INLINE getByteString #-}

-- | An efficient get method for strict ByteStrings. @n@ __must__ be non-negative.
--   Fails if fewer than @n@ bytes are left in the input.
unsafeGetByteString :: Int -> Get B.ByteString
unsafeGetByteString n =
  C $ \i o bs lbs more loc pol roll yes no ->
    let o' = o + n
        n' = o' - B.length bs
    in if n' <= 0
         then yes i o' bs lbs more loc pol roll (B.unsafeTake n $ B.unsafeDrop o bs)

         else getMoreByteString more loc
                ( \i' o'' bs' lbs' pol' roll' r ->
                    yes i' o'' bs' lbs' more loc pol' roll' r
                )
                no
                (byteString (B.unsafeDrop o bs)) n' i bs lbs pol roll
{-# INLINE unsafeGetByteString #-}

type ReadSuccess a r = TotalOffset
                    -> ChunkOffset
                    -> B.ByteString
                    -> L.ByteString
                    -> Policy
                    -> Rollback
                    -> a
                    -> Decoder r

-- This can run faster with (!Int, MutableByteArray s -> Int -> ST s ())
-- instead of a Builder, but it requires both a @primitive@ library dependency
-- and a custom ByteString allocation routine.
getMoreByteString
  :: More
  -> Location
  -> ReadSuccess B.ByteString r
  -> Failure r

  -> Builder
  -> Int
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Policy
  -> Rollback
  -> Decoder r
getMoreByteString more loc yes no = go
  where
    go acc n i0 bs0 lbs0 pol0 roll0 =
      advance i0 bs0 lbs0 more loc pol0 roll0
        ( \i bs lbs pol roll ->
            let n' = n - B.length bs
            in if n' <= 0
                 then
                   yes i n bs lbs pol roll $
                     L.toStrict
                       (toLazyByteString $ acc <> byteString (B.unsafeTake n bs))

                 else go (acc <> byteString bs) n' i bs lbs pol roll
        )
        no
{-# NOINLINE getMoreByteString #-}


-- | Return at least @n@ bytes, usually more. @n@ __must__ be non-negative.
--   If not enough data is available the computation will escape with 'Partial'.
unsafeRead :: (B.ByteString -> a) -> Int -> Get a
unsafeRead f n =
  C $ \i o bs lbs more loc pol roll yes no ->
    let o' = o + n
        n' = o' - B.length bs
    in if n' <= 0
         then yes i o' bs lbs more loc pol roll (f (B.unsafeDrop o bs))

         else getMoreByteString more loc
                ( \i' o'' bs' lbs' pol' roll' a ->
                    yes i' o'' bs' lbs' more loc pol' roll' (f a)
                )
                no
                (byteString (B.unsafeDrop o bs)) n' i bs lbs pol roll
{-# INLINE unsafeRead #-}

-- | @accursedRead f n@ where @f@ must be deterministic and not have side effects.
--   @n@ __must__ be non-negative.
accursedRead :: (Ptr b -> IO a) -> Int -> Get a
accursedRead f =
  -- It should be safe to use accursedUnutterablePerformIO here.
  -- The action must be deterministic and not have any external side effects.
  -- It depends on the value of the ByteString so the value dependencies look OK.
  unsafeRead $ \s ->
    accursedUnutterablePerformIO $ B.unsafeUseAsCString s (f . castPtr)
{-# INLINE accursedRead #-}



-- | An efficient get method for lazy ByteStrings. Fails if fewer than @n@
-- bytes are left in the input.
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n =
  C $ \i o bs lbs more loc pol roll yes no ->
    if n <= 0
      then yes i o bs lbs more loc pol roll L.Empty
      else runCont (unsafeGetLazyByteString n) i o bs lbs more loc pol roll yes no
{-# INLINE getLazyByteString #-}

-- | An efficient get method for lazy ByteStrings. @n@ __must__ be non-negative.
--   Fails if fewer than @n@ bytes are left in the input.
unsafeGetLazyByteString :: Int64 -> Get L.ByteString
unsafeGetLazyByteString n0 =
  C $ \i0 o0 bs0 lbs0 more loc pol0 roll0 yes no ->
    let n1 = n0 + fromIntegral (o0 - B.length bs0)
    in if n1 <= 0
         then let !n32 = fromIntegral n0
              in yes i0 (o0 + n32) bs0 lbs0 more loc pol0 roll0
                   (L.fromStrict . B.unsafeTake n32 $ B.unsafeDrop o0 bs0)
         else
           let go i bs lbs pol roll acc n =
                 advance i bs lbs more loc pol roll
                   ( \i' bs' lbs' pol' roll' ->
                       let n32 = fromIntegral n
                           n' = n - fromIntegral (B.length bs')
                       in if n' <= 0
                            then yes i' n32 bs' lbs' more loc pol' roll'
                                   (acc $ L.fromStrict (B.unsafeTake n32 bs'))

                            else go i' bs' lbs' pol' roll' (acc . L.Chunk bs') n'
                   )
                   no

               !acc0 | o0 == B.length bs0 = id
                     | otherwise          = L.Chunk (B.unsafeDrop o0 bs0)

           in go i0 bs0 lbs0 pol0 roll0 acc0 n1
{-# INLINE unsafeGetLazyByteString #-}



-- | Get a lazy ByteString that is terminated with a NUL byte.
-- The returned string does not contain the NUL byte. Fails
-- if it reaches the end of input without finding a NUL.
getLazyByteStringNul :: Get L.ByteString
getLazyByteStringNul =
  C $ \i0 o0 bs0 lbs0 more loc pol0 roll0 yes no ->
    case B.elemIndex 0 bs0 of
      Just x ->
        let x'  = x + 1
            !o1 = o0 + fromIntegral x'
        in yes i0 o1 bs0 lbs0 more loc pol0 roll0
             (L.fromStrict . B.unsafeTake x $ B.unsafeDrop o0 bs0)

      Nothing ->
        let go i bs lbs pol roll acc =
              advance i bs lbs more loc pol roll
                ( \i' bs' lbs' pol' roll' ->
                    case B.elemIndex 0 bs' of
                      Just x ->
                        let !x' = x + 1
                        in yes i' x' bs' lbs' more loc pol' roll'
                             (acc $ L.fromStrict (B.unsafeTake x bs'))

                      Nothing -> go i' bs' lbs' pol' roll' (acc . L.Chunk bs')
                )
                no

            !acc0 | o0 == B.length bs0 = id
                  | otherwise          = L.Chunk (B.unsafeDrop o0 bs0)

        in go i0 bs0 lbs0 pol0 roll0 acc0
{-# INLINE getLazyByteStringNul #-}



rummage
  :: (L.ByteString -> L.ByteString)
  -> TotalOffset
  -> L.ByteString
  -> (TotalOffset -> (L.ByteString -> L.ByteString) -> a)
  -> a
rummage acc0 i0 lbs0 f = go acc0 i0 lbs0
  where
    go acc i lbs =
      case lbs of
        L.Chunk bs lbs' ->
          let !i' = i + fromIntegral (B.length bs)
          in go (acc . L.Chunk bs) i' lbs'

        L.Empty         -> f i acc
{-# INLINE rummage #-}

-- | Get the remaining bytes as a lazy ByteString.
-- Note that this can be an expensive function to use as it forces reading
-- all input and keeping the string in-memory.
getRemainingLazyByteString :: Get L.ByteString
getRemainingLazyByteString =
  C $ \i o bs lbs more loc pol roll yes _no ->
    case more of
      NoMore  ->
        let !i' = i + fromIntegral (B.length bs) + L.length lbs
        in yes i' 0 B.empty L.Empty NoMore loc pol roll (L.chunk (B.unsafeDrop o bs) lbs)

      More ->
        let !i' = i + fromIntegral (B.length bs)

            !acc | o == B.length bs = id
                 | otherwise        = L.Chunk bs

        in rummage acc i' lbs $ \i'' cur ->
             flush i'' loc roll $ \i''' rest roll' ->
               yes i''' 0 B.empty L.Empty NoMore loc Drop roll' $ cur rest
{-# INLINE getRemainingLazyByteString #-}



-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n =
  C $ \i o bs lbs more loc pol roll yes no ->
    if n <= 0
      then yes i o bs lbs more loc pol roll ()
      else runCont (unsafeSkip n) i o bs lbs more loc pol roll yes no
{-# INLINE skip #-}

-- | Skip ahead @n@ bytes. @n@ __must__ be non-negative.
--   Fails if fewer than @n@ bytes are available.
unsafeSkip :: Int -> Get ()
unsafeSkip n0 =
  C $ \i0 o0 bs0 lbs0 more loc pol0 roll0 yes no ->
    let o1 = o0 + n0
        n1 = o1 - B.length bs0
    in if n1 <= 0
         then yes i0 o1 bs0 lbs0 more loc pol0 roll0 ()
         else
           let go i bs lbs pol roll n =
                 advance i bs lbs more loc pol roll
                   ( \i' bs' lbs' pol' roll' ->
                       let n32 = fromIntegral n
                           n' = n - fromIntegral (B.length bs')
                       in if n' <= 0
                            then yes i' n32 bs' lbs' more loc pol' roll' ()
                            else go i' bs' lbs' pol' roll' n'
                   )
                   no

           in go i0 bs0 lbs0 pol0 roll0 n1
{-# INLINE unsafeSkip #-}
