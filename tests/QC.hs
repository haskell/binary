{-# LANGUAGE CPP, DeriveGeneric, ScopedTypeVariables, DataKinds, TypeSynonymInstances, MagicHash, UnboxedTuples #-}
module Main ( main ) where

#if MIN_VERSION_base(4,8,0)
#define HAS_NATURAL
#endif

#if MIN_VERSION_base(4,7,0)
#define HAS_FIXED_CONSTRUCTOR
#endif

import           Control.Applicative
import           Control.Exception                    as C (SomeException,
                                                            catch, evaluate)
import           Control.Monad                        (MonadPlus(..), unless, liftM2)
import           Data.Bits                            ((.|.), shiftL)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Internal             as BI
import qualified Data.ByteString.Lazy                 as L
import qualified Data.ByteString.Lazy.Internal        as L
import           Data.ByteString.Short                (ShortByteString)
import           Data.Complex                         (Complex(..))
import           Data.Functor.Identity                (Identity(..))
import           Data.Int
import qualified Data.IntMap                          as IntMap
import qualified Data.IntSet                          as IntSet
import           Data.IORef                           (newIORef, readIORef, writeIORef)
import           Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.Map                             as Map
import           Data.Ratio
import qualified Data.Sequence                        as Seq
import qualified Data.Set                             as Set
import qualified Data.Tree                            as Tree
import           Data.Typeable
import           Data.Word
import           Foreign                              (Ptr, castPtr, free, mallocBytes, peek, peekByteOff, poke)
import qualified Foreign.Concurrent                   as FC
import           System.IO.Unsafe
import           System.Mem                           (performGC)

import           Data.Orphans ()

#ifdef HAS_NATURAL
import           Numeric.Natural
#endif

import           GHC.Exts (Array#, Word#
#if MIN_VERSION_base(4,10,0)
                          , VecCount, VecElem
#endif
                          )
import           GHC.Fingerprint
import           GHC.Generics (Generic)

import qualified Data.Fixed as Fixed

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck hiding (total)

import qualified Action                               (tests)
import           Arbitrary                            ()
import           Data.Binary
import qualified Data.Binary.Builder                  as Builder
import           Data.Binary.Get
import qualified Data.Binary.Get.Internal             as GetI
import           Data.Binary.Get.Internal.Buf
import           Data.Binary.Put


------------------------------------------------------------------------

roundTrip :: (Eq a, Binary a) => a -> (L.ByteString -> L.ByteString) -> Bool
roundTrip a f = a ==
    {-# SCC "decode.refragment.encode" #-} decode (f (encode a))

roundTripWith ::  Eq a => (a -> Put) -> Get a -> a -> Property
roundTripWith putter getter x =
    forAll positiveList $ \xs ->
    x == runGet getter (refragment xs (runPut (putter x)))

-- make sure that a test fails
mustThrowError :: B a
mustThrowError a = unsafePerformIO $
    C.catch (do _ <- C.evaluate a
                return False)
            (\(_e :: SomeException) -> return True)

-- low level ones:
--
-- Words

prop_Word8 :: Word8 -> Property
prop_Word8 = roundTripWith putWord8 getWord8

prop_Word16be :: Word16 -> Property
prop_Word16be = roundTripWith putWord16be getWord16be

prop_Word16le :: Word16 -> Property
prop_Word16le = roundTripWith putWord16le getWord16le

prop_Word16host :: Word16 -> Property
prop_Word16host = roundTripWith putWord16host getWord16host

prop_Word32be :: Word32 -> Property
prop_Word32be = roundTripWith putWord32be getWord32be

prop_Word32le :: Word32 -> Property
prop_Word32le = roundTripWith putWord32le getWord32le

prop_Word32host :: Word32 -> Property
prop_Word32host = roundTripWith putWord32host getWord32host

prop_Word64be :: Word64 -> Property
prop_Word64be = roundTripWith putWord64be getWord64be

prop_Word64le :: Word64 -> Property
prop_Word64le = roundTripWith putWord64le getWord64le

prop_Word64host :: Word64 -> Property
prop_Word64host = roundTripWith putWord64host getWord64host

prop_Wordhost :: Word -> Property
prop_Wordhost = roundTripWith putWordhost getWordhost

-- Ints

prop_Int8 :: Int8 -> Property
prop_Int8 = roundTripWith putInt8 getInt8

prop_Int16be :: Int16 -> Property
prop_Int16be = roundTripWith putInt16be getInt16be

prop_Int16le :: Int16 -> Property
prop_Int16le = roundTripWith putInt16le getInt16le

prop_Int16host :: Int16 -> Property
prop_Int16host = roundTripWith putInt16host getInt16host

prop_Int32be :: Int32 -> Property
prop_Int32be = roundTripWith putInt32be getInt32be

prop_Int32le :: Int32 -> Property
prop_Int32le = roundTripWith putInt32le getInt32le

prop_Int32host :: Int32 -> Property
prop_Int32host = roundTripWith putInt32host getInt32host

prop_Int64be :: Int64 -> Property
prop_Int64be = roundTripWith putInt64be getInt64be

prop_Int64le :: Int64 -> Property
prop_Int64le = roundTripWith putInt64le getInt64le

prop_Int64host :: Int64 -> Property
prop_Int64host = roundTripWith putInt64host getInt64host

prop_Inthost :: Int -> Property
prop_Inthost = roundTripWith putInthost getInthost

-- Floats and Doubles

prop_Floatbe :: Float -> Property
prop_Floatbe = roundTripWith putFloatbe getFloatbe

prop_Floatle :: Float -> Property
prop_Floatle = roundTripWith putFloatle getFloatle

prop_Floathost :: Float -> Property
prop_Floathost = roundTripWith putFloathost getFloathost

prop_Doublebe :: Double -> Property
prop_Doublebe = roundTripWith putDoublebe getDoublebe

prop_Doublele :: Double -> Property
prop_Doublele = roundTripWith putDoublele getDoublele

prop_Doublehost :: Double -> Property
prop_Doublehost = roundTripWith putDoublehost getDoublehost

#if MIN_VERSION_base(4,10,0)
testTypeable :: Test
testTypeable = testGroup "Typeable"
    [ testProperty "TypeRep" prop_TypeRep
    , testProperty "VecCount invalid tag fails" prop_VecCount_invalid
    , testProperty "VecElem invalid tag fails" prop_VecElem_invalid
    ]

prop_VecCount_invalid :: Word8 -> Property
prop_VecCount_invalid w =
    fromIntegral w > fromEnum (maxBound :: VecCount) ==>
      case decodeOrFail (encode w) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, VecCount) of
        Left _  -> True
        Right _ -> False

prop_VecElem_invalid :: Word8 -> Property
prop_VecElem_invalid w =
    fromIntegral w > fromEnum (maxBound :: VecElem) ==>
      case decodeOrFail (encode w) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, VecElem) of
        Left _  -> True
        Right _ -> False

prop_TypeRep :: TypeRep -> Property
prop_TypeRep = roundTripWith put get

atomicTypeReps :: [TypeRep]
atomicTypeReps =
    [ typeRep (Proxy :: Proxy ())
    , typeRep (Proxy :: Proxy String)
    , typeRep (Proxy :: Proxy Int)
    , typeRep (Proxy :: Proxy (,))
    , typeRep (Proxy :: Proxy ((,) (Maybe Int)))
    , typeRep (Proxy :: Proxy Maybe)
    , typeRep (Proxy :: Proxy 'Nothing)
    , typeRep (Proxy :: Proxy 'Left)
    , typeRep (Proxy :: Proxy "Hello")
    , typeRep (Proxy :: Proxy 42)
    , typeRep (Proxy :: Proxy '[1,2,3,4])
    , typeRep (Proxy :: Proxy ('Left Int))
    , typeRep (Proxy :: Proxy (Either Int String))
    , typeRep (Proxy :: Proxy (() -> ()))
    , typeRep (Proxy :: Proxy (# #))
    , typeRep (Proxy :: Proxy (#,#))
    , typeRep (Proxy :: Proxy Word#)
    , typeRep (Proxy :: Proxy (Word# -> Word#))
    , typeRep (Proxy :: Proxy Array#)
    , typeRep (Proxy :: Proxy (Array# Int))
    ]

instance Arbitrary TypeRep where
    arbitrary = elements atomicTypeReps
#else
testTypeable :: Test
testTypeable = testGroup "Skipping Typeable tests" []
#endif

-- done, partial and fail

-- | Test partial results.
-- May or may not use the whole input, check conditions for the different
-- outcomes.
prop_partial :: L.ByteString -> Property
prop_partial lbs = forAll (choose (0, L.length lbs * 2)) $ \skipN ->
  let result = pushChunks (runGetIncremental decoder) lbs
      decoder = do
        s <- getByteString (fromIntegral skipN)
        return (L.fromChunks [s])
  in case result of
       Partial _ -> L.length lbs < skipN
       Done unused _pos value ->
         and [ L.length value == skipN
             , L.append value (L.fromChunks [unused]) == lbs
             ]
       Fail _ _ _ -> False

-- | Fail a decoder and make sure the result is sane.
prop_fail :: L.ByteString -> String -> Property
prop_fail lbs msg = forAll (choose (0, L.length lbs)) $ \pos ->
  let result = pushChunks (runGetIncremental decoder) lbs
      decoder = do
        -- use part of the input...
        _ <- getByteString (fromIntegral pos)
        -- ... then fail
        fail msg
  in case result of
     Fail unused pos' msg' ->
       and [ pos == pos'
           , msg == msg'
           , L.length lbs - pos == fromIntegral (B.length unused)
           , L.fromChunks [unused] `L.isSuffixOf` lbs
           ]
     _ -> False -- wuut?

-- read negative length
prop_getByteString_negative :: Int -> Property
prop_getByteString_negative n =
  n < 1 ==>
    runGet (getByteString n) L.empty == B.empty

prop_getLazyByteString_negative :: Int64 -> Property
prop_getLazyByteString_negative n =
  n < 1 ==>
    runGet (getLazyByteString n) L.empty == L.empty

prop_skip_negative :: Int -> Property
prop_skip_negative n =
  n < 1 ==>
    runGet (skip n) L.empty == ()

prop_isolate_negative :: Int -> Property
prop_isolate_negative n =
  n < 0 ==>
    case runGetOrFail (isolate n (return ())) L.empty of
      Left (_, 0, _) -> True
      _              -> False

prop_isEmpty :: L.ByteString -> [Int] -> Property
prop_isEmpty lbs xs =
    let lbs' = refragment xs lbs
        -- Test both directly and after feeding empty chunks
        dec  = do
          e1 <- isEmpty
          if e1
            then return (True, [])
            else do
              ws <- many getWord8
              e2 <- isEmpty
              return (e2, ws)
    in case runGetOrFail dec lbs' of
         Right (left, pos, (endEmpty, ws)) ->
           conjoin
             [ L.null left === True
             , pos === L.length lbs'
             , endEmpty === True
             , ws === L.unpack lbs'
             ]
         Left (_, _, err) -> counterexample ("unexpected failure: " ++ err) False

prop_isEmpty_emptyChunks :: [Word8] -> Property
prop_isEmpty_emptyChunks ws =
    let d0 = runGetIncremental isEmpty
        d1 = pushChunk (pushChunk d0 B.empty) B.empty
        d2 = if null ws
               then pushEndOfInput d1
               else pushChunk d1 (B.pack ws)
    in case d2 of
         Done left 0 res ->
           conjoin
             [ res === null ws
             , B.unpack left === ws
             ]
         _ -> counterexample "expected Done at pos 0" False

prop_alternative_empty_some_many :: [Word8] -> [Int] -> Property
prop_alternative_empty_some_many ws xs =
    let lbs = refragment xs (L.pack ws)
        emptyFails = case runGetOrFail (empty :: Get Word8) lbs of
                       Left (left, 0, _) -> left == lbs
                       _                 -> False
        mzeroFails = case runGetOrFail (mzero :: Get Word8) lbs of
                       Left (left, 0, _) -> left == lbs
                       _                 -> False
        mplusWorks = case runGetOrFail (mzero `mplus` pure (42 :: Int)) lbs of
                       Right (left, 0, 42) -> left == lbs
                       _                   -> False
        manyWorks  = runGet (many getWord8) lbs == ws
        someWorks  = case runGetOrFail (some getWord8) lbs of
                       Right (left, pos, res) -> not (null ws) && L.null left && pos == fromIntegral (length ws) && res == ws
                       Left _                 -> null ws
    in conjoin
         [ counterexample "empty" emptyFails
         , counterexample "mzero" mzeroFails
         , counterexample "mplus" mplusWorks
         , counterexample "many"  manyWorks
         , counterexample "some"  someWorks
         ]

prop_readN_ensureN :: [Word8] -> [Int] -> Property
prop_readN_ensureN ws xs =
    let lbs = refragment xs (L.pack ws)
        len = length ws
    in forAll (choose (0, len + 2)) $ \n ->
      let decReadN = GetI.readN n (B.take n)
          decEnsure = GetI.ensureN n >> getByteString n
          resReadN = runGetOrFail decReadN lbs
          resEnsure = runGetOrFail decEnsure lbs
      in if n <= len
           then conjoin
             [ case resReadN of
                 Right (left, pos, bs) ->
                   bs == B.pack (take n ws) && pos == fromIntegral n && L.unpack left == drop n ws
                 Left _ -> False
             , case resEnsure of
                 Right (left, pos, bs) ->
                   bs == B.pack (take n ws) && pos == fromIntegral n && L.unpack left == drop n ws
                 Left _ -> False
             ]
           else conjoin
             [ case resReadN of Left _ -> True; Right _ -> False
             , case resEnsure of Left _ -> True; Right _ -> False
             ]

prop_internal_get_put :: B.ByteString -> B.ByteString -> Bool
prop_internal_get_put bs1 bs2 =
    let dec = do
          _ <- GetI.get
          GetI.put bs2
          GetI.get
    in runGet dec (L.fromChunks [bs1]) == bs2

-- | Differential test: runGet and runGetOrFail must agree with runGetIncremental
-- across L.Empty, single-chunk, and multi-chunk inputs for immediate Done/Fail,
-- multi-chunk reads, bytesRead across chunks/EOF, and Partial->EOF failures.
prop_runGet_equiv_incremental :: [Word8] -> [Int] -> Property
prop_runGet_equiv_incremental ws xs =
    let lbs0 = L.pack ws
        len  = length ws
        lbsVariants = [L.empty, L.fromChunks [B.pack ws], refragment xs lbs0]
        runInc :: Get a -> L.ByteString -> Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, a)
        runInc dec lbs =
          case pushEndOfInput (pushChunks (runGetIncremental dec) lbs) of
            Done left pos a -> Right (L.fromChunks [left | not (B.null left)], pos, a)
            Fail left pos e -> Left  (L.fromChunks [left | not (B.null left)], pos, e)
            Partial _       -> error "runInc: unexpected Partial after pushEndOfInput"
        check :: (Eq a, Show a) => Get a -> L.ByteString -> Property
        check dec lbs =
          let incRes = runInc dec lbs
              orfRes = runGetOrFail dec lbs
              rgProp = case incRes of
                         Right (_, _, a) -> property (runGet dec lbs == a)
                         Left _          -> property (mustThrowError (runGet dec lbs))
          in conjoin [ orfRes === incRes, rgProp ]
    in forAll (choose (0, len + 2)) $ \n1 ->
       forAll (choose (0, len + 2)) $ \n2 ->
         let decRet   = return (42 :: Int)
             decFail  = fail "immediate" :: Get Int
             decRead  = do
               a <- getByteString n1
               p1 <- bytesRead
               b <- getByteString n2
               p2 <- bytesRead
               return (a, p1, b, p2)
             decEofBr = do
               _ <- getRemainingLazyByteString
               p1 <- bytesRead
               e  <- isEmpty
               p2 <- bytesRead
               return (p1, e, p2)
             decEofFail = do
               _ <- getRemainingLazyByteString
               _ <- bytesRead
               _ <- getWord8
               return ()
             decEofAlt = (getByteString (len + 5) >> return False) <|> (bytesRead >> return True)
         in conjoin
              [ conjoin [ check decRet lbs
                        , check decFail lbs
                        , check decRead lbs
                        , check decEofBr lbs
                        , check decEofFail lbs
                        , check decEofAlt lbs
                        ]
              | lbs <- lbsVariants
              ]

prop_pushChunk_and_pushEndOfInput :: B.ByteString -> B.ByteString -> Property
prop_pushChunk_and_pushEndOfInput bs1 bs2 =
    let -- pushChunk on Done appends leftover bytes
        dDone0 = pushChunk (runGetIncremental (return (1 :: Int))) bs1
        dDone1 = pushEndOfInput (pushChunk dDone0 bs2)
        okDone = case dDone1 of
                   Done left 0 1 -> left == B.append bs1 bs2
                   _             -> False
        -- pushChunk on Fail appends leftover bytes; pushEndOfInput preserves Fail
        dFail0 = pushChunk (runGetIncremental (fail "err" :: Get Int)) bs1
        dFail1 = pushEndOfInput (pushChunk dFail0 bs2)
        okFail = case dFail1 of
                   Fail left 0 "err" -> left == B.append bs1 bs2
                   _                 -> False
    in conjoin
         [ counterexample "pushChunk/pushEndOfInput on Done" okDone
         , counterexample "pushChunk/pushEndOfInput on Fail" okFail
         ]

prop_list_negative_length :: Int -> Property
prop_list_negative_length n =
  n < 0 ==>
    case decodeOrFail (encode n) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, [()]) of
      Left _  -> True
      Right _ -> False

prop_seq_negative_length :: Int -> Property
prop_seq_negative_length n =
  n < 0 ==>
    case decodeOrFail (encode n) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Seq.Seq ()) of
      Left _  -> True
      Right _ -> False

prop_maybe_invalid_tag :: Word8 -> Property
prop_maybe_invalid_tag w =
  w > 1 ==>
    case decodeOrFail (encode (w, ())) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Maybe ()) of
      Left _  -> True
      Right _ -> False

prop_either_invalid_tag :: Word8 -> Property
prop_either_invalid_tag w =
  w > 1 ==>
    case decodeOrFail (encode (w, ())) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Either () ()) of
      Left _  -> True
      Right _ -> False

prop_integer_invalid_tag :: Word8 -> Property
prop_integer_invalid_tag w =
  w > 1 ==>
    case decodeOrFail (encode (w, (1 :: Word8), ([] :: [Word8]))) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Integer) of
      Left _  -> True
      Right _ -> False

prop_integer_invalid_sign :: Word8 -> Property
prop_integer_invalid_sign s =
  s /= 1 && s /= 255 ==>
    case decodeOrFail (encode ((1 :: Word8), s, ([] :: [Word8]))) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Integer) of
      Left _  -> True
      Right _ -> False

#ifdef HAS_NATURAL
prop_natural_invalid_tag :: Word8 -> Property
prop_natural_invalid_tag w =
  w > 1 ==>
    case decodeOrFail (encode (w, ([] :: [Word8]))) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Natural) of
      Left _  -> True
      Right _ -> False
#endif


prop_bytesRead :: L.ByteString -> Property
prop_bytesRead lbs =
  forAll (makeChunks 0 totalLength) $ \chunkSizes ->
  let result = pushChunks (runGetIncremental decoder) lbs
      decoder = do
        -- Read some data and invoke bytesRead several times.
        -- Each time, check that the values are what we expect.
        flip mapM_ chunkSizes $ \(total, step) -> do
          _ <- getByteString (fromIntegral step)
          n <- bytesRead
          unless (n == total) $ fail "unexpected position"
        bytesRead
  in case result of
       Done unused pos value ->
         and [ value == totalLength
             , pos == value
             , B.null unused
             ]
       Partial _ -> False
       Fail _ _ _ -> False
  where
    totalLength = L.length lbs
    makeChunks total i
      | i == 0 = return []
      | otherwise = do
          n <- choose (0,i)
          let total' = total + n
          rest <- makeChunks total' (i - n)
          return ((total',n):rest)


-- | We're trying to guarantee that the Decoder will not ask for more input
-- with Partial if it has been given Nothing once.
-- In this test we're making the decoder return 'Partial' to get more
-- input, and to get knowledge of the current position using 'BytesRead'.
-- Both of these operations, when used with the <|> operator, result internally
-- in that the decoder return with Partial and BytesRead multiple times,
-- in which case we need to keep track of if the user has passed Nothing to a
-- Partial in the past.
prop_partialOnlyOnce :: Property
prop_partialOnlyOnce = property $
  let result = runGetIncremental (decoder <|> decoder)
      decoder = do
        0 <- bytesRead
        _ <- getWord8 -- this will make the decoder return with Partial
        return "shouldn't get here"
  in case result of
       -- we expect Partial followed by Fail
       Partial k -> case k Nothing of -- push down a Nothing
                      Fail _ _ _ -> True
                      Partial _ -> error $ "partial twice! oh noes!"
                      Done _ _ _ -> error $ "we're not supposed to be done."
       _ -> error $ "not partial, error!"

-- read too much
prop_readTooMuch :: (Eq a, Binary a) => a -> Bool
prop_readTooMuch x = mustThrowError $ x == a && x /= b
  where
    -- encode 'a', but try to read 'b' too
    (a,b) = decode (encode x)
    _types = [a,b]

-- In binary-0.5 the Get monad looked like
--
-- > data S = S {-# UNPACK #-} !B.ByteString
-- >            L.ByteString
-- >            {-# UNPACK #-} !Int64
-- >
-- > newtype Get a = Get { unGet :: S -> (# a, S #) }
--
-- with a helper function
--
-- > mkState :: L.ByteString -> Int64 -> S
-- > mkState l = case l of
-- >     L.Empty      -> S B.empty L.empty
-- >     L.Chunk x xs -> S x xs
--
-- Note that mkState is strict in its first argument. This goes wrong in this
-- function:
--
-- > getBytes :: Int -> Get B.ByteString
-- > getBytes n = do
-- >     S s ss bytes <- traceNumBytes n $ get
-- >     if n <= B.length s
-- >         then do let (consume,rest) = B.splitAt n s
-- >                 put $! S rest ss (bytes + fromIntegral n)
-- >                 return $! consume
-- >         else
-- >               case L.splitAt (fromIntegral n) (s `join` ss) of
-- >                 (consuming, rest) ->
-- >                     do let now = B.concat . L.toChunks $ consuming
-- >                        put $ mkState rest (bytes + fromIntegral n)
-- >                        -- forces the next chunk before this one is returned
-- >                        if (B.length now < n)
-- >                          then
-- >                             fail "too few bytes"
-- >                          else
-- >                             return now
--
-- Consider the else-branch of this function; suppose we ask for n bytes;
-- the call to L.splitAt gives us a lazy bytestring 'consuming' of precisely @n@
-- bytes (unless we don't have enough data, in which case we fail); but then
-- the strict evaluation of mkState on 'rest' means we look ahead too far.
--
-- Although this is all done completely differently in binary-0.7 it is
-- important that the same bug does not get introduced in some other way. The
-- test is basically the same test that already exists in this test suite,
-- verifying that
--
-- > decode . refragment . encode == id
--
-- However, we use a different 'refragment', one that introduces an exception
-- as the tail of the bytestring after rechunking. If we don't look ahead too
-- far then this should make no difference, but if we do then this will throw
-- an exception (for instance, in binary-0.5, this will throw an exception for
-- certain rechunkings, but not for others).
--
-- To make sure that the property holds no matter what refragmentation we use,
-- we test exhaustively for a single chunk, and all ways to break the string
-- into 2, 3 and 4 chunks.
prop_lookAheadIndepOfChunking :: (Eq a, Binary a) => a -> Property
prop_lookAheadIndepOfChunking testInput =
   forAll (testCuts (L.length (encode testInput))) $
     roundTrip testInput . rechunk
  where
    testCuts :: forall a. (Num a, Enum a) => a -> Gen [a]
    testCuts len = elements $ [ [] ]
                           ++ [ [i]
                              | i <- [0 .. len] ]
                           ++ [ [i, j]
                              | i <- [0 .. len]
                              , j <- [0 .. len - i] ]
                           ++ [ [i, j, k]
                              | i <- [0 .. len]
                              , j <- [0 .. len - i]
                              , k <- [0 .. len - i - j] ]

    -- Rechunk a bytestring, leaving the tail as an exception rather than Empty
    rechunk :: forall a. Integral a => [a] -> L.ByteString -> L.ByteString
    rechunk cuts = fromChunks . cut cuts . B.concat . L.toChunks
      where
        cut :: [a] -> B.ByteString -> [B.ByteString]
        cut []     bs = [bs]
        cut (i:is) bs = let (bs0, bs1) = B.splitAt (fromIntegral i) bs
                        in bs0 : cut is bs1

        fromChunks :: [B.ByteString] ->  L.ByteString
        fromChunks []       = error "Binary should not have to ask for this chunk!"
        fromChunks (bs:bss) = L.Chunk bs (fromChunks bss)

-- String utilities

prop_getLazyByteString :: L.ByteString -> Property
prop_getLazyByteString lbs = forAll (choose (0, 2 * L.length lbs)) $ \len ->
  let result = pushChunks (runGetIncremental decoder) lbs
      decoder = getLazyByteString len
  in case result of
       Done unused _pos value ->
         and [ value == L.take len lbs
             , L.fromChunks [unused] == L.drop len lbs
             ]
       Partial _ -> len > L.length lbs
       _ -> False

prop_getLazyByteStringNul :: Word16 -> [Int] -> Property
prop_getLazyByteStringNul count0 fragments = count >= 0 ==>
  forAll (choose (0, count)) $ \pos ->
  let lbs = case L.splitAt pos (L.replicate count 65) of
              (start,end) -> refragment fragments $ L.concat [start, L.singleton 0, end]
      result = pushEndOfInput $ pushChunks (runGetIncremental getLazyByteStringNul) lbs
  in case result of
       Done unused pos' value ->
         and [ value == L.take pos lbs
             , pos + 1 == pos' -- 1 for the NUL
             , L.fromChunks [unused] == L.drop (pos + 1) lbs
             ]
       _ -> False
  where
  count = fromIntegral count0 -- to make the generated numbers a bit smaller

-- | Same as prop_getLazyByteStringNul, but without any NULL in the string.
prop_getLazyByteStringNul_noNul :: Word16 -> [Int] -> Property
prop_getLazyByteStringNul_noNul count0 fragments = count >= 0 ==>
  let lbs = refragment fragments $ L.replicate count 65
      result = pushEndOfInput $ pushChunks (runGetIncremental getLazyByteStringNul) lbs
  in case result of
       Fail _ _ _ -> True
       _ -> False
  where
  count = fromIntegral count0 -- to make the generated numbers a bit smaller

prop_getRemainingLazyByteString :: L.ByteString -> Property
prop_getRemainingLazyByteString lbs = property $
  let result = pushEndOfInput $ pushChunks (runGetIncremental getRemainingLazyByteString) lbs
  in case result of
    Done unused pos value ->
      and [ value == lbs
          , B.null unused
          , fromIntegral pos == L.length lbs
          ]
    _ -> False

-- sanity:

invariant_lbs :: L.ByteString -> Bool
invariant_lbs (L.Empty)      = True
invariant_lbs (L.Chunk x xs) = not (B.null x) && invariant_lbs xs

prop_invariant :: (Binary a) => a -> Bool
prop_invariant = invariant_lbs . encode

-- refragment a lazy bytestring's chunks
refragment :: [Int] -> L.ByteString -> L.ByteString
refragment [] lbs = lbs
refragment (x:xs) lbs =
    let x' = fromIntegral . (+1) . abs $ x
        rest = refragment xs (L.drop x' lbs) in
    L.append (L.fromChunks [B.concat . L.toChunks . L.take x' $ lbs]) rest

-- check identity of refragmentation
prop_refragment :: L.ByteString -> [Int] -> Bool
prop_refragment lbs xs = lbs == refragment xs lbs

-- check that refragmention still hold invariant
prop_refragment_inv :: L.ByteString -> [Int] -> Bool
prop_refragment_inv lbs xs = invariant_lbs $ refragment xs lbs

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------

genInteger :: Gen Integer
genInteger = do
  b <- arbitrary
  if b then genIntegerSmall else genIntegerBig

genIntegerSmall :: Gen Integer
genIntegerSmall = arbitrary

genIntegerBig :: Gen Integer
genIntegerBig = do
  x <- arbitrarySizedIntegral :: Gen Integer
  -- arbitrarySizedIntegral generates numbers smaller than
  -- (maxBound :: Word32), so let's make them bigger to better test
  -- the Binary instance.
  return (x + fromIntegral (maxBound :: Word32))

#ifdef HAS_NATURAL
genNatural :: Gen Natural
genNatural = do
  b <- arbitrary
  if b then genNaturalSmall else genNaturalBig

genNaturalSmall :: Gen Natural
genNaturalSmall = arbitrarySizedNatural

genNaturalBig :: Gen Natural
genNaturalBig = do
  x <- arbitrarySizedNatural :: Gen Natural
  -- arbitrarySizedNatural generates numbers smaller than
  -- (maxBound :: Word64), so let's make them bigger to better test
  -- the Binary instance.
  return (x + fromIntegral (maxBound :: Word64))
#endif

------------------------------------------------------------------------

genFingerprint :: Gen Fingerprint
genFingerprint = liftM2 Fingerprint arbitrary arbitrary

------------------------------------------------------------------------

#ifdef HAS_FIXED_CONSTRUCTOR

fixedPut :: forall a. Fixed.HasResolution a => Fixed.Fixed a -> Put
fixedPut x = put (truncate (x * fromInteger (Fixed.resolution (undefined :: Maybe a))) :: Integer)

fixedGet :: forall a. Fixed.HasResolution a => Get (Fixed.Fixed a)
fixedGet = (\x -> fromInteger x / fromInteger (Fixed.resolution (undefined :: Maybe a))) `liftA` get

-- | Serialise using base >=4.7 and <4.7 methods agree
prop_fixed_ser :: Fixed.Fixed Fixed.E3 -> Bool
prop_fixed_ser x = runPut (put x) == runPut (fixedPut x)

-- | Serialised with base >=4.7, unserialised with base <4.7 method roundtrip
prop_fixed_constr_resolution :: Fixed.Fixed Fixed.E3 -> Bool
prop_fixed_constr_resolution x = runGet fixedGet (runPut (put x)) == x

-- | Serialised with base <4.7, unserialised with base >=4.7 method roundtrip
prop_fixed_resolution_constr :: Fixed.Fixed Fixed.E3 -> Bool
prop_fixed_resolution_constr x = runGet get (runPut (fixedPut x)) == x

#endif

------------------------------------------------------------------------

data Generic256
    = C00 | C01 | C02 | C03 | C04 | C05 | C06 | C07 | C08 | C09 | C0a | C0b | C0c | C0d | C0e | C0f
    | C10 | C11 | C12 | C13 | C14 | C15 | C16 | C17 | C18 | C19 | C1a | C1b | C1c | C1d | C1e | C1f
    | C20 | C21 | C22 | C23 | C24 | C25 | C26 | C27 | C28 | C29 | C2a | C2b | C2c | C2d | C2e | C2f
    | C30 | C31 | C32 | C33 | C34 | C35 | C36 | C37 | C38 | C39 | C3a | C3b | C3c | C3d | C3e | C3f
    | C40 | C41 | C42 | C43 | C44 | C45 | C46 | C47 | C48 | C49 | C4a | C4b | C4c | C4d | C4e | C4f
    | C50 | C51 | C52 | C53 | C54 | C55 | C56 | C57 | C58 | C59 | C5a | C5b | C5c | C5d | C5e | C5f
    | C60 | C61 | C62 | C63 | C64 | C65 | C66 | C67 | C68 | C69 | C6a | C6b | C6c | C6d | C6e | C6f
    | C70 | C71 | C72 | C73 | C74 | C75 | C76 | C77 | C78 | C79 | C7a | C7b | C7c | C7d | C7e | C7f
    | C80 | C81 | C82 | C83 | C84 | C85 | C86 | C87 | C88 | C89 | C8a | C8b | C8c | C8d | C8e | C8f
    | C90 | C91 | C92 | C93 | C94 | C95 | C96 | C97 | C98 | C99 | C9a | C9b | C9c | C9d | C9e | C9f
    | Ca0 | Ca1 | Ca2 | Ca3 | Ca4 | Ca5 | Ca6 | Ca7 | Ca8 | Ca9 | Caa | Cab | Cac | Cad | Cae | Caf
    | Cb0 | Cb1 | Cb2 | Cb3 | Cb4 | Cb5 | Cb6 | Cb7 | Cb8 | Cb9 | Cba | Cbb | Cbc | Cbd | Cbe | Cbf
    | Cc0 | Cc1 | Cc2 | Cc3 | Cc4 | Cc5 | Cc6 | Cc7 | Cc8 | Cc9 | Cca | Ccb | Ccc | Ccd | Cce | Ccf
    | Cd0 | Cd1 | Cd2 | Cd3 | Cd4 | Cd5 | Cd6 | Cd7 | Cd8 | Cd9 | Cda | Cdb | Cdc | Cdd | Cde | Cdf
    | Ce0 | Ce1 | Ce2 | Ce3 | Ce4 | Ce5 | Ce6 | Ce7 | Ce8 | Ce9 | Cea | Ceb | Cec | Ced | Cee | Cef
    | Cf0 | Cf1 | Cf2 | Cf3 | Cf4 | Cf5 | Cf6 | Cf7 | Cf8 | Cf9 | Cfa | Cfb | Cfc | Cfd | Cfe | Cff
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary Generic256

instance Arbitrary Generic256 where
    arbitrary = elements [minBound..maxBound]

prop_Generic256 :: Generic256 -> Property
prop_Generic256 = roundTripWith put get

data GenericVoid deriving Generic

instance Binary GenericVoid

prop_GenericVoid_get :: L.ByteString -> Bool
prop_GenericVoid_get lbs =
    case decodeOrFail lbs :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, GenericVoid) of
      Left _  -> True
      Right _ -> False

prop_GenericVoid_put :: Bool
prop_GenericVoid_put = mustThrowError (L.length (encode (undefined :: GenericVoid)))

prop_GenericVoid_list :: Bool
prop_GenericVoid_list = null (decode (encode ([] :: [GenericVoid])) :: [GenericVoid])

prop_GenericVoid_nonEmptyList :: Bool
prop_GenericVoid_nonEmptyList =
    case decodeOrFail (encode (1 :: Int)) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, [GenericVoid]) of
      Left _  -> True
      Right _ -> False

data GenericRecord = GenericRecord Word8 Int16 Word32 B.ByteString
    deriving (Eq, Show, Generic)

instance Binary GenericRecord

instance Arbitrary GenericRecord where
    arbitrary = GenericRecord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_GenericRecord :: GenericRecord -> Property
prop_GenericRecord = roundTripWith put get

data GenericSmall
    = GSNullary
    | GSSingle Word32
    | GSProduct Word8 Int32 B.ByteString
    | GSNested (Maybe Word16) Bool
    deriving (Eq, Show, Generic)

instance Binary GenericSmall

instance Arbitrary GenericSmall where
    arbitrary = oneof
      [ return GSNullary
      , GSSingle <$> arbitrary
      , GSProduct <$> arbitrary <*> arbitrary <*> arbitrary
      , GSNested <$> arbitrary <*> arbitrary
      ]

prop_GenericSmall :: GenericSmall -> Property
prop_GenericSmall = roundTripWith put get

prop_GenericSmall_invalidTag :: Word8 -> Property
prop_GenericSmall_invalidTag w =
    w >= 4 ==>
      case decodeOrFail (encode w) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, GenericSmall) of
        Left _  -> True
        Right _ -> False

------------------------------------------------------------------------
-- PutM and container/class tests

prop_putM_monad_and_flush :: Word8 -> Word16 -> Word32 -> Property
prop_putM_monad_and_flush w8 w16 w32 =
    let pm :: PutM (Word8, Word16, Word32)
        pm = do
          a <- fmap (+ 1) (putWord8 w8 *> pure w8)
          flush
          b <- pure (+) <*> (putWord16be w16 *> pure w16) <*> pure 0
          c <- (putWord32be w32 *> pure w32) >>= \x -> pure x
          return (a, b, c)
        (res, lbs) = runPutM pm
        lbsExec    = Builder.toLazyByteString (execPut (putWord8 w8 `mappend` mempty `mappend` putWord16be w16 `mappend` putWord32be w32))
    in conjoin
         [ res === (w8 + 1, w16, w32)
         , lbs === lbsExec
         ]

prop_put_infinite_streaming :: Word8 -> Property
prop_put_infinite_streaming w =
    forAll (choose (0, 64)) $ \n ->
      let infMappend = putWord8 w `mappend` infMappend
          infSeq     = mapM_ putWord8 [w ..]
      in conjoin
           [ L.unpack (L.take (fromIntegral n) (runPut infMappend)) === replicate n w
           , L.unpack (L.take (fromIntegral n) (runPut infSeq)) === take n [w ..]
           ]

prop_ratio_zero_denom :: Int -> Bool
prop_ratio_zero_denom n =
    case decodeOrFail (encode (n, 0 :: Int)) :: Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, Ratio Int) of
      Left _  -> True
      Right _ -> False

genTree :: Gen (Tree.Tree Int8)
genTree = sized go
  where
    go n
      | n <= 0    = Tree.Node <$> arbitrary <*> return []
      | otherwise = do
          k <- choose (0, min 3 n)
          Tree.Node <$> arbitrary <*> vectorOf k (go (n `div` 2))

------------------------------------------------------------------------
-- Buffer representation tests (both PortableBuf and UnboxedBuf)

-- | Generate strict ByteStrings covering empty, full, leading-offset,
-- and mid-sliced buffers (important for exercising non-zero pointer offsets).
genSlicedBS :: Gen B.ByteString
genSlicedBS = do
    bs <- arbitrary
    let n = B.length bs
    if n == 0
      then return B.empty
      else do
        off <- choose (0, n)
        len <- choose (0, n - off)
        return (B.take len (B.drop off bs))

-- | Alignment-safe 4-byte little-endian read for testing bufReadWith.
peekWord32LEBytes :: Ptr Word32 -> IO Word32
peekWord32LEBytes ptr = do
    b0 <- peekByteOff ptr 0 :: IO Word8
    b1 <- peekByteOff ptr 1 :: IO Word8
    b2 <- peekByteOff ptr 2 :: IO Word8
    b3 <- peekByteOff ptr 3 :: IO Word8
    return $   fromIntegral b0
           .|. (fromIntegral b1 `shiftL` 8)
           .|. (fromIntegral b2 `shiftL` 16)
           .|. (fromIntegral b3 `shiftL` 24)

refWord32LE :: B.ByteString -> Word32
refWord32LE bs =
      fromIntegral (B.index bs 0)
  .|. (fromIntegral (B.index bs 1) `shiftL` 8)
  .|. (fromIntegral (B.index bs 2) `shiftL` 16)
  .|. (fromIntegral (B.index bs 3) `shiftL` 24)

data BufOp
  = OpLen
  | OpNull
  | OpHasN Int
  | OpTake Int
  | OpDrop Int
  | OpReadWord8
  | OpReadWord32LE
  deriving (Show)

data BufObs
  = ObsLen Int
  | ObsNull Bool
  | ObsHasN Bool
  | ObsTake B.ByteString
  | ObsDrop B.ByteString
  | ObsWord8 Word8
  | ObsWord32 Word32
  deriving (Eq, Show)

genBufOp :: Gen BufOp
genBufOp = oneof
  [ return OpLen
  , return OpNull
  , OpHasN <$> oneof [choose (-5, 5), arbitrary]
  , OpTake <$> choose (0, 64)
  , OpDrop <$> choose (0, 32)
  , return OpReadWord8
  , return OpReadWord32LE
  ]

runBufOpsRef :: B.ByteString -> [BufOp] -> ([BufObs], B.ByteString)
runBufOpsRef bs0 = go bs0 []
  where
    go bs acc [] = (reverse acc, bs)
    go bs acc (op : ops) =
      let len = B.length bs
      in case op of
           OpLen -> go bs (ObsLen len : acc) ops
           OpNull -> go bs (ObsNull (B.null bs) : acc) ops
           OpHasN n -> go bs (ObsHasN (len >= n) : acc) ops
           OpTake n ->
             let k = min n len
             in go bs (ObsTake (B.take k bs) : acc) ops
           OpDrop n ->
             let k = min n len
                 bs' = B.drop k bs
             in go bs' (ObsDrop bs' : acc) ops
           OpReadWord8
             | len >= 1  -> go bs (ObsWord8 (B.head bs) : acc) ops
             | otherwise -> go bs acc ops
           OpReadWord32LE
             | len >= 4  -> go bs (ObsWord32 (refWord32LE bs) : acc) ops
             | otherwise -> go bs acc ops

runBufOpsPortable :: B.ByteString -> [BufOp] -> ([BufObs], B.ByteString)
runBufOpsPortable bs0 = go (unpackPortableBuf bs0) []
  where
    go s acc [] = (reverse acc, packPortableBuf s)
    go s acc (op : ops) =
      let len = portableBufLength s
      in case op of
           OpLen -> go s (ObsLen len : acc) ops
           OpNull -> go s (ObsNull (portableBufNull s) : acc) ops
           OpHasN n -> go s (ObsHasN (portableBufHasN n s) : acc) ops
           OpTake n ->
             let k = min n len
             in go s (ObsTake (portableBufTake k s) : acc) ops
           OpDrop n ->
             let k = min n len
                 s' = portableBufDrop k s
             in go s' (ObsDrop (packPortableBuf s') : acc) ops
           OpReadWord8
             | portableBufHasN 1 s ->
                 go s (ObsWord8 (portableBufReadWith peek s) : acc) ops
             | otherwise -> go s acc ops
           OpReadWord32LE
             | portableBufHasN 4 s ->
                 go s (ObsWord32 (portableBufReadWith peekWord32LEBytes s) : acc) ops
             | otherwise -> go s acc ops

#ifdef __GLASGOW_HASKELL__
runBufOpsUnboxed :: B.ByteString -> [BufOp] -> ([BufObs], B.ByteString)
runBufOpsUnboxed bs0 = go (unpackUnboxedBuf bs0) []
  where
    go s acc [] = (reverse acc, packUnboxedBuf s)
    go s acc (op : ops) =
      let len = unboxedBufLength s
      in case op of
           OpLen -> go s (ObsLen len : acc) ops
           OpNull -> go s (ObsNull (unboxedBufNull s) : acc) ops
           OpHasN n -> go s (ObsHasN (unboxedBufHasN n s) : acc) ops
           OpTake n ->
             let k = min n len
             in go s (ObsTake (unboxedBufTake k s) : acc) ops
           OpDrop n ->
             let k = min n len
             in case unboxedBufDrop k s of
                  s' -> go s' (ObsDrop (packUnboxedBuf s') : acc) ops
           OpReadWord8
             | unboxedBufHasN 1 s ->
                 go s (ObsWord8 (unboxedBufReadWith peek s) : acc) ops
             | otherwise -> go s acc ops
           OpReadWord32LE
             | unboxedBufHasN 4 s ->
                 go s (ObsWord32 (unboxedBufReadWith peekWord32LEBytes s) : acc) ops
             | otherwise -> go s acc ops
#endif

runBufOpsActive :: B.ByteString -> [BufOp] -> ([BufObs], B.ByteString)
runBufOpsActive bs0 = go (unpackBuf bs0) []
  where
    go s acc [] = (reverse acc, packBuf s)
    go s acc (op : ops) =
      let len = bufLength s
      in case op of
           OpLen -> go s (ObsLen len : acc) ops
           OpNull -> go s (ObsNull (bufNull s) : acc) ops
           OpHasN n -> go s (ObsHasN (bufHasN n s) : acc) ops
           OpTake n ->
             let k = min n len
             in go s (ObsTake (bufTake k s) : acc) ops
           OpDrop n ->
             let k = min n len
             in case bufDrop k s of
                  s' -> go s' (ObsDrop (packBuf s') : acc) ops
           OpReadWord8
             | bufHasN 1 s ->
                 go s (ObsWord8 (bufReadWith peek s) : acc) ops
             | otherwise -> go s acc ops
           OpReadWord32LE
             | bufHasN 4 s ->
                 go s (ObsWord32 (bufReadWith peekWord32LEBytes s) : acc) ops
             | otherwise -> go s acc ops

prop_buf_pack_unpack :: Property
prop_buf_pack_unpack = forAll genSlicedBS $ \bs ->
    conjoin
      [ packPortableBuf (unpackPortableBuf bs) === bs
      , packBuf (unpackBuf bs) === bs
#ifdef __GLASGOW_HASKELL__
      , packUnboxedBuf (unpackUnboxedBuf bs) === bs
#endif
      ]

prop_buf_boundaries :: Property
prop_buf_boundaries = forAll genSlicedBS $ \bs ->
    let len = B.length bs
        ns  = [-1, 0, 1, len - 1, len, len + 1]
        pBuf = unpackPortableBuf bs
    in case unpackBuf bs of
      aBuf -> conjoin
        [ portableBufLength pBuf === len
        , portableBufNull pBuf === B.null bs
        , map (`portableBufHasN` pBuf) ns === map (len >=) ns
        , portableBufTake 0 pBuf === B.empty
        , portableBufTake len pBuf === bs
        , packPortableBuf (portableBufDrop 0 pBuf) === bs
        , packPortableBuf (portableBufDrop len pBuf) === B.empty
        , bufLength aBuf === len
        , bufNull aBuf === B.null bs
        , map (`bufHasN` aBuf) ns === map (len >=) ns
        , bufTake 0 aBuf === B.empty
        , bufTake len aBuf === bs
        , packBuf (bufDrop 0 aBuf) === bs
        , packBuf (bufDrop len aBuf) === B.empty
#ifdef __GLASGOW_HASKELL__
        , case unpackUnboxedBuf bs of
            uBuf -> conjoin
              [ unboxedBufLength uBuf === len
              , unboxedBufNull uBuf === B.null bs
              , map (`unboxedBufHasN` uBuf) ns === map (len >=) ns
              , unboxedBufTake 0 uBuf === B.empty
              , unboxedBufTake len uBuf === bs
              , packUnboxedBuf (unboxedBufDrop 0 uBuf) === bs
              , packUnboxedBuf (unboxedBufDrop len uBuf) === B.empty
              ]
#endif
        ]

prop_buf_ops_equiv :: Property
prop_buf_ops_equiv =
    forAll genSlicedBS $ \bs ->
    forAll (listOf genBufOp) $ \ops ->
      let ref = runBufOpsRef bs ops
      in conjoin
        [ runBufOpsPortable bs ops === ref
        , runBufOpsActive bs ops === ref
#ifdef __GLASGOW_HASKELL__
        , runBufOpsUnboxed bs ops === ref
#endif
        ]

prop_buf_empty :: Property
prop_buf_empty =
    forAll (listOf genBufOp) $ \ops ->
      let ref = runBufOpsRef B.empty ops
      in conjoin
        [ runBufOpsPortable B.empty ops === ref
        , runBufOpsActive B.empty ops === ref
#ifdef __GLASGOW_HASKELL__
        , runBufOpsUnboxed B.empty ops === ref
#endif
        ]

#ifdef __GLASGOW_HASKELL__
-- | Verify that unboxedBufReadWith keeps the underlying ForeignPtrContents
-- alive (via touch#) across the read callback, even if GC runs during the
-- callback when no other references to the ByteString remain.
prop_buf_readWith_liveness :: Word8 -> Property
prop_buf_readWith_liveness w = ioProperty $ do
    finalized <- newIORef False
    rawPtr <- mallocBytes 1 :: IO (Ptr Word8)
    poke rawPtr w
    fp <- FC.newForeignPtr rawPtr $ do
      -- Poison memory before freeing so a premature finalizer is caught
      -- both via the flag and via the read value.
      poke rawPtr (w + 1)
      writeIORef finalized True
      free rawPtr
    let bs = BI.fromForeignPtr fp 0 1
    (wasFinalized, got) <- C.evaluate $
      unboxedBufReadWith
        (\ptr -> do
           performGC
           fin <- readIORef finalized
           val <- peek (castPtr ptr :: Ptr Word8)
           return (fin, val))
        (unpackUnboxedBuf bs)
    return (not wasFinalized && got == w)
#endif

------------------------------------------------------------------------

type T a = a -> Property
type B a = a -> Bool

p :: (Testable p) => p -> Property
p = property

test    :: (Eq a, Binary a) => a -> Property
test a  = forAll positiveList (roundTrip a . refragment)

test' :: (Show a, Arbitrary a) => String -> (a -> Property) -> ([a] -> Property) -> Test
test' desc prop propList =
  testGroup desc [
    testProperty desc prop,
    testProperty ("[" ++ desc ++ "]") propList
  ]

testWithGen :: (Show a, Eq a, Binary a) => String -> Gen a -> Test
testWithGen desc gen =
  testGroup desc [
    testProperty desc (forAll gen test),
    testProperty ("[" ++ desc ++ "]") (forAll (listOf gen) test)
  ]

positiveList :: Gen [Int]
positiveList = fmap (filter (/=0) . map abs) $ arbitrary

tests :: [Test]
tests =
        [ testGroup "Utils"
            [ testProperty "refragment id" (p prop_refragment)
            , testProperty "refragment invariant" (p prop_refragment_inv)
            ]

        , testGroup "Boundaries"
            [ testProperty "read to much"         (p (prop_readTooMuch :: B Word8))
            , testProperty "read negative length" (p (prop_getByteString_negative :: T Int))
            , testProperty "read lazy negative length" (p prop_getLazyByteString_negative)
            , testProperty "skip negative length" (p prop_skip_negative)
            , testProperty "isolate negative length" (p prop_isolate_negative)
            , testProperty "isEmpty" (p prop_isEmpty)
            , testProperty "isEmpty with empty chunks" (p prop_isEmpty_emptyChunks)
            , testProperty "Alternative empty/mzero/mplus/some/many" (p prop_alternative_empty_some_many)
            , testProperty "readN and ensureN" (p prop_readN_ensureN)
            , testProperty "Internal get and put" (p prop_internal_get_put)
            , testProperty "list negative length" (p prop_list_negative_length)
            , testProperty "seq negative length"  (p prop_seq_negative_length)
            , testProperty "Maybe invalid tag"    (p prop_maybe_invalid_tag)
            , testProperty "Either invalid tag"   (p prop_either_invalid_tag)
            , testProperty "Integer invalid tag"  (p prop_integer_invalid_tag)
            , testProperty "Integer invalid sign" (p prop_integer_invalid_sign)
#ifdef HAS_NATURAL
            , testProperty "Natural invalid tag"  (p prop_natural_invalid_tag)
#endif
            , testProperty "Ratio zero denominator" (p prop_ratio_zero_denom)
            , -- Arbitrary test input
              let testInput :: [Int] ; testInput = [0 .. 10]
              in testProperty "look-ahead independent of chunking" (p (prop_lookAheadIndepOfChunking testInput))
            ]

        , testGroup "Partial"
            [ testProperty "partial" (p prop_partial)
            , testProperty "fail"    (p prop_fail)
            , testProperty "bytesRead" (p prop_bytesRead)
            , testProperty "partial only once" (p prop_partialOnlyOnce)
            , testProperty "runGet/runGetOrFail equiv runGetIncremental" (p prop_runGet_equiv_incremental)
            , testProperty "pushChunk and pushEndOfInput on Done/Fail" (p prop_pushChunk_and_pushEndOfInput)
            ]

        , testGroup "Buf"
            [ testProperty "pack . unpack == id" prop_buf_pack_unpack
            , testProperty "boundary operations" prop_buf_boundaries
            , testProperty "operation sequences (portable vs unboxed vs ref)" prop_buf_ops_equiv
            , testProperty "empty buffer operations" prop_buf_empty
#ifdef __GLASGOW_HASKELL__
            , testProperty "unboxedBufReadWith keeps ForeignPtr alive across GC" prop_buf_readWith_liveness
#endif
            ]

        , testGroup "Model"
            Action.tests

        , testGroup "Primitives"
            [ testProperty "Word8"      (p prop_Word8)
            , testProperty "Word16be"   (p prop_Word16be)
            , testProperty "Word16le"   (p prop_Word16le)
            , testProperty "Word16host" (p prop_Word16host)
            , testProperty "Word32be"   (p prop_Word32be)
            , testProperty "Word32le"   (p prop_Word32le)
            , testProperty "Word32host" (p prop_Word32host)
            , testProperty "Word64be"   (p prop_Word64be)
            , testProperty "Word64le"   (p prop_Word64le)
            , testProperty "Word64host" (p prop_Word64host)
            , testProperty "Wordhost"   (p prop_Wordhost)
              -- Int
            , testProperty "Int8"       (p prop_Int8)
            , testProperty "Int16be"    (p prop_Int16be)
            , testProperty "Int16le"    (p prop_Int16le)
            , testProperty "Int16host"  (p prop_Int16host)
            , testProperty "Int32be"    (p prop_Int32be)
            , testProperty "Int32le"    (p prop_Int32le)
            , testProperty "Int32host"  (p prop_Int32host)
            , testProperty "Int64be"    (p prop_Int64be)
            , testProperty "Int64le"    (p prop_Int64le)
            , testProperty "Int64host"  (p prop_Int64host)
            , testProperty "Inthost"    (p prop_Inthost)
              -- Float/Double
            , testProperty "Floatbe"    (p prop_Floatbe)
            , testProperty "Floatle"    (p prop_Floatle)
            , testProperty "Floathost"  (p prop_Floathost)
            , testProperty "Doublebe"   (p prop_Doublebe)
            , testProperty "Doublele"   (p prop_Doublele)
            , testProperty "Doublehost" (p prop_Doublehost)
            ]

        , testGroup "String utils"
            [ testProperty "getLazyByteString"          prop_getLazyByteString
            , testProperty "getLazyByteStringNul"       prop_getLazyByteStringNul
            , testProperty "getLazyByteStringNul No Null" prop_getLazyByteStringNul_noNul
            , testProperty "getRemainingLazyByteString" prop_getRemainingLazyByteString
            ]

        , testGroup "Using Binary class, refragmented ByteString"
            [ test' "()"          (test :: T ()         ) test
            , test' "Bool"        (test :: T Bool       ) test
            , test' "Char"        (test :: T Char       ) test
            , test' "Ordering"    (test :: T Ordering   ) test
            , test' "Ratio Int"   (test :: T (Ratio Int)) test

            , test' "Word"        (test :: T Word  ) test
            , test' "Word8"       (test :: T Word8 ) test
            , test' "Word16"      (test :: T Word16) test
            , test' "Word32"      (test :: T Word32) test
            , test' "Word64"      (test :: T Word64) test

            , test' "Int"         (test :: T Int  ) test
            , test' "Int8"        (test :: T Int8 ) test
            , test' "Int16"       (test :: T Int16) test
            , test' "Int32"       (test :: T Int32) test
            , test' "Int64"       (test :: T Int64) test

            , testWithGen "Integer mixed" genInteger
            , testWithGen "Integer small" genIntegerSmall
            , testWithGen "Integer big"   genIntegerBig

            , test' "Fixed"       (test :: T (Fixed.Fixed Fixed.E3) ) test
#ifdef HAS_NATURAL
            , testWithGen "Natural mixed" genNatural
            , testWithGen "Natural small" genNaturalSmall
            , testWithGen "Natural big"   genNaturalBig
#endif
            , testWithGen "GHC.Fingerprint" genFingerprint

            , test' "Float"       (test :: T Float ) test
            , test' "Double"      (test :: T Double) test

            , test' "((), ())"            (test :: T ((), ())            ) test
            , test' "(Word8, Word32)"     (test :: T (Word8, Word32)     ) test
            , test' "(Int8, Int32)"       (test :: T (Int8,  Int32)      ) test
            , test' "(Int32, [Int])"      (test :: T (Int32, [Int])      ) test
            , test' "Maybe Int8"          (test :: T (Maybe Int8)        ) test
            , test' "Either Int8 Int16"   (test :: T (Either Int8 Int16) ) test

            , test' "(Int, ByteString)"
                    (test     :: T (Int, B.ByteString)   ) test
            , test' "[(Int, ByteString)]"
                    (test     :: T [(Int, B.ByteString)] ) test

            , test' "(Maybe Int64, Bool, [Int])"
                    (test :: T (Maybe Int64, Bool, [Int])) test
            , test' "(Maybe Word8, Bool, [Int], Either Bool Word8)"
                    (test :: T (Maybe Word8, Bool, [Int], Either Bool Word8)) test
            , test' "(Maybe Word16, Bool, [Int], Either Bool Word16, Int)"
                    (test :: T (Maybe Word16, Bool, [Int], Either Bool Word16, Int)) test

            , test' "(Int,Int,Int,Int,Int,Int)"
                      (test :: T (Int,Int,Int,Int,Int,Int)) test
            , test' "(Int,Int,Int,Int,Int,Int,Int)"
                      (test :: T (Int,Int,Int,Int,Int,Int,Int)) test
            , test' "(Int,Int,Int,Int,Int,Int,Int,Int)"
                      (test :: T (Int,Int,Int,Int,Int,Int,Int,Int)) test
            , test' "(Int,Int,Int,Int,Int,Int,Int,Int,Int)"
                      (test :: T (Int,Int,Int,Int,Int,Int,Int,Int,Int)) test
            , test' "(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)"
                      (test :: T (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)) test

            , test' "B.ByteString" (test :: T B.ByteString) test
            , test' "L.ByteString" (test :: T L.ByteString) test
            , test' "ShortByteString" (test :: T ShortByteString) test

            , testWithGen "Set Int"         (Set.fromList <$> (arbitrary :: Gen [Int]))
            , testWithGen "Map Int Word16"  (Map.fromList <$> (arbitrary :: Gen [(Int, Word16)]))
            , testWithGen "IntSet"          (IntSet.fromList <$> (arbitrary :: Gen [Int]))
            , testWithGen "IntMap Word16"   (IntMap.fromList <$> (arbitrary :: Gen [(Int, Word16)]))
            , testWithGen "Seq Int"         (Seq.fromList <$> (arbitrary :: Gen [Int]))
            , testWithGen "Tree Int8"       genTree
            , testWithGen "Complex Double"  (liftM2 (:+) (arbitrary :: Gen Double) arbitrary)
            , testWithGen "Identity Int"    (Identity <$> (arbitrary :: Gen Int))
            , testWithGen "NonEmpty Int"    (liftM2 (:|) (arbitrary :: Gen Int) arbitrary)
            , testWithGen "Proxy Int"       (return (Proxy :: Proxy Int))
            ]

        , testGroup "Put"
            [ testProperty "PutM Functor/Applicative/Monad/flush/execPut" (p prop_putM_monad_and_flush)
            , testProperty "Put infinite streaming" (p prop_put_infinite_streaming)
            ]

        , testGroup "Invariants" $ map (uncurry testProperty)
            [ ("B.ByteString invariant",   p (prop_invariant :: B B.ByteString                 ))
            , ("[B.ByteString] invariant", p (prop_invariant :: B [B.ByteString]               ))
            , ("L.ByteString invariant",   p (prop_invariant :: B L.ByteString                 ))
            , ("[L.ByteString] invariant", p (prop_invariant :: B [L.ByteString]               ))
            , ("ShortByteString invariant",  p (prop_invariant :: B ShortByteString            ))
            , ("[ShortByteString] invariant", p (prop_invariant :: B [ShortByteString]         ))
            ]
#ifdef HAS_FIXED_CONSTRUCTOR
        , testGroup "Fixed"
            [ testProperty "Serialisation same"       $ p prop_fixed_ser
            , testProperty "MkFixed -> HasResolution" $ p prop_fixed_constr_resolution
            , testProperty "HasResolution -> MkFixed" $ p prop_fixed_resolution_constr
            ]
#endif
        , testTypeable

        , testGroup "Generic"
            [ testProperty "Generic256" prop_Generic256
            , testProperty "GenericRecord (products/fields)" prop_GenericRecord
            , testProperty "GenericSmall (small sum + products)" prop_GenericSmall
            , testProperty "GenericSmall invalid tag fails" prop_GenericSmall_invalidTag
            , testProperty "GenericVoid get fails" prop_GenericVoid_get
            , testProperty "GenericVoid put is strict" prop_GenericVoid_put
            , testProperty "GenericVoid empty list" prop_GenericVoid_list
            , testProperty "GenericVoid non-empty list fails" prop_GenericVoid_nonEmptyList
            ]

        , testGroup "Char"
            [ testProperty "encodings are unique" $
                withMaxSize 4 $ \bs ->
                  case runGetOrFail (get :: Get Char) bs of
                    Left _ -> discard
                    Right (_, n, x) -> runPut (put x) === L.take n bs
            ]
        ]
