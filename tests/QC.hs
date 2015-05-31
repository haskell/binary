{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Main ( main ) where

#if MIN_VERSION_base(4,8,0)
#define HAS_NATURAL
#endif

import           Control.Applicative
import           Control.Exception                    as C (SomeException,
                                                            catch, evaluate)
import           Control.Monad                        (unless)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as L
import qualified Data.ByteString.Lazy.Internal        as L
import           Data.Int
import           Data.Ratio
import           System.IO.Unsafe

#ifdef HAS_NATURAL
import           Numeric.Natural
#endif

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import qualified Action                               (tests)
import           Arbitrary                            ()
import           Data.Binary
import           Data.Binary.Get
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
-- input, and to get knownledge of the current position using 'BytesRead'.
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

#ifdef HAS_NATURAL
prop_test_Natural :: Property
prop_test_Natural = forAll (gen :: Gen Natural) test
  where
    gen :: Gen Natural
    gen = do
      b <- arbitrary
      if b
        then do
          x <- arbitrarySizedNatural :: Gen Natural
          -- arbitrarySizedNatural generates numbers smaller than
          -- (maxBound :: Word64), so let's make them bigger to better test
          -- the Binary instance.
          return (x + fromIntegral (maxBound :: Word64))
        else arbitrarySizedNatural
#endif

------------------------------------------------------------------------

type T a = a -> Property
type B a = a -> Bool

p :: (Testable p) => p -> Property
p = property

test    :: (Eq a, Binary a) => a -> Property
test a  = forAll positiveList (roundTrip a . refragment)

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
            , -- Arbitrary test input
              let testInput :: [Int] ; testInput = [0 .. 10]
              in testProperty "look-ahead independent of chunking" (p (prop_lookAheadIndepOfChunking testInput))
            ]

        , testGroup "Partial"
            [ testProperty "partial" (p prop_partial)
            , testProperty "fail"    (p prop_fail)
            , testProperty "bytesRead" (p prop_bytesRead)
            , testProperty "partial only once" (p prop_partialOnlyOnce)
            ]

        , testGroup "Model"
            Action.tests

        , testGroup "Primitives"
            [ testProperty "Word16be"   (p prop_Word16be)
            , testProperty "Word16le"   (p prop_Word16le)
            , testProperty "Word16host" (p prop_Word16host)
            , testProperty "Word32be"   (p prop_Word32be)
            , testProperty "Word32le"   (p prop_Word32le)
            , testProperty "Word32host" (p prop_Word32host)
            , testProperty "Word64be"   (p prop_Word64be)
            , testProperty "Word64le"   (p prop_Word64le)
            , testProperty "Word64host" (p prop_Word64host)
            , testProperty "Wordhost"   (p prop_Wordhost)
            ]

        , testGroup "String utils"
            [ testProperty "getLazyByteString"          prop_getLazyByteString
            , testProperty "getLazyByteStringNul"       prop_getLazyByteStringNul
            , testProperty "getLazyByteStringNul No Null" prop_getLazyByteStringNul_noNul
            , testProperty "getRemainingLazyByteString" prop_getRemainingLazyByteString
            ]

        , testGroup "Using Binary class, refragmented ByteString" $ map (uncurry testProperty)
            [ ("()",         p (test :: T ()                     ))
            , ("Bool",       p (test :: T Bool                   ))
            , ("Ordering",   p (test :: T Ordering               ))
            , ("Ratio Int",  p (test :: T (Ratio Int)            ))


            , ("Word8",      p (test :: T Word8                  ))
            , ("Word16",     p (test :: T Word16                 ))
            , ("Word32",     p (test :: T Word32                 ))
            , ("Word64",     p (test :: T Word64                 ))

            , ("Int8",       p (test :: T Int8                   ))
            , ("Int16",      p (test :: T Int16                  ))
            , ("Int32",      p (test :: T Int32                  ))
            , ("Int64",      p (test :: T Int64                  ))

            , ("Word",       p (test :: T Word                   ))
            , ("Int",        p (test :: T Int                    ))
            , ("Integer",    p (test :: T Integer                ))
#ifdef HAS_NATURAL
            , ("Natural",      (prop_test_Natural :: Property    ))
#endif

            , ("Float",      p (test :: T Float                  ))
            , ("Double",     p (test :: T Double                 ))

            , ("Char",       p (test :: T Char                   ))

            , ("[()]",       p (test :: T [()]                  ))
            , ("[Word8]",    p (test :: T [Word8]               ))
            , ("[Word32]",   p (test :: T [Word32]              ))
            , ("[Word64]",   p (test :: T [Word64]              ))
            , ("[Word]",     p (test :: T [Word]                ))
            , ("[Int]",      p (test :: T [Int]                 ))
            , ("[Integer]",  p (test :: T [Integer]             ))
            , ("String",     p (test :: T String                ))
            , ("((), ())",           p (test :: T ((), ())        ))
            , ("(Word8, Word32)",    p (test :: T (Word8, Word32) ))
            , ("(Int8, Int32)",      p (test :: T (Int8,  Int32)  ))
            , ("(Int32, [Int])",     p (test :: T (Int32, [Int])  ))

            , ("Maybe Int8",         p (test :: T (Maybe Int8)        ))
            , ("Either Int8 Int16",  p (test :: T (Either Int8 Int16) ))

            , ("(Int, ByteString)",
                      p (test     :: T (Int, B.ByteString)   ))
            , ("[(Int, ByteString)]",
                      p (test     :: T [(Int, B.ByteString)] ))

            , ("(Maybe Int64, Bool, [Int])",
                      p (test :: T (Maybe Int64, Bool, [Int])))
            , ("(Maybe Word8, Bool, [Int], Either Bool Word8)",
                      p (test :: T (Maybe Word8, Bool, [Int], Either Bool Word8) ))
            , ("(Maybe Word16, Bool, [Int], Either Bool Word16, Int)",
                      p (test :: T (Maybe Word16, Bool, [Int], Either Bool Word16, Int) ))

            , ("(Int,Int,Int,Int,Int,Int)",
                      p (test :: T (Int,Int,Int,Int,Int,Int)))
            , ("(Int,Int,Int,Int,Int,Int,Int)",
                      p (test :: T (Int,Int,Int,Int,Int,Int,Int)))
            , ("(Int,Int,Int,Int,Int,Int,Int,Int)",
                      p (test :: T (Int,Int,Int,Int,Int,Int,Int,Int)))
            , ("(Int,Int,Int,Int,Int,Int,Int,Int,Int)",
                      p (test :: T (Int,Int,Int,Int,Int,Int,Int,Int,Int)))
            , ("(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)",
                      p (test :: T (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)))

            , ("B.ByteString",  p (test :: T B.ByteString        ))
            , ("L.ByteString",  p (test :: T L.ByteString        ))
            ]

        , testGroup "Invariants" $ map (uncurry testProperty)
            [ ("B.ByteString invariant",   p (prop_invariant :: B B.ByteString                 ))
            , ("[B.ByteString] invariant", p (prop_invariant :: B [B.ByteString]               ))
            , ("L.ByteString invariant",   p (prop_invariant :: B L.ByteString                 ))
            , ("[L.ByteString] invariant", p (prop_invariant :: B [L.ByteString]               ))
            ]
        ]
