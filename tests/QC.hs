{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

-- import Parallel

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)

import Data.Word
import Data.Int

import qualified Control.OldException as C (catch,evaluate)
import Control.Monad
import System.Environment
import System.IO
import System.IO.Unsafe

import Test.QuickCheck
-- import QuickCheckUtils
import Text.Printf

-- import qualified Data.Sequence as Seq

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Monoid

------------------------------------------------------------------------

roundTrip :: (Eq a, Binary a) => a -> (L.ByteString -> L.ByteString) -> Bool
roundTrip a f = a ==
    {-# SCC "decode.refragment.encode" #-} decode (f (encode a))

roundTripWith put get x =
    forAll positiveList $ \xs ->
    x == runGet get (refragment xs (runPut (put x)))

-- make sure that a test fails
mustThrowError :: B a
mustThrowError a = unsafePerformIO $
    C.catch (do C.evaluate a
                return False)
            (\_ -> return True)

-- low level ones:

prop_Word16be = roundTripWith putWord16be getWord16be
prop_Word16le = roundTripWith putWord16le getWord16le
prop_Word16host = roundTripWith putWord16host getWord16host

prop_Word32be = roundTripWith putWord32be getWord32be
prop_Word32le = roundTripWith putWord32le getWord32le
prop_Word32host = roundTripWith putWord32host getWord32host

prop_Word64be = roundTripWith putWord64be getWord64be
prop_Word64le = roundTripWith putWord64le getWord64le
prop_Word64host = roundTripWith putWord64host getWord64host

prop_Wordhost = roundTripWith putWordhost getWordhost


-- done, partial and fail

-- | Test partial results.
-- May or may not use the whole input, check conditions for the different
-- outcomes.
prop_partial :: L.ByteString -> Property
prop_partial lbs = forAll (choose (0, L.length lbs * 2)) $ \skip ->
  let result = feedAll (runGetPartial parser) (L.toChunks lbs)
      parser = do
        s <- getByteString (fromIntegral skip)
        return (L.fromChunks [s])
  in case result of
       Partial _ -> L.length lbs < skip
       Done remaining pos value ->
         all id [ L.length value == skip
                , L.append value (L.fromChunks [remaining]) == lbs
                ]
       Fail _ _ _ -> False

-- | Fail a parser and make sure the result is sane.
prop_fail :: L.ByteString -> String -> Property
prop_fail lbs msg = forAll (choose (0, L.length lbs)) $ \pos ->
  let result = feedAll (runGetPartial parser) (L.toChunks lbs)
      parser = do
        -- use part of the input...
        _ <- getByteString (fromIntegral pos)
        -- ... then fail
        fail msg
  in case result of
     Fail remaining pos' msg' ->
       all id [ pos == pos'
              , msg == msg'
              , L.length lbs - pos == fromIntegral (B.length remaining)
              , L.fromChunks [remaining] `L.isSuffixOf` lbs
              ]
     _ -> False -- wuut?

feedAll r (x:xs) = feedAll (r `feed` x) xs
feedAll r [] = r

-- read too much:

prop_readTooMuch x = mustThrowError $ x == a && x /= b
  where
    -- encode 'a', but try to read 'b' too
    (a,b) = decode (encode x)
    types = [a,b]

-- sanity:

invariant_lbs :: L.ByteString -> Bool
invariant_lbs (L.Empty)      = True
invariant_lbs (L.Chunk x xs) = not (B.null x) && invariant_lbs xs

prop_invariant :: (Binary a) => a -> Bool
prop_invariant = invariant_lbs . encode

-- refragment a lazy bytestring's chunks
refragment :: [Int] -> L.ByteString -> L.ByteString
refragment [] lps = lps
refragment (x:xs) lps =
    let x' = fromIntegral . (+1) . abs $ x
        rest = refragment xs (L.drop x' lps) in
    L.append (L.fromChunks [B.concat . L.toChunks . L.take x' $ lps]) rest

-- check identity of refragmentation
prop_refragment lps xs = lps == refragment xs lps

-- check that refragmention still hold invariant
prop_refragment_inv lps xs = invariant_lbs $ refragment xs lps

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------

type T a = a -> Property
type B a = a -> Bool

p :: (Testable p) => p -> Property
p = property

test    :: (Eq a, Binary a) => a -> Property
test a  = forAll positiveList (roundTrip a . refragment)

positiveList :: Gen [Int]
positiveList = fmap (filter (/=0) . map abs) $ arbitrary

tests =
        [ testGroup "Utils"
            [ testProperty "refragment id" (p prop_refragment)
            , testProperty "refragment invariant" (p prop_refragment_inv)
            ]

        , testGroup "Boundaries"
            [ testProperty "read to much" (p (prop_readTooMuch :: B Word8))
            ]

        , testGroup "Partial"
            [ testProperty "partial" (p prop_partial)
            , testProperty "fail"    (p prop_fail)
            ]

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

        , testGroup "Using Binary class, refragmented ByteString" $ map (uncurry testProperty)
            [ ("()",         p (test :: T ()                     ))
            , ("Bool",       p (test :: T Bool                   ))

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

            , ("(Maybe Word8, Bool, [Int], Either Bool Word8)",
                    p (test :: T (Maybe Word8, Bool, [Int], Either Bool Word8) ))

            , ("(Int, ByteString)",        p (test     :: T (Int, B.ByteString)   ))
            , ("[(Int, ByteString)]",      p (test     :: T [(Int, B.ByteString)] ))

            , ("(Maybe Int64, Bool, [Int])", p (test :: T (Maybe Int64, Bool, [Int])))
            , ("(Maybe Word16, Bool, [Int], Either Bool Word16, Int)", p (test :: T (Maybe Word16, Bool, [Int], Either Bool Word16, Int) ))

    {-
            , ("IntSet",            p (test      :: T IntSet.IntSet          ))
            , ("IntMap ByteString", p (test      :: T (IntMap.IntMap B.ByteString) ))
    -}

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

-- GHC only:
--      ,("Sequence", p (roundTrip :: Seq.Seq Int64 -> Bool))

instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . L.fromChunks . filter (not. B.null) -- maintain the invariant.

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary
