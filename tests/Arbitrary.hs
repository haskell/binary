{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if MIN_VERSION_base(4,8,0)
#define HAS_NATURAL
#endif

module Arbitrary where

import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

#ifdef HAS_NATURAL
import Numeric.Natural
#endif

instance Arbitrary L.ByteString where
  arbitrary = fmap L.fromChunks arbitrary

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e,
          Arbitrary f) =>
         Arbitrary (a,b,c,d,e,f) where
  arbitrary = do
    (a,b,c,d,e) <- arbitrary
    f <- arbitrary
    return (a,b,c,d,e,f)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e,
          Arbitrary f, Arbitrary g) =>
         Arbitrary (a,b,c,d,e,f,g) where
  arbitrary = do
    (a,b,c,d,e) <- arbitrary
    (f,g) <- arbitrary
    return (a,b,c,d,e,f,g)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e,
          Arbitrary f, Arbitrary g, Arbitrary h) =>
         Arbitrary (a,b,c,d,e,f,g,h) where
  arbitrary = do
    (a,b,c,d,e) <- arbitrary
    (f,g,h) <- arbitrary
    return (a,b,c,d,e,f,g,h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e,
          Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i) =>
         Arbitrary (a,b,c,d,e,f,g,h,i) where
  arbitrary = do
    (a,b,c,d,e) <- arbitrary
    (f,g,h,i) <- arbitrary
    return (a,b,c,d,e,f,g,h,i)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e,
          Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i, Arbitrary j) =>
         Arbitrary (a,b,c,d,e,f,g,h,i,j) where
  arbitrary = do
    (a,b,c,d,e) <- arbitrary
    (f,g,h,i,j) <- arbitrary
    return (a,b,c,d,e,f,g,h,i,j)


#ifdef HAS_NATURAL
-- | Generates a natural number. The number must be positive
-- and its maximum value depends on the size parameter.
arbitrarySizedNatural :: Gen Natural
arbitrarySizedNatural =
  sized $ \n0 ->
  let n = toInteger n0 in
  inBounds fromInteger (choose (0, n*n))

inBounds :: Integral a => (Integer -> a) -> Gen Integer -> Gen a
inBounds fi g = fmap fi (g `suchThat` (\x -> toInteger (fi x) == x))
#endif