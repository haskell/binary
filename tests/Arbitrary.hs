{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

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
