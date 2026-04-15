{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as S

instance Arbitrary L.ByteString where
  arbitrary = fmap L.fromChunks arbitrary

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary

instance Arbitrary S.ShortByteString where
  arbitrary = S.toShort `fmap` arbitrary
