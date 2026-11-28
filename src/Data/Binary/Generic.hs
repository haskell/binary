{-# LANGUAGE BangPatterns, CPP, EmptyCase, FlexibleInstances, KindSignatures,
    ScopedTypeVariables, TypeOperators, TypeSynonymInstances #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Generic
-- Copyright   : Bryan O'Sullivan
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Bryan O'Sullivan <bos@serpentine.com>
-- Stability   : unstable
-- Portability : Only works with GHC 7.2 and newer
--
-- Instances for supporting GHC generics.
--
-----------------------------------------------------------------------------
module Data.Binary.Generic
    (
    ) where

import Control.Applicative
import Data.Binary.Class
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Kind (Type)
import GHC.Generics
import Prelude -- Silence AMP warning.

-- Type without constructors
instance GBinaryPut V1 where
    gput x = case x of {}
    {-# INLINE gput #-}

instance GBinaryGet V1 where
    gget   = fail "Cannot decode uninhabited type"

-- Constructor without arguments
instance GBinaryPut U1 where
    gput U1 = pure ()
    {-# INLINE gput #-}

instance GBinaryGet U1 where
    gget    = return U1

-- Product: constructor with parameters
instance (GBinaryPut a, GBinaryPut b) => GBinaryPut (a :*: b) where
    gput (x :*: y) = gput x <> gput y
    {-# INLINE gput #-}

instance (GBinaryGet a, GBinaryGet b) => GBinaryGet (a :*: b) where
    gget = (:*:) <$> gget <*> gget

-- Metadata (constructor name, etc)
instance GBinaryPut a => GBinaryPut (M1 i c a) where
    gput = gput . unM1
    {-# INLINE gput #-}

instance GBinaryGet a => GBinaryGet (M1 i c a) where
    gget = M1 <$> gget

-- Constants, additional parameters, and rank-1 recursion
instance Binary a => GBinaryPut (K1 i a) where
    gput = put . unK1
    {-# INLINE gput #-}

instance Binary a => GBinaryGet (K1 i a) where
    gget = K1 <$> get

-- Borrowed from the cereal package.

-- The following GBinary instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = putSum (Proxy :: Proxy WORD) 0
#define GETSUM(WORD) GUARD(WORD) = (get :: Get WORD) >>= checkGetSum size . fromIntegral

instance ( GSumPut  a, GSumPut  b
         , SumSize    a, SumSize    b) => GBinaryPut (a :+: b) where
    gput | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE[0] gput #-}

instance ( GSumGet  a, GSumGet  b
         , SumSize    a, SumSize    b) => GBinaryGet (a :+: b) where
    gget | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE[0] gget #-}

sizeError :: Show size => String -> size -> error
sizeError s size =
    error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum :: (GSumGet f) => Word64 -> Word64 -> Get (f a)
checkGetSum size code
    | code < size = getSumWith (size > 8) id code
    | otherwise   = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GSumGet f where
    getSumWith :: Bool -> (f a -> r) -> Word64 -> Get r

class GSumPut f where
    putSum :: (Binary word, Num word) => Proxy word -> Word64 -> f a -> Put

instance (GSumGet a, GSumGet b, SumSize a) => GSumGet (a :+: b) where
    getSumWith large k !code
        | code < sizeL = getSumWith large (k . L1) code
        | otherwise    = getSumWith large (k . R1) (code - sizeL)
      where
        sizeL = unTagged (sumSize :: Tagged a Word64)
    {-# INLINE getSumWith #-}

instance (GSumPut a, GSumPut b, SumSize a) => GSumPut (a :+: b) where
    putSum p !code s = case s of
        L1 x -> putSum p code           x
        R1 x -> putSum p (code + sizeL) x
      where
        sizeL = unTagged (sumSize :: Tagged a Word64)
    {-# INLINE putSum #-}

instance GBinaryGet a => GSumGet (C1 c a) where
    getSumWith large k _
      | large     = fmapSum k gget
      | otherwise = k <$> gget
    {-# INLINE getSumWith #-}

fmapSum :: (a -> b) -> Get a -> Get b
fmapSum f g = f <$> g
{-# NOINLINE fmapSum #-}

instance GBinaryPut a => GSumPut (C1 c a) where
    putSum (_ :: Proxy word) !code x = put (fromIntegral code :: word) <> gput x
    {-# INLINE putSum #-}

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

newtype Tagged (s :: Type -> Type) b = Tagged {unTagged :: b}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)
    {-# INLINE sumSize #-}

instance SumSize (C1 c a) where
    sumSize = Tagged 1
    {-# INLINE sumSize #-}
