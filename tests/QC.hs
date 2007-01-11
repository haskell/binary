{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Data.Binary

import Control.Monad
import Foreign
import System.Environment
import Test.QuickCheck.Parallel

roundTrip :: (Eq a, Binary a) => a -> Bool
roundTrip a = a == decode (encode a)

roundTripWith put get x = x == runGetM get (runPutM (put x))

instance Arbitrary Word8 where
    arbitrary = liftM fromIntegral (choose (0, 2^8-1))
    coarbitrary w = variant 0

instance Arbitrary Word16 where
    arbitrary = liftM fromIntegral (choose (0, 2^16-1))

instance Arbitrary Word32 where
    arbitrary = liftM fromIntegral (choose (0, 2^32-1))

instance Arbitrary Word64 where
    arbitrary = liftM fromIntegral (choose (0, 2^64-1))

instance Arbitrary Char where
    arbitrary = choose (maxBound, minBound)

instance Arbitrary a => Arbitrary (Maybe a) where
    arbitrary = oneof [ return Nothing, liftM Just arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = oneof [ liftM Left arbitrary, liftM Right arbitrary]

-- low level ones:

prop_Word16be = roundTripWith putWord16be getWord16be
prop_Word16le = roundTripWith putWord16le getWord16le

prop_Word32be = roundTripWith putWord32be getWord32be
prop_Word32le = roundTripWith putWord32le getWord32le

prop_Word64be = roundTripWith putWord64be getWord64be
prop_Word64le = roundTripWith putWord64le getWord64le

-- higher level ones using the Binary class

prop_Word8 :: Word8 -> Bool
prop_Word8 = roundTrip

prop_Word16 :: Word16 -> Bool
prop_Word16 = roundTrip

prop_Word32 :: Word32 -> Bool
prop_Word32 = roundTrip

prop_Word64 :: Word64 -> Bool
prop_Word64 = roundTrip

prop_List :: [Word8] -> Bool
prop_List = roundTrip

prop_Maybe :: Maybe Word8 -> Bool
prop_Maybe = roundTrip

prop_Either :: Either Word8 Word16 -> Bool
prop_Either = roundTrip

prop_Char :: Char -> Bool
prop_Char = roundTrip

prop_String :: String -> Bool
prop_String = roundTrip

main = do
    args <- getArgs
    n <- case args of
            []  -> return 1
            (s:_) -> readIO s
    pRun n 1000 $ take (max 100 (length tests)) $ cycle tests
    where
    tests =
        [ ("Word8", pDet prop_Word8)
        , ("Word16", pDet prop_Word16)
        , ("Word32", pDet prop_Word32)
        , ("Word64", pDet prop_Word64)
        , ("Word16be", pDet prop_Word16be)
        , ("Word16le", pDet prop_Word16le)
        , ("Word32be", pDet prop_Word32be)
        , ("Word32le", pDet prop_Word32le)
        , ("Word64be", pDet prop_Word64be)
        , ("Word64le", pDet prop_Word64le)
        , ("[Word8]",  pDet prop_List)
        , ("Maybe Word8", pDet prop_Maybe)
        , ("Either Word8 Word16", pDet prop_Either)
        , ("Char", pDet prop_Char)
        , ("String", pDet prop_String)
        ]
