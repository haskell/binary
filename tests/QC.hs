module Main where

import Data.Binary

import Control.Monad
import Foreign
import System.Environment
import Test.QuickCheck.Parallel

encdec x = x == runDecM get (runEncM (put x))
encdec' x fp fg = x == runDecM fg (runEncM (fp x))

instance Arbitrary Word8 where
    arbitrary = liftM fromIntegral (choose (0, 2^8-1))
    coarbitrary w = variant 0

instance Arbitrary Word16 where
    arbitrary = liftM fromIntegral (choose (0, 2^16-1))

instance Arbitrary Word32 where
    arbitrary = liftM fromIntegral (choose (0, 2^32-1))

instance Arbitrary Word64 where
    arbitrary = liftM fromIntegral (choose (0, 2^64-1))

instance Arbitrary a => Arbitrary (Maybe a) where
    arbitrary = oneof [ return Nothing, liftM Just arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = oneof [ liftM Left arbitrary, liftM Right arbitrary]

prop_Word8  (w :: Word8 ) = encdec w
prop_Word16 (w :: Word16) = encdec w
prop_Word32 (w :: Word32) = encdec w
prop_Word64 (w :: Word64) = encdec w

prop_Word16be w = encdec' w putWord16be getWord16be
prop_Word16le w = encdec' w putWord16le getWord16le

prop_Word32be w = encdec' w putWord32be getWord32be
prop_Word32le w = encdec' w putWord32le getWord32le

prop_Word64be w = encdec' w putWord64be getWord64be
prop_Word64le w = encdec' w putWord64le getWord64le

prop_list (xs :: [Word8]) = encdec xs
prop_maybe (ma :: Maybe Word8) = encdec ma
prop_either (eab :: Either Word8 Word16) = encdec eab


main = do
    n <- getArgs >>= readIO . head
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
        , ("[Word8]",  pDet prop_list)
        , ("Maybe Word8", pDet prop_maybe)
        , ("Either Word8 Word16", pDet prop_either)
        ]
