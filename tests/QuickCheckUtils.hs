{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- Uses multi-param type classes
--
module QuickCheckUtils where

import Control.Monad

import Test.QuickCheck.Batch
import Test.QuickCheck
import Text.Show.Functions

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet


import Control.Monad        ( liftM2 )
import Data.Char
import Data.List
import Data.Word
import Data.Int
import System.Random
import System.IO

import Control.Concurrent
import System.Mem
import System.CPUTime
import Text.Printf

import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base as L (LazyByteString(..))

-- import qualified Data.Sequence as Seq

-- Enable this to get verbose test output. Including the actual tests.
debug = False

mytest :: Testable a => a -> Int -> IO ()
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO ()
mycheck config a = do
     rnd <- newStdGen
     performGC >> threadDelay 100
     time (mytests config (evaluate a) rnd 0 0 [])

time :: IO () -> IO ()
time a = do
    start <- getCPUTime
    a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf " %0.3f seconds\n" (diff :: Double)
    hFlush stdout

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
mytests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK," ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout

         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout

     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ". "
  display [x] = " (" ++ x ++ "). "
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word16 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int16 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int32 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

------------------------------------------------------------------------

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

------------------------------------------------------------------------

instance Arbitrary Word8 where
    arbitrary       = choose (0, 2^8-1)
    coarbitrary w   = variant 0

instance Arbitrary Word16 where
    arbitrary       = choose (0, 2^16-1)
    coarbitrary     = undefined

instance Arbitrary Word32 where
--  arbitrary       = choose (0, 2^32-1)
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Word64 where
--  arbitrary       = choose (0, 2^64-1)
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Int8 where
--  arbitrary       = choose (0, 2^8-1)
    arbitrary       = choose (minBound, maxBound)
    coarbitrary w   = variant 0

instance Arbitrary Int16 where
--  arbitrary       = choose (0, 2^16-1)
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Int32 where
--  arbitrary       = choose (0, 2^32-1)
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Int64 where
--  arbitrary       = choose (0, 2^64-1)
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Word where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary w   = variant 0

------------------------------------------------------------------------

instance Arbitrary Char where
    arbitrary = choose (maxBound, minBound)
    coarbitrary = undefined

instance Arbitrary a => Arbitrary (Maybe a) where
    arbitrary = oneof [ return Nothing, liftM Just arbitrary]
    coarbitrary = undefined

instance Arbitrary Ordering where
    arbitrary = oneof [ return LT,return  GT,return  EQ ]
    coarbitrary = undefined

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = oneof [ liftM Left arbitrary, liftM Right arbitrary]
    coarbitrary = undefined

instance Arbitrary IntSet.IntSet where
    arbitrary = fmap IntSet.fromList arbitrary
    coarbitrary = undefined

instance (Arbitrary e) => Arbitrary (IntMap.IntMap e) where
    arbitrary = fmap IntMap.fromList arbitrary
    coarbitrary = undefined

instance (Arbitrary a, Ord a) => Arbitrary (Set.Set a) where
    arbitrary = fmap Set.fromList arbitrary
    coarbitrary = undefined

instance (Arbitrary a, Ord a, Arbitrary b) => Arbitrary (Map.Map a b) where
    arbitrary = fmap Map.fromList arbitrary
    coarbitrary = undefined

{-
instance (Arbitrary a) => Arbitrary (Seq.Seq a) where
    arbitrary = fmap Seq.fromList arbitrary
    coarbitrary = undefined
-}

instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . B.LPS . filter (not. B.null) -- maintain the invariant.
    coarbitrary s = coarbitrary (L.unpack s)

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (B.unpack s)
