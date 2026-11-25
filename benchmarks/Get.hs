{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import Control.DeepSeq
import Control.Exception (evaluate)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Fixed (Fixed, E2)
import Data.Functor.Identity (Identity(..))
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import Data.Ratio (Ratio, (%))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Bits
import Data.Char (ord)
import Data.List (foldl')
import Data.Word
import Numeric.Natural (Natural)
import Test.Tasty.Bench

import Control.Applicative
import Data.Binary
import Data.Binary.Get

import qualified Data.Serialize.Get as Cereal

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as AL

main :: IO ()
main = do
  evaluate $ rnf [
    rnf brackets,
    rnf bracketsInChunks,
    rnf bracketCount,
    rnf oneMegabyte,
    rnf oneMegabyteLBS,
    rnf manyBytes,
    rnf encodedBigInteger,
    rnf smallMsgs1Chunk,
    rnf smallMsgs4Chunks,
    rnf encodedListInt,
    rnf encodedListW32,
    rnf encodedListPair,
    rnf encodedListMaybe,
    rnf encodedSetInt,
    rnf encodedMapIntW32,
    rnf encodedIntSet,
    rnf encodedIntMapW32,
    rnf encodedSeqInt,
    rnf encodedSumInts,
    rnf encodedIdentityW32s,
    rnf encodedFixedE2s,
    rnf encodedSmallIntegers,
    rnf encodedSmallNaturals,
    rnf encodedEitherIntW32s,
    rnf encodedRatioInts
     ]
  defaultMain
    [ bgroup "brackets"
        [ bench "Binary 100kb, one chunk" $
            whnf (checkBracket . runGet bracketParser) brackets
        , bench "Binary 100kb, 100 byte chunks" $
            whnf (checkBracket . runGet bracketParser) bracketsInChunks
        , bench "Attoparsec lazy-bs 100kb, one chunk" $
            whnf (checkBracket . runAttoL bracketParser_atto) brackets
        , bench "Attoparsec lazy-bs 100kb, 100 byte chunks" $
            whnf (checkBracket . runAttoL bracketParser_atto) bracketsInChunks
        , bench "Attoparsec strict-bs 100kb" $
            whnf (checkBracket . runAtto bracketParser_atto) $ L.toStrict brackets
        , bench "Cereal lazy-bs 100kb, one chunk" $
            whnf (checkBracket . runCerealL bracketParser_cereal) brackets
        , bench "Cereal lazy-bs 100kb, 100 byte chunks" $
            whnf (checkBracket . runCerealL bracketParser_cereal) bracketsInChunks
        , bench "Cereal strict-bs 100kb" $
            whnf (checkBracket . runCereal bracketParser_cereal) $ L.toStrict brackets
        ]
    , bgroup "comparison getStruct4, 1MB of struct of 4 Word8s"
      [ bench "Attoparsec" $
          whnf (runAtto (getStruct4_atto mega)) oneMegabyte
      , bench "Binary" $
          whnf (runGet (getStruct4 mega)) oneMegabyteLBS
      , bench "Cereal" $
          whnf (runCereal (getStruct4_cereal mega)) oneMegabyte
      ]
    , bgroup "comparison getWord8, 1MB"
        [ bench "Attoparsec" $
            whnf (runAtto (getWord8N1_atto mega)) oneMegabyte
        , bench "Binary" $
            whnf (runGet (getWord8N1 mega)) oneMegabyteLBS
        , bench "Cereal" $
            whnf (runCereal (getWord8N1_cereal mega)) oneMegabyte
        ]
    , bgroup "getWord8 1MB"
        [ bench "chunk size 2 bytes" $
            whnf (runGet (getWord8N2 mega)) oneMegabyteLBS
        , bench "chunk size 4 bytes" $
            whnf (runGet (getWord8N4 mega)) oneMegabyteLBS
        , bench "chunk size 8 bytes" $
            whnf (runGet (getWord8N8 mega)) oneMegabyteLBS
        , bench "chunk size 16 bytes" $
            whnf (runGet (getWord8N16 mega)) oneMegabyteLBS
        ]
    , bgroup "getWord8 1MB Applicative"
        [ bench "chunk size 2 bytes" $
            whnf (runGet (getWord8N2A mega)) oneMegabyteLBS
        , bench "chunk size 4 bytes" $
            whnf (runGet (getWord8N4A mega)) oneMegabyteLBS
        , bench "chunk size 8 bytes" $
            whnf (runGet (getWord8N8A mega)) oneMegabyteLBS
        , bench "chunk size 16 bytes" $
            whnf (runGet (getWord8N16A mega)) oneMegabyteLBS
        ]
    , bgroup "roll"
        [ bench "foldr"  $ nf (roll_foldr  :: [Word8] -> Integer) manyBytes
        , bench "foldl'" $ nf (roll_foldl' :: [Word8] -> Integer) manyBytes
        ]
    , bgroup "Integer"
        [ bench "decode" $ nf (decode :: L.ByteString -> Integer) encodedBigInteger
        , bench "[small Integer] (10k)" $ nf (decode :: L.ByteString -> [Integer]) encodedSmallIntegers
        , bench "[small Natural] (10k)" $ nf (decode :: L.ByteString -> [Natural]) encodedSmallNaturals
        ]
    , bgroup "small messages (1000 x 16B)"
        [ bench "runGet, one chunk" $
            whnf (runSmallMany (runGet getSmallMsg)) smallMsgs1Chunk
        , bench "runGet, 4-byte chunks" $
            whnf (runSmallMany (runGet getSmallMsg)) smallMsgs4Chunks
        , bench "runGetOrFail, one chunk" $
            whnf (runSmallMany runSmallOrFail) smallMsgs1Chunk
        , bench "runGetOrFail, 4-byte chunks" $
            whnf (runSmallMany runSmallOrFail) smallMsgs4Chunks
        ]
    , bgroup "Containers (10k)"
        [ bench "[Int]"           $ nf (decode :: L.ByteString -> [Int]) encodedListInt
        , bench "[Word32]"        $ nf (decode :: L.ByteString -> [Word32]) encodedListW32
        , bench "[(Int, Word32)]" $ nf (decode :: L.ByteString -> [(Int, Word32)]) encodedListPair
        , bench "[Maybe Word32]"  $ nf (decode :: L.ByteString -> [Maybe Word32]) encodedListMaybe
        , bench "Set Int"         $ nf (decode :: L.ByteString -> Set.Set Int) encodedSetInt
        , bench "Map Int Word32"  $ nf (decode :: L.ByteString -> Map.Map Int Word32) encodedMapIntW32
        , bench "IntSet"          $ nf (decode :: L.ByteString -> IntSet.IntSet) encodedIntSet
        , bench "IntMap Word32"   $ nf (decode :: L.ByteString -> IntMap.IntMap Word32) encodedIntMapW32
        , bench "Seq Int"         $ nf (decode :: L.ByteString -> Seq.Seq Int) encodedSeqInt
        ]
    , bgroup "Wrappers (10k)"
        [ bench "[Sum Int]"         $ nf (decode :: L.ByteString -> [Monoid.Sum Int]) encodedSumInts
        , bench "[Identity Word32]" $ nf (decode :: L.ByteString -> [Identity Word32]) encodedIdentityW32s
        , bench "[Fixed E2]"        $ nf (decode :: L.ByteString -> [Fixed E2]) encodedFixedE2s
        , bench "[Either Int Word32]" $ nf (decode :: L.ByteString -> [Either Int Word32]) encodedEitherIntW32s
        , bench "[Ratio Int]"       $ nf (decode :: L.ByteString -> [Ratio Int]) encodedRatioInts
        ]
    ]

checkBracket :: Int -> Int
checkBracket x | x == bracketCount = x
               | otherwise = error "argh!"

runCereal :: Cereal.Get a -> S.ByteString -> a
runCereal decoder inp = case Cereal.runGet decoder inp of
                          Right a -> a
                          Left err -> error err

runCerealL :: Cereal.Get a -> L.ByteString -> a
runCerealL decoder inp = case Cereal.runGetLazy decoder inp of
                          Right a -> a
                          Left err -> error err

runAtto :: AL.Parser a -> S.ByteString -> a
runAtto decoder inp = case A.parseOnly decoder inp of
                        Right a -> a
                        Left err -> error err

runAttoL :: AL.Parser a -> L.ByteString -> a
runAttoL decoder inp = case AL.parseOnly decoder inp of
                        Right a -> a
                        Left err -> error err

-- Defs.

oneMegabyte :: S.ByteString
oneMegabyte = S.replicate mega $ fromIntegral $ ord 'a'

oneMegabyteLBS :: L.ByteString
oneMegabyteLBS = L.fromChunks [oneMegabyte]

smallMsgs1Chunk :: [L.ByteString]
smallMsgs1Chunk =
  [ L.fromChunks [S.pack [fromIntegral (i + j) | j <- [0..15 :: Int]]]
  | i <- [0..999 :: Int]
  ]

smallMsgs4Chunks :: [L.ByteString]
smallMsgs4Chunks =
  [ L.fromChunks
      [ S.pack [fromIntegral (i + j) | j <- [0..3 :: Int]]
      , S.pack [fromIntegral (i + j) | j <- [4..7 :: Int]]
      , S.pack [fromIntegral (i + j) | j <- [8..11 :: Int]]
      , S.pack [fromIntegral (i + j) | j <- [12..15 :: Int]]
      ]
  | i <- [0..999 :: Int]
  ]

getSmallMsg :: Get Word32
getSmallMsg = do
  a <- getWord32be
  b <- getWord32be
  c <- getWord32be
  d <- getWord32be
  return $! a + b + c + d

runSmallOrFail :: L.ByteString -> Word32
runSmallOrFail lbs =
  case runGetOrFail getSmallMsg lbs of
    Right (_, _, x) -> x
    Left (_, _, e)  -> error e

runSmallMany :: (L.ByteString -> Word32) -> [L.ByteString] -> Word32
runSmallMany f = foldl' (\acc lbs -> acc + f lbs) 0

mega :: Int
mega = 1024 * 1024

bracketCount :: Int
bracketCount = fromIntegral $ L.length brackets `div` 2

brackets :: L.ByteString
brackets = L.fromChunks [L.toStrict bracketsInChunks]

bracketsInChunks :: L.ByteString
bracketsInChunks = L.fromChunks (replicate chunksOfBrackets oneChunk)
  where
    oneChunk = "((()((()()))((()(()()()()()()()(((()()()()(()()(()(()())))))()((())())))()())(((())())(()))))()(()))"
    chunksOfBrackets = 102400 `div` S.length oneChunk

bracketParser :: Get Int
bracketParser = cont <|> return 0
  where
    cont = do
      v <- some $ do
        40 <- getWord8 -- '('
        n <- many cont
        41 <- getWord8 -- ')'
        return $! sum n + 1
      return $! sum v

bracketParser_cereal :: Cereal.Get Int
bracketParser_cereal = cont <|> return 0
  where
    cont = do
      v <- some $ do
        40 <- Cereal.getWord8 -- '('
        n <- many cont
        41 <- Cereal.getWord8 -- ')'
        return $! sum n + 1
      return $! sum v

bracketParser_atto :: A.Parser Int
bracketParser_atto = cont <|> return 0
  where
    cont = do
      v <- some $ do
        _ <- A.word8 40 -- '('
        n <- A.many' cont
        _ <- A.word8 41 -- ')'
        return $! sum n + 1
      return $! sum v

data S2 = S2 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
data S4 = S4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
data S8 = S8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
             {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
data S16 = S16 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
               {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

getStruct4 :: Int -> Get [S4]
getStruct4 = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- getWord8
          !w1 <- getWord8
          !w2 <- getWord8
          !w3 <- getWord8
          let !s = S4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getStruct4_cereal :: Int -> Cereal.Get [S4]
getStruct4_cereal = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- Cereal.getWord8
          !w1 <- Cereal.getWord8
          !w2 <- Cereal.getWord8
          !w3 <- Cereal.getWord8
          let !s = S4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getStruct4_atto :: Int -> A.Parser [S4]
getStruct4_atto = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- A.anyWord8
          !w1 <- A.anyWord8
          !w2 <- A.anyWord8
          !w3 <- A.anyWord8
          let !s = S4 w0 w1 w2 w3
          loop (s : acc) (n - 4)

getWord8N1 :: Int -> Get [Word8]
getWord8N1 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          loop (s0:s) (n-1)

getWord8N1_cereal :: Int -> Cereal.Get [Word8]
getWord8N1_cereal = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- Cereal.getWord8
          loop (s0:s) (n-1)

getWord8N1_atto :: Int -> A.Parser [Word8]
getWord8N1_atto = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- A.anyWord8
          loop (s0:s) (n-1)

getWord8N2 :: Int -> Get [S2]
getWord8N2 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          let !v = S2 s0 s1
          loop (v:s) (n-2)

getWord8N2A :: Int -> Get [S2]
getWord8N2A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S2 <$> getWord8 <*> getWord8
          loop (v:s) (n-2)

getWord8N4 :: Int -> Get [S4]
getWord8N4 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          let !v = S4 s0 s1 s2 s3
          loop (v:s) (n-4)

getWord8N4A :: Int -> Get [S4]
getWord8N4A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S4 <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
          loop (v:s) (n-4)

getWord8N8 :: Int -> Get [S8]
getWord8N8 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          let !v = S8 s0 s1 s2 s3 s4 s5 s6 s7
          loop (v:s) (n-8)

getWord8N8A :: Int -> Get [S8]
getWord8N8A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S8 <$> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
          loop (v:s) (n-8)

getWord8N16 :: Int -> Get [S16]
getWord8N16 = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          s8 <- getWord8
          s9 <- getWord8
          s10 <- getWord8
          s11 <- getWord8
          s12 <- getWord8
          s13 <- getWord8
          s14 <- getWord8
          s15 <- getWord8
          let !v = S16 s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15
          loop (v:s) (n-16)

getWord8N16A :: Int -> Get [S16]
getWord8N16A = loop []
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          !v <- S16 <$> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
                    <*> getWord8
          loop (v:s) (n-16)

manyBytes :: [Word8]
manyBytes = concat $ replicate 256 [0..255]

bigInteger :: Integer
bigInteger = roll_foldl' manyBytes

encodedBigInteger :: L.ByteString
encodedBigInteger = encode bigInteger

roll_foldr :: (Integral a, Bits a) => [Word8] -> a
roll_foldr   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

roll_foldl' :: (Integral a, Bits a) => [Word8] -> a
roll_foldl'   = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

containerSize :: Int
containerSize = 10000

encodedListInt :: L.ByteString
encodedListInt = L.fromStrict . L.toStrict $ encode [1..containerSize]

encodedListW32 :: L.ByteString
encodedListW32 = L.fromStrict . L.toStrict $ encode [1..fromIntegral containerSize :: Word32]

encodedListPair :: L.ByteString
encodedListPair = L.fromStrict . L.toStrict $ encode [(i, fromIntegral i :: Word32) | i <- [1..containerSize]]

encodedListMaybe :: L.ByteString
encodedListMaybe = L.fromStrict . L.toStrict $ encode [if even i then Just (fromIntegral i :: Word32) else Nothing | i <- [1..containerSize]]

encodedSetInt :: L.ByteString
encodedSetInt = L.fromStrict . L.toStrict $ encode (Set.fromList [1..containerSize])

encodedMapIntW32 :: L.ByteString
encodedMapIntW32 = L.fromStrict . L.toStrict $ encode (Map.fromList [(i, fromIntegral i :: Word32) | i <- [1..containerSize]])

encodedIntSet :: L.ByteString
encodedIntSet = L.fromStrict . L.toStrict $ encode (IntSet.fromList [1..containerSize])

encodedIntMapW32 :: L.ByteString
encodedIntMapW32 = L.fromStrict . L.toStrict $ encode (IntMap.fromList [(i, fromIntegral i :: Word32) | i <- [1..containerSize]])

encodedSeqInt :: L.ByteString
encodedSeqInt = L.fromStrict . L.toStrict $ encode (Seq.fromList [1..containerSize])

encodedSumInts :: L.ByteString
encodedSumInts = L.fromStrict . L.toStrict $ encode [Monoid.Sum i | i <- [1..containerSize]]

encodedIdentityW32s :: L.ByteString
encodedIdentityW32s = L.fromStrict . L.toStrict $ encode [Identity (fromIntegral i :: Word32) | i <- [1..containerSize]]

encodedFixedE2s :: L.ByteString
encodedFixedE2s = L.fromStrict . L.toStrict $ encode [fromIntegral i :: Fixed E2 | i <- [1..containerSize]]

encodedSmallIntegers :: L.ByteString
encodedSmallIntegers = L.fromStrict . L.toStrict $ encode [0..fromIntegral containerSize :: Integer]

encodedSmallNaturals :: L.ByteString
encodedSmallNaturals = L.fromStrict . L.toStrict $ encode [0..fromIntegral containerSize :: Natural]

encodedEitherIntW32s :: L.ByteString
encodedEitherIntW32s = L.fromStrict . L.toStrict $ encode [if even i then Left i else Right (fromIntegral i :: Word32) | i <- [1..containerSize]]

encodedRatioInts :: L.ByteString
encodedRatioInts = L.fromStrict . L.toStrict $ encode [i % (i + 1) | i <- [1..containerSize]]
