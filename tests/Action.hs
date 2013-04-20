{-# LANGUAGE BangPatterns #-}
module Action where

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.Binary.Get as Binary

import Arbitrary()

data Action
  = Actions [Action]
  | GetByteString Int
  | Try [Action] [Action]
  | LookAhead [Action]
  -- | First argument is True if this action returns Just, otherwise False.
  | LookAheadM Bool [Action]
  -- | First argument is True if this action returns Right, otherwise Left.
  | LookAheadE Bool [Action]
  | BytesRead
  | Fail
  deriving (Show, Eq)

instance Arbitrary Action where
  shrink action =
    case action of
      Actions [a] -> [a]
      Actions as -> [ Actions as' | as' <- shrink as ]
      GetByteString n -> [ GetByteString n' | n' <- shrink n, n >= 0 ]
      BytesRead -> []
      Fail -> []
      LookAhead a -> Actions a : [ LookAhead a' | a' <- shrink a ]
      LookAheadM b a -> Actions a : [ LookAheadM b a' | a' <- shrink a]
      LookAheadE b a -> Actions a : [ LookAheadE b a' | a' <- shrink a]
      Try [Fail] b -> Actions b : [ Try [Fail] b' | b' <- shrink b ]
      Try a b ->
        (if not (willFail a) then [Actions a] else [])
        ++ [ Try a' b' | a' <- shrink a, b' <- shrink b ]
        ++ [ Try a' b | a' <- shrink a ]
        ++ [ Try a b' | b' <- shrink b ]

willFail :: [Action] -> Bool
willFail [] = False
willFail (x:xs) =
  case x of
    Actions x' -> willFail x' || willFail xs
    GetByteString _ -> willFail xs
    Try a b -> (willFail a && willFail b) || willFail xs
    LookAhead a -> willFail a || willFail xs
    LookAheadM _ a -> willFail a || willFail xs
    LookAheadE _ a -> willFail a || willFail xs
    BytesRead -> willFail xs
    Fail -> True

-- | The maximum length of input decoder can request.
-- The decoder may end up using less, but never more.
-- This way, you know how much input to generate for running a decoder test.
max_len :: [Action] -> Int
max_len [] = 0
max_len (x:xs) =
  case x of
    Actions x' -> max_len x' + max_len xs
    GetByteString n -> n + max_len xs
    BytesRead -> max_len xs
    Fail -> 0
    Try a b -> max (max_len a) (max_len b) + max_len xs
    LookAhead a -> max (max_len a) (max_len xs)
    LookAheadM b a | willFail a -> max_len a
                   | b -> max_len a + max_len xs
                   | otherwise -> max (max_len a) (max_len xs)
    LookAheadE b a | willFail a -> max_len a
                   | b -> max_len a + max_len xs
                   | otherwise -> max (max_len a) (max_len xs)

-- | The actual length of input that will be consumed when
-- a decoder is executed, or Nothing if the decoder will fail.
actual_len :: [Action] -> Maybe Int
actual_len [] = Just 0
actual_len (x:xs) =
  case x of
    Actions x' -> (+) <$> actual_len x' <*> rest
    GetByteString n -> (n+) <$> rest
    Fail -> Nothing
    BytesRead -> rest
    LookAhead a | willFail a -> Nothing
                | otherwise -> rest
    LookAheadM b a | willFail a -> Nothing
                   | b -> (+) <$> actual_len a <*> rest
                   | otherwise -> rest
    LookAheadE b a | willFail a -> Nothing
                   | b -> (+) <$> actual_len a <*> rest
                   | otherwise -> rest
    Try a b | not (willFail a) -> (+) <$> actual_len a <*> rest
            | not (willFail b) -> (+) <$> actual_len b <*> rest
            | otherwise -> Nothing

  where
    rest = actual_len xs

-- | Build binary programs and compare running them to running a (hopefully)
-- identical model.
-- Tests that 'bytesRead' returns correct values when used together with '<|>'
-- and 'fail'.
prop_action :: Property
prop_action =
  forAllShrink gen_actions shrink $ \ actions ->
    forAll arbitrary $ \ lbs ->
      L.length lbs >= fromIntegral (max_len actions) ==>
        let allInput = B.concat (L.toChunks lbs) in
        case Binary.runGet (eval allInput actions) lbs of
          () -> True

-- | Evaluate (run) the model.
-- First argument is all the input that will be used when executing
-- this decoder. It is used in this function to compare the expected
-- value with the actual value from the decoder functions.
-- The second argument is the model - the actions we will evaluate.
eval :: B.ByteString -> [Action] -> Binary.Get ()
eval str = go 0
  where
  go _ [] = return ()
  go pos (x:xs) =
    case x of
      Actions a -> go pos (a++xs)
      GetByteString n -> do
        -- Run the operation in the Get monad...
        actual <- Binary.getByteString n
        let expected = B.take n . B.drop pos $ str
        -- ... and compare that we got what we expected.
        when (actual /= expected) $ error "actual /= expected"
        go (pos+n) xs
      BytesRead -> do
        pos' <- Binary.bytesRead
        if (pos == fromIntegral pos')
          then go pos xs
          else error $ "expected " ++ show pos ++ " but got " ++ show pos'
      Fail -> fail "fail"
      LookAhead a -> do
        _ <- Binary.lookAhead (go pos a)
        go pos xs
      LookAheadM b a -> do
        let f True = Just <$> leg pos a
            f False = go pos a >> return Nothing
        len <- Binary.lookAheadM (f b)
        case len of
          Nothing -> go pos xs
          Just offset -> go (pos+offset) xs
      LookAheadE b a -> do
        let f True = Right <$> leg pos a
            f False = go pos a >> return (Left ())
        len <- Binary.lookAheadE (f b)
        case len of
          Left _ -> go pos xs
          Right offset -> go (pos+offset) xs
      Try a b -> do
        offset <- leg pos a <|> leg pos b
        go (pos+offset) xs
  leg pos t = do
    go pos t
    case actual_len t of
      Nothing -> error "impossible: branch should have failed"
      Just offset -> return offset

gen_actions :: Gen [Action]
gen_actions = sized (go False)
  where
  go :: Bool -> Int -> Gen [Action]
  go     _ 0 = return []
  go inTry s = oneof $ [ do n <- choose (0,10)
                            (:) (GetByteString n) <$> go inTry (s-1)
                       , do (:) BytesRead <$> go inTry (s-1)
                       , do t1 <- go True (s `div` 2)
                            t2 <- go inTry (s `div` 2)
                            (:) (Try t1 t2) <$> go inTry (s `div` 2)
                       , do t <- go inTry (s`div`2)
                            (:) (LookAhead t) <$> go inTry (s-1)
                       , do t <- go inTry (s`div`2)
                            b <- arbitrary
                            (:) (LookAheadM b t) <$> go inTry (s-1)
                       , do t <- go inTry (s`div`2)
                            b <- arbitrary
                            (:) (LookAheadE b t) <$> go inTry (s-1)
                       ] ++ [ return [Fail] | inTry ]