{-# LANGUAGE BangPatterns #-}
module Action where

import Control.Applicative
import Test.QuickCheck

import qualified Data.ByteString.Lazy as L

import qualified Data.Binary.Get as Binary

import Arbitrary()

data Action
  = GetByteString Int
  | Try [Action] [Action]
  | LookAhead [Action]
  | BytesRead
  | Fail
  deriving (Show, Eq)

instance Arbitrary Action where
  shrink action =
    case action of
      GetByteString n -> [ GetByteString n' | n' <- shrink n, n > 0 ]
      BytesRead -> []
      Fail -> []
      LookAhead a -> a ++ [ LookAhead a' | a' <- shrink a ]
      Try a b ->
        [ Try a' b' | a' <- shrink a, b' <- shrink b ]
        ++ [ Try a' b | a' <- shrink a ]
        ++ [ Try a b' | b' <- shrink b ]

willFail :: [Action] -> Bool
willFail [] = False
willFail (x:xs) =
  case x of
    GetByteString _ -> willFail xs
    Try a b -> (willFail a && willFail b) || willFail xs
    LookAhead a -> willFail a || willFail xs
    BytesRead -> willFail xs
    Fail -> True

max_len :: [Action] -> Int
max_len [] = 0
max_len (x:xs) =
  case x of
    GetByteString n -> n + max_len xs
    BytesRead -> max_len xs
    Fail -> 0
    Try a b -> max (max_len a) (max_len b) + max_len xs
    LookAhead a -> max (max_len a) (max_len xs)

actual_len :: [Action] -> Maybe Int
actual_len = go 0
  where
  go !s [] = Just s
  go !s (x:xs) =
    case x of
      GetByteString n -> go (s+n) xs
      Fail -> Nothing
      BytesRead -> go s xs
      LookAhead _ -> go s xs
      Try a b | not (willFail a) -> liftA2 (+) (go s a) (actual_len xs)
              | not (willFail b) -> liftA2 (+) (go s b) (actual_len xs)
              | otherwise -> Nothing

-- | Build binary programs and compare running them to running a (hopefully)
-- identical model.
-- Tests that 'bytesRead' returns correct values when used together with '<|>'
-- and 'fail'.
prop_action :: Property
prop_action =
  forAllShrink gen_actions shrink $ \ actions ->
    forAll arbitrary $ \ lbs ->
      L.length lbs >= fromIntegral (max_len actions) ==>
        case Binary.runGet (eval actions) lbs of
          () -> True

eval :: [Action] -> Binary.Get ()
eval = go 0
  where
  go _ [] = return ()
  go pos (x:xs) =
    case x of
      GetByteString n ->
        Binary.getByteString n >> go (pos+n) xs
      BytesRead -> do
        pos' <- Binary.bytesRead
        if (pos == fromIntegral pos')
          then go pos xs
          else error $ "expected " ++ show pos ++ " but got " ++ show pos'
      Fail -> fail "fail"
      LookAhead a -> do
        _ <- Binary.lookAhead (go pos a)
        go pos xs
      Try a b -> do
        len <- leg pos a <|> leg pos b
        case len of
          Nothing -> error "got Nothing, but we're still here..."
          Just offset -> go (pos+offset) xs
  leg pos t = go pos t >> return (actual_len t)

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
                       , do t <- go inTry (s-1)
                            (:) (LookAhead t) <$> go inTry (s-1)
                       ] ++ [ return [Fail] | inTry ]
