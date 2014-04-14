{-# LANGUAGE PatternGuards #-}
module Action ( tests ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as L
import           Data.Char
import           Data.List                            (intersperse, nub)

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import           Arbitrary                            ()
import qualified Data.Binary.Get                      as Binary

tests :: [Test]
tests = [ testProperty "action" prop_action
        , testProperty "label" prop_label
        , testProperty "fail" prop_fail ]

data Action
  = Actions [Action]
  | GetByteString Int
  | Isolate Int [Action]
  | Try [Action] [Action]
  | Label String [Action]
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
      BytesRead -> []
      Fail -> []
      GetByteString n -> [ GetByteString n' | n' <- shrink n, n >= 0 ]
      Isolate n0 as -> nub $
        let ns as' = filter (>=0) $ (n0 - 1) : [ 0 .. max_len as' + 1 ]
        in Actions as : [ Isolate n' as'
                        | as' <- [] : shrink as
                        , n' <- ns as' ]
      Label str a -> Actions a : [ Label str a' | a' <- [] : shrink a, a /= []]
      LookAhead a -> Actions a : [ LookAhead a' | a' <- [] : shrink a, a /= []]
      LookAheadM b a -> Actions a : [ LookAheadM b a' | a' <- [] : shrink a, a /= []]
      LookAheadE b a -> Actions a : [ LookAheadE b a' | a' <- [] : shrink a, a /= []]
      Try [Fail] b -> Actions b : [ Try [Fail] b' | b' <- [] : shrink b ]
      Try a b ->
        [Actions a | not (willFail' a)]
        ++ [ Try a' b' | a' <- [] : shrink a, b' <- [] : shrink b ]
        ++ [ Try a' b | a' <- [] : shrink a ]
        ++ [ Try a b' | b' <- [] : shrink b ]

willFail :: Int -> [Action] -> Bool
willFail inp xxs =
  case eval inp xxs of
    EFail _ _ -> True
    _ -> False

willFail' :: [Action] -> Bool
willFail' = willFail maxBound

-- | The maximum length of input decoder can request.
-- The decoder may end up using less, but never more.
-- This way, you know how much input to generate for running a decoder test.
max_len :: [Action] -> Int
max_len [] = 0
max_len (x:xs) =
  case x of
    Actions xs' -> max_len (xs' ++ xs)
    BytesRead -> max_len xs
    Fail -> 0
    GetByteString n -> n + max_len xs
    Isolate n xs'
      | Just n' <- actual_len' [Isolate n xs'], n == n' -> n + max_len xs
      | otherwise -> n
    Label _ xs' -> max_len (xs' ++ xs)
    LookAhead xs'
      | willFail' xs' -> max_len xs'
      | otherwise -> max (max_len xs') (max_len xs)
    LookAheadM consume xs'
      | consume -> max_len (xs' ++ xs)
      | otherwise -> max_len (LookAhead xs' : xs)
    LookAheadE consume xs'
      | consume -> max_len (xs' ++ xs)
      | otherwise -> max_len (LookAhead xs' : xs)
    Try a b
      | willFail' a && willFail' b -> max (max_len a) (max_len b)
      | willFail' a -> max (max_len a) (max_len b) + max_len xs
      | otherwise ->  max_len (a ++ xs)

-- | The actual length of input that will be consumed when
-- a decoder is executed, or Nothing if the decoder will fail.
actual_len :: Int -> [Action] -> Maybe Int
actual_len inp xs =
  case eval inp xs of
    ESuccess inp' -> Just (inp - inp')
    _ -> Nothing

actual_len' :: [Action] -> Maybe Int
actual_len' = actual_len maxBound

randomInput :: Int -> Gen L.ByteString
randomInput 0 = return L.empty
randomInput n = do
  m <- choose (1, min n 10)
  s <- vectorOf m $ choose ('a', 'z')
  let b = B.pack $ map (fromIntegral.ord) s
  rest <- randomInput (n-m)
  return (L.append (L.fromStrict b) rest)

-- | Build binary programs and compare running them to running a (hopefully)
-- identical model.
-- Tests that 'bytesRead' returns correct values when used together with '<|>'
-- and 'fail'.
prop_action :: Property
prop_action =
  forAllShrink (gen_actions False) shrink $ \ actions ->
    let max_len_input = max_len actions in
    forAll (randomInput max_len_input) $ \ lbs ->
      let allInput = B.concat (L.toChunks lbs) in
      case Binary.runGetOrFail (execute allInput actions) lbs of
        Right (_inp, _off, _x) -> True
        Left (_inp, _off, _msg) -> True

-- | When a decoder aborts with 'fail', check that all relevant uses of 'label'
-- are respected.
prop_label :: Property
prop_label =
  forAllShrink (gen_actions True) shrink $ \ actions ->
    let max_len_input = max_len actions in
    forAll (randomInput max_len_input) $ \ lbs ->
      let allInput = B.concat (L.toChunks lbs) in
      case Binary.runGetOrFail (execute allInput actions) lbs of
        Left (_inp, _off, msg) ->
          let lbls = case collectLabels max_len_input actions of
                         Just lbls' -> lbls'
                         Nothing -> error $ "expected labels, got: " ++ msg
              expectedMsg = concat $ intersperse "\n" lbls
          in if msg == expectedMsg
               then label ("labels: " ++ show (length lbls)) True
               else error (show msg ++ " vs. " ++ show expectedMsg)
        Right (_inp, _off, _value) -> label "test case without 'fail'" True

-- | When a decoder aborts with 'fail', check the fail position and
-- remaining input.
prop_fail :: Property
prop_fail =
  forAllShrink (gen_actions True) shrink $ \ actions ->
    let max_len_input = max_len actions in
    forAll (randomInput max_len_input) $ \ lbs ->
      let allInput = B.concat (L.toChunks lbs) in
      case Binary.runGetOrFail (execute allInput actions) lbs of
        Left (inp, off, _msg) ->
          case () of
            _ | Just off /= findFailPosition max_len_input actions ->
                  error ("fail position incorrect, expected " ++
                         show (findFailPosition max_len_input actions) ++
                         " but got " ++ show off)
              | inp /= L.drop (fromIntegral off) lbs ->
                  error $ "remaining output incorrect, was: " ++ show inp ++
                    ", should hav been: " ++ show (L.drop (fromIntegral off) lbs)
              | otherwise -> property True
        Right (_inp, _off, _value) -> label "test case without 'fail'" True

-- | Collect all the labels up to a 'fail', or Nothing if the
-- decoder will not fail.
collectLabels :: Int -> [Action] -> Maybe [String]
collectLabels inp xxs =
  case eval inp xxs of
    EFail lbls _ -> Just lbls
    _ -> Nothing

-- | Finds at which byte offset the decoder will fail,
-- or Nothing if it won't fail.
findFailPosition :: Int -> [Action] -> Maybe Binary.ByteOffset
findFailPosition inp xxs =
  case eval inp xxs of
    EFail _ inp' -> return (fromIntegral (inp-inp'))
    _ -> Nothing

-- | The result of an evaluation.
data Eval = ESuccess Int
          -- ^ The evalutation completed successfully. Contains the number of
          -- remaining bytes of the input.
          | EFail [String] Int
          -- ^ The evaluation completed with a failure. Contains the labels up
          -- to the failure, and the number of remaining bytes of the input.
          deriving (Show,Eq)

-- | Given the number of input bytes and a list of actions, evaluate the
-- actions and return whether the actions succeeed or fail.
eval :: Int -> [Action] -> Eval
eval inp0 = go inp0 []
  where
    step :: Int -> Int -> [String] -> [Action] -> Eval
    step inp n lbls xs
      | inp - n < 0 = EFail lbls inp
      | otherwise = go (inp-n) lbls xs
    go :: Int -> [String] -> [Action] -> Eval
    go inp _lbls [] = ESuccess inp
    go inp lbls (x:xs) =
      case x of
        Actions xs' -> go inp lbls (xs'++xs)
        BytesRead -> go inp lbls xs
        Fail -> EFail ("fail":lbls) inp
        GetByteString n -> step inp n lbls xs
        Isolate n xs'
          | n > inp ->
              case go inp lbls xs' of
                ESuccess inp' -> EFail lbls inp'
                efail -> efail
          | otherwise ->
              case go n lbls xs' of
                EFail lbls' inp' -> EFail lbls' (inp - n + inp')
                ESuccess 0       -> go (inp-n) lbls xs
                ESuccess inp'      -> EFail lbls (inp - n + inp')
        Label str xs'
          | EFail lbls' inp' <- go inp (str:lbls) xs' -> EFail lbls' inp'
          | otherwise -> go inp lbls (xs'++xs)
        LookAhead xs'
          | EFail lbls' inp' <- go inp lbls xs' -> EFail lbls' inp'
          | otherwise -> go inp lbls xs
        LookAheadM consume xs'
          | consume -> go inp lbls (xs'++xs)
          | otherwise -> go inp lbls (LookAhead xs' : xs)
        LookAheadE consume xs'
          | consume -> go inp lbls (xs'++xs)
          | otherwise -> go inp lbls (LookAhead xs' : xs)
        Try a b ->
          case go inp lbls a of
            ESuccess inp' -> go inp' lbls     xs
            EFail _ _     -> go inp  lbls (b++xs)
 
-- | Execute (run) the model.
-- First argument is all the input that will be used when executing
-- this decoder. It is used in this function to compare the expected
-- value with the actual value from the decoder functions.
-- The second argument is the model - the actions we will execute.
execute :: B.ByteString -> [Action] -> Binary.Get ()
execute inp acts0 = go 0 acts0 >> return ()
  where
  inp_len = B.length inp
  go _ [] = return ()
  go pos (x:xs) =
    case x of
      Actions a -> go pos (a++xs)
      GetByteString n -> do
        -- Run the operation in the Get monad...
        actual <- Binary.getByteString n
        let expected = B.take n . B.drop pos $ inp
        -- ... and compare that we got what we expected.
        when (actual /= expected) $ error $
          "execute(getByteString): actual /= expected at pos " ++ show pos ++
          ", got: " ++ show actual ++ ", expected: " ++ show expected
        go (pos+n) xs
      BytesRead -> do
        pos' <- Binary.bytesRead
        if pos == fromIntegral pos'
          then go pos xs
          else error $ "execute(bytesRead): expected " ++
            show pos ++ " but got " ++ show pos'
      Fail -> fail "fail"
      Isolate n as -> do
        let str = B.take n (B.drop pos inp)
        _ <- Binary.isolate n (execute str as)
        when (willFail (inp_len - pos) [Isolate n as]) $
          error "expected isolate to fail"
        go (pos + n) xs
      Label str as -> do
        len <- Binary.label str (leg pos as)
        go (pos+len) xs
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
    case actual_len (inp_len - pos) t of
      Nothing -> error "impossible: branch should have failed"
      Just offset -> return offset

gen_actions :: Bool -> Gen [Action]
gen_actions genFail = do
  acts <- sized (go False)
  return (if genFail then acts ++ [Fail] else acts)
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
                       , do t <- go inTry (s`div`2)
                            Positive n <- arbitrary :: Gen (Positive Int)
                            (:) (Label ("some label: " ++ show n) t) <$> go inTry (s-1)
                       , do t <- go inTry (s`div`2)
                            let (n,t') | Just n' <- actual_len' t, n' == max_len t = (n',t)
                                       | Just n' <- actual_len' t = (max_len t, t ++ [GetByteString (max_len t - n')])
                                       | otherwise = (max_len t, t)
                            (:) (Isolate n t') <$> go inTry (s-1)
                       ] ++ [ return [Fail] | inTry || genFail ]
