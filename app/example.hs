{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

import NumHask.Backprop ()
import NumHask.KTuple
import NumHask.Prelude
import Numeric.Backprop hiding (zero,one)
import System.Random

type Model p a b
   = forall z. (Reifies z W) =>
                 BVar z p -> BVar z a -> BVar z b

linReg :: Model (KTuple Double Double) Double Double
linReg p x = b * x + a
  where
    (a, b) = splitKT p

squaredErrorGrad ::
     (Backprop p, Backprop b, Multiplicative b, AdditiveGroup b)
  => Model p a b -- ^ Model
  -> a -- ^ Observed input
  -> b -- ^ Observed output
  -> p -- ^ Parameter guess
  -> p -- ^ Gradient
squaredErrorGrad model x targ =
  gradBP $ \p -> (model p (auto x) - auto targ) ^ (2 :: Integer)

trainModel ::
     ( Backprop p
     , FromRatio p
     , Multiplicative p
     , AdditiveGroup p
     , Multiplicative b
     , AdditiveGroup b
     , Backprop b
     )
  => Model p a b -- ^ model to train
  -> p -- ^ initial parameter guess
  -> [(a, b)] -- ^ list of observations
  -> p -- ^ updated parameter guess
trainModel f = foldl' $ \p (x, y) -> p - 0.1 * squaredErrorGrad f x y p

trainModelIO ::
     ( Multiplicative p
     , AdditiveGroup p
     , FromRatio p
     , Backprop p
     , Multiplicative b
     , AdditiveGroup b
     , Backprop b
     , Random p
     )
  => Model p a b -- ^ model to train
  -> [(a, b)] -- ^ list of observations
  -> IO p -- ^ parameter guess
trainModelIO m xs = do
  p0 <- randomIO
  return $ trainModel m p0 xs

testTrainLinReg :: IO (KTuple Double Double)
testTrainLinReg = trainModelIO linReg (concat (replicate 1000 samps))
  where
    samps = [(1, 1), (2, 3), (3, 5), (4, 7), (5, 9)]

main :: IO ()
main = do
  (KT alpha beta) <- testTrainLinReg
  putStrLn ("alpha: " <> show alpha <> " beta: " <> show beta <> " 👍" :: Text)
  writeFile
    "other/answer.md"
    ("alpha: " <> show alpha <> " beta: " <> show beta)

-- | examples from intro
--
-- >>> gradBP (\x -> x^2 + 3) (9 :: Double)
-- 18.0
--
-- >>> gradBP2 (\x xs -> sum (map (**2) (sequenceVar xs)) / x) (9::Double) ([1,6,2]::[Double])
-- (-0.5061728395061729,[0.2222222222222222,1.3333333333333333,0.4444444444444444])


