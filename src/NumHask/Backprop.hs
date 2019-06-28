{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NumHask.Backprop where

import NumHask.KTuple
import NumHask.Prelude as NH
import Numeric.Backprop as BP

-- | unwrapped BVar orphan instances for numhask
-- Fixme: Epsilon, FromInteger, ToInteger have problematic APIs (raw Bool, Integer's in types)
-- Fixme: not sure how to proceed with Normed, Metric
-- numhask classes
instance (Backprop a, Additive a, Reifies s W) => Additive (BVar s a) where
  (+) = liftOp2 . op2 $ \x y -> (x + y, \g -> (g, g))
  zero = constVar NH.zero

instance (Backprop a, Subtractive a, Reifies s W) =>
         Subtractive (BVar s a) where
  negate = liftOp1 . op1 $ \x -> (negate x, negate)

instance (Backprop a, Multiplicative a, Reifies s W) =>
         Multiplicative (BVar s a) where
  (*) = liftOp2 . op2 $ \x y -> (x * y, \g -> (y * g, x * g))
  one = constVar NH.one

instance (Backprop a, Reifies s W, Subtractive a, Divisive a) =>
         Divisive (BVar s a) where
  recip = liftOp1 . op1 $ \x -> (recip x, (/ (x * x)) . negate)

instance (Backprop a, Distributive a, Reifies s W) =>
         Distributive (BVar s a)

instance (Backprop a, CommutativeRing a, Reifies s W) =>
  CommutativeRing (BVar s a)

instance (Backprop a, Reifies s W, StarSemiring a) =>
         StarSemiring (BVar s a) where
  plus = liftOp1 . op1 $ \x -> (star x, plus)
  star = liftOp1 . op1 $ \x -> (plus x, (* star x))

instance (Backprop a, Magma a, Reifies s W) =>
         Magma (BVar s a) where
  magma = liftOp2 . op2 $ \x y -> (x `magma` y, \g -> (g, g))

instance (Backprop a, Idempotent a, Reifies s W) =>
         Idempotent (BVar s a)

instance (Backprop a, KleeneAlgebra a, Reifies s W) =>
         KleeneAlgebra (BVar s a)

instance (Backprop a, Reifies s W, InvolutiveRing a) =>
         InvolutiveRing (BVar s a) where
  adj = liftOp1 . op1 $ \x -> (adj x, adj)

instance (Backprop a, IntegralDomain a, Subtractive a, Reifies s W) =>
         IntegralDomain (BVar s a)

instance (Backprop a, Field a, Subtractive a, Reifies s W) => Field (BVar s a)

instance ( Backprop a
         , Reifies s W
         , Additive a
         , Subtractive a
         , ExpField a
         ) =>
         ExpField (BVar s a) where
  exp = liftOp1 . op1 $ \x -> (exp x, (exp x *))
  log = liftOp1 . op1 $ \x -> (log x, (/ x))

instance ( Backprop a
         , Reifies s W -- fixme: why was this needed here?
         , ExpField a
         , Subtractive a
         , TrigField a
         ) =>
         TrigField (BVar s a) where
  pi = constVar NH.pi
  sin = liftOp1 . op1 $ \x -> (sin x, (* cos x))
  cos = liftOp1 . op1 $ \x -> (cos x, (* negate (sin x)))
  asin = liftOp1 . op1 $ \x -> (asin x, (/ sqrt (NH.one - x * x)))
  acos = liftOp1 . op1 $ \x -> (acos x, (/ sqrt (NH.one - x * x)) . negate)
  atan = liftOp1 . op1 $ \x -> (atan x, (/ (x * x + NH.one)))
  sinh = liftOp1 . op1 $ \x -> (sinh x, (* cosh x))
  cosh = liftOp1 . op1 $ \x -> (cosh x, (* sinh x))
  asinh = liftOp1 . op1 $ \x -> (tanh x, (/ cosh x ** (NH.one + NH.one)))
  acosh = liftOp1 . op1 $ \x -> (acosh x, (/ sqrt (x * x - NH.one)))
  atanh = liftOp1 . op1 $ \x -> (atanh x, (/ (NH.one - x * x)))

divModOp ::
     (Subtractive a, Integral a) => Op '[ a, a] (KTuple a a)
divModOp =
  op2 $ \x y ->
    (let (d, m) = divMod x y
     in (KT d m, \(KT d' m') -> (d', m' - m' * d)))

quotRemOp ::
     (Subtractive a, Integral a) => Op '[ a, a] (KTuple a a)
quotRemOp =
  op2 $ \x y ->
    (let (q, r) = quotRem x y
     in (KT q r, \(KT q' r') -> (q', r' - r' * q)))

instance ( Backprop a
         , Integral a
         , UpperBoundedField a
         , FromInteger a
         , Subtractive a
         , Reifies s W
         ) =>
         Integral (BVar s a) where
  divMod a b =
    let (splitBV -> KT a' b') = liftOp2 divModOp a b
    in (a', b')
  quotRem a b =
    let (splitBV -> KT a' b') = liftOp2 quotRemOp a b
    in (a', b')

properFractionOp ::
     (Eq a, UpperBoundedField a, ToInteger b, FromInteger a, QuotientField a b)
  => Op '[ a] (KTuple b a)
properFractionOp =
  op1 $ \x ->
    (,)
      (let (i, r) = properFraction x
       in KT i r)
      (\(KT i r) ->
         if r == NH.zero
           then nan
           else NH.fromIntegral i)

instance ( Ord a
         , Ord b
         , Backprop a
         , Backprop b
         , Field a
         , Field b
         , UpperBoundedField b
         , UpperBoundedField a
         , FromInteger a
         , ToInteger b
         , FromInteger b
         , Subtractive b
         , Reifies s W
         , QuotientField a b
         ) =>
         QuotientField (BVar s a) (BVar s b) where
  properFraction = splitKT . liftOp1 properFractionOp

instance (Backprop a, Reifies s W, UpperBoundedField a, Subtractive a) =>
         UpperBoundedField (BVar s a) where
  -- isNaN = liftOp1 . op1 $ \x -> (isNaN x)

instance (Backprop a, Reifies s W, LowerBoundedField a) =>
         LowerBoundedField (BVar s a)

instance (Backprop a, Reifies s W, Additive a, Signed a) =>
         Signed (BVar s a) where
  sign = liftOp1 . op1 $ \x -> (sign x, const NH.zero)
  abs = liftOp1 . op1 $ \x -> (abs x, (* sign x))

-- | not possible to define ToInteger or FromInteger because of the raw Integer type in the API.
fromIntegralBVar ::
     ( Reifies s W
     , Backprop a
     , ToInteger a
     , FromInteger b
     , ToInteger b
     , FromInteger a
     )
  => BVar s a
  -> BVar s b
fromIntegralBVar =
  liftOp1 . op1 $ \x -> (,) (NH.fromIntegral x) (NH.fromIntegral . toInteger)

-- | not possible to define ToRatio or FromRatio because of the raw Ratio Integer type in the API.
fromRationalBVar ::
     ( Reifies s W
     , Backprop a
     , ToRatio a
     , FromRatio b
     , ToRatio b
     , FromRatio a
     )
  => BVar s a
  -> BVar s b
fromRationalBVar =
  liftOp1 . op1 $ \x -> (,) (NH.fromRational x) (NH.fromRational . toRatio)

instance Backprop (Ratio Integer) where
  zero _ = NH.zero
  one _ = NH.one
  add = (NH.+)
