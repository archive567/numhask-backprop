{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Backprop instances for NumHask classes.
module NumHask.Backprop ( ) where

import NumHask.KTuple (KTuple, KTupleF (..), splitKT)
import NumHask.Prelude as NH
import Numeric.Backprop as BP

-- | unwrapped BVar orphan instances for numhask

-- Magma and Idempotent needed for KleeneAlgebra
instance
  (Backprop a, Reifies s W, Magma a) =>
  Magma (BVar s a)
  where
  (⊕) = liftOp2 . op2 $ \x y -> (x ⊕ y, \g -> (g, g))

instance
  (Backprop a, Reifies s W, Idempotent a) =>
  Idempotent (BVar s a)

-- numhask classes
instance (Backprop a, Reifies s W, Additive a) => Additive (BVar s a) where
  (+) = liftOp2 . op2 $ \x y -> (x + y, \g -> (g, g))
  zero = constVar NH.zero

instance
  (Backprop a, Reifies s W, Subtractive a) =>
  Subtractive (BVar s a)
  where
  negate = liftOp1 . op1 $ \x -> (negate x, negate)

instance
  (Backprop a, Reifies s W, Multiplicative a) =>
  Multiplicative (BVar s a)
  where
  (*) = liftOp2 . op2 $ \x y -> (x * y, \g -> (y * g, x * g))
  one = constVar NH.one

instance
  (Backprop a, Reifies s W, Divisive a, Subtractive a) =>
  Divisive (BVar s a)
  where
  recip = liftOp1 . op1 $ \x -> (recip x, (/ (x * x)) . negate)

instance
  (Backprop a, Reifies s W, Distributive a) =>
  Distributive (BVar s a)

instance
  (Backprop a, Reifies s W, InvolutiveRing a) =>
  InvolutiveRing (BVar s a)
  where
  adj = liftOp1 . op1 $ \x -> (adj x, adj)

instance
  (Backprop a, Reifies s W, StarSemiring a) =>
  StarSemiring (BVar s a)
  where
  plus = liftOp1 . op1 $ \x -> (star x, plus)
  star = liftOp1 . op1 $ \x -> (plus x, (* star x))

instance
  (Backprop a, Reifies s W, KleeneAlgebra a) =>
  KleeneAlgebra (BVar s a)

instance (Backprop a, Reifies s W, Field a) => Field (BVar s a)

instance
  ( Backprop a,
    Reifies s W,
    ExpField a
  ) =>
  ExpField (BVar s a)
  where
  exp = liftOp1 . op1 $ \x -> (exp x, (exp x *))
  log = liftOp1 . op1 $ \x -> (log x, (/ x))

instance
  ( Backprop a,
    Reifies s W,
    ExpField a,
    Subtractive a,
    TrigField a
  ) =>
  TrigField (BVar s a)
  where
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
  -- y = f(x1,x2) = atan2 x1 x2 = atan x1/x2
  -- z = g(f(x1,x2))
  -- dz/dx1 = dz/dy dy/dx1
  -- dy/dx1 = 1/(x2 + x1*x1 / x2)
  -- dz/dx1 = dz/dy * 1/(x2 + x1*x1 / x2)
  -- dz/dx2 = dz/dy dy/dx1
  -- dy/dx2 = 1/(1+ (x1/x2)^2) * -x1/x2^2
  -- dz/dx2 = dz/dy * -x / (y^2 + x^2)
  atan2 = liftOp2 . op2 $ \x y -> (atan2 x y, \g ->
        ( g / (y + x^2/y),
          g * (-x) / (x^2 + y^2)
        ))

divModOp ::
  (Subtractive a, Integral a) => Op '[a, a] (KTuple a a)
divModOp =
  op2 $ \x y ->
    ( let (d, m) = divMod x y
       in (KT d m, \(KT d' m') -> (d', m' - m' * d))
    )

quotRemOp ::
  (Subtractive a, Integral a) => Op '[a, a] (KTuple a a)
quotRemOp =
  op2 $ \x y ->
    ( let (q, r) = quotRem x y
       in (KT q r, \(KT q' r') -> (q', r' - r' * q))
    )

instance
  ( Backprop a,
    Integral a,
    UpperBoundedField a,
    Subtractive a,
    Reifies s W
  ) =>
  Integral (BVar s a)
  where
  divMod a b =
    let (splitBV -> KT a' b') = liftOp2 divModOp a b
     in (a', b')
  quotRem a b =
    let (splitBV -> KT a' b') = liftOp2 quotRemOp a b
     in (a', b')

properFractionOp ::
  (Eq a, UpperBoundedField a, QuotientField a b, FromIntegral a b) =>
  Op '[a] (KTuple b a)
properFractionOp =
  op1 $ \x ->
    (,)
      ( let (i, r) = properFraction x
         in KT i r
      )
      ( \(KT i r) ->
          if r == NH.zero
            then nan
            else NH.fromIntegral i
      )

instance
  ( Ord a,
    Ord b,
    Backprop a,
    Backprop b,
    Field a,
    Field b,
    UpperBoundedField b,
    UpperBoundedField a,
    Integral b,
    FromIntegral a b,
    Subtractive b,
    Reifies s W,
    QuotientField a b
  ) =>
  QuotientField (BVar s a) (BVar s b)
  where
  properFraction = splitKT . liftOp1 properFractionOp

instance
  (Additive a, Multiplicative a, Backprop a, Reifies s W, MeetSemiLattice a) =>
  MeetSemiLattice (BVar s a)
  where
  (/\) = liftOp2 . op2 $ \x y ->
    ( x /\ y,
      \g ->
        ( g * bool NH.zero NH.one (x /\ y == x),
          g * bool NH.zero NH.one (x /\ y == x)
        )
    )

instance
  (Additive a, Multiplicative a, Backprop a, Reifies s W, JoinSemiLattice a) =>
  JoinSemiLattice (BVar s a)
  where
  (\/) = liftOp2 . op2 $ \x y ->
    ( x \/ y,
      \g ->
        ( g * bool NH.zero NH.one (x \/ y == x),
          g * bool NH.zero NH.one (x \/ y == x)
        )
    )

instance
  (Backprop a, Reifies s W, BoundedJoinSemiLattice a, Additive a, Multiplicative a) =>
  BoundedJoinSemiLattice (BVar s a)
  where
  bottom = constVar bottom

instance
  (Backprop a, Reifies s W, BoundedMeetSemiLattice a, Additive a, Multiplicative a) =>
  BoundedMeetSemiLattice (BVar s a)
  where
  top = constVar top

instance
  (Backprop a, Reifies s W, UpperBoundedField a, Subtractive a) =>
  UpperBoundedField (BVar s a)

instance
  (Backprop a, Reifies s W, LowerBoundedField a) =>
  LowerBoundedField (BVar s a)

instance (Additive b, Backprop b, Reifies s W, FromIntegral a b) => FromIntegral (BVar s a) (BVar s b) where
  fromIntegral =
    liftOp1 . op1 $ \x -> (,) (NH.fromIntegral x) (const NH.zero)

instance (Additive a, Backprop a, Reifies s W, ToIntegral a b) => ToIntegral (BVar s a) (BVar s b) where
  toIntegral =
    liftOp1 . op1 $ \x -> (,) (NH.toIntegral x) (const NH.zero)

toRatioOp ::
  (Additive a, ToRatio a b) =>
  Op '[a] (KTuple b b)
toRatioOp =
  op1 $ \x ->
    (,)
      ( let (i :% r) = toRatio x
         in KT i r
      )
      (\(KT _ _) -> NH.zero)

instance (Additive a, Backprop a, Backprop b, Reifies s W, ToRatio a b) => ToRatio (BVar s a) (BVar s b) where
  toRatio x = (\(n, d) -> n :% d) $ splitKT (liftOp1 toRatioOp x)

fromRatioOp ::
  (Additive b, Multiplicative b, FromRatio a b) =>
  Op '[b, b] a
fromRatioOp =
  op2 $ \n d ->
    (,)
      (fromRatio (n :% d))
      (\_ -> (NH.zero, NH.one))

instance (Additive b, Multiplicative b, Backprop b, Reifies s W, FromRatio a b) => FromRatio (BVar s a) (BVar s b) where
  fromRatio (n :% d) = liftOp2 fromRatioOp n d

instance
  (Backprop a, Reifies s W, Additive a, Signed a) =>
  Signed (BVar s a)
  where
  sign = liftOp1 . op1 $ \x -> (sign x, const NH.zero)
  abs = liftOp1 . op1 $ \x -> (abs x, (* sign x))

instance
  (Additive a, Multiplicative a, Backprop a, Reifies s W, Epsilon a) =>
  Epsilon (BVar s a)
  where
  epsilon = constVar epsilon
