{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Higher-Kinded Tuple Trick numbers
module NumHask.KTuple
  ( KTupleF(..)
  , KTuple
  , splitKT
  ) where

import NumHask.Prelude
import Numeric.Backprop hiding (one, zero)
import System.Random

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data KTupleF a b f = KT
  { ktA :: HKD f a
  , ktB :: HKD f b
  }
  deriving (Generic)

type KTuple a b = KTupleF a b Identity

instance (Eq a, Eq b) => Eq (KTuple a b) where
  (==) (KT a b) (KT c d) = a == c && b == d

instance (Backprop a, Backprop b) => Backprop (KTupleF a b Identity)

splitKT ::
     (Backprop a, Backprop b, Reifies s W)
  => BVar s (KTuple a b)
  -> (BVar s a, BVar s b)
splitKT (splitBV -> KT a b) = (a, b)

instance (Random a, Random b) => Random (KTuple a b) where
  random g0 = (KT x y, g2)
    where
      (x, g1) = random g0
      (y, g2) = random g1
  randomR (KT x0 y0, KT x1 y1) g0 = (KT x y, g2)
    where
      (x, g1) = randomR (x0, x1) g0
      (y, g2) = randomR (y0, y1) g1

-- numhask classes
unaryOp ::
     (HKD f1 a1 -> HKD f2 a2)
  -> (HKD f1 b1 -> HKD f2 b2)
  -> KTupleF a1 b1 f1
  -> KTupleF a2 b2 f2
unaryOp opA opB (KT a b) = KT (opA a) (opB b)

binOp ::
     (HKD f1 a1 -> HKD f2 a2 -> HKD f3 a3)
  -> (HKD f1 b1 -> HKD f2 b2 -> HKD f3 b3)
  -> KTupleF a1 b1 f1
  -> KTupleF a2 b2 f2
  -> KTupleF a3 b3 f3
binOp opA opB (KT a b) (KT a' b') = KT (opA a a') (opB b b')

instance (Additive a, Additive b) => Additive (KTuple a b) where
  (+) = binOp (+) (+)
  zero = KT zero zero

instance (Subtractive a, Subtractive b) =>
         Subtractive (KTuple a b) where
  negate = unaryOp negate negate

instance (Multiplicative a, Multiplicative b) =>
         Multiplicative (KTuple a b) where
  (*) = binOp (*) (*)
  one = KT one one

instance (Divisive a, Divisive b) =>
         Divisive (KTuple a b) where
  recip = unaryOp recip recip

instance (Distributive a, Distributive b) => Distributive (KTuple a b)

instance (StarSemiring a, StarSemiring b) => StarSemiring (KTuple a b) where
  star = unaryOp star star
  plus = unaryOp plus plus

instance (InvolutiveRing a, InvolutiveRing b) =>
         InvolutiveRing (KTuple a b) where
  adj = unaryOp adj adj

instance (IntegralDomain a, IntegralDomain b) => IntegralDomain (KTuple a b)

instance (Field a, Field b) => Field (KTuple a b)

instance (ExpField a, ExpField b) => ExpField (KTuple a b) where
  exp = unaryOp exp exp
  log = unaryOp log log

instance (TrigField a, TrigField b) => TrigField (KTuple a b) where
  pi = KT pi pi
  sin = unaryOp sin sin
  cos = unaryOp cos cos
  asin = unaryOp asin asin
  acos = unaryOp acos acos
  atan = unaryOp atan atan
  sinh = unaryOp sinh sinh
  cosh = unaryOp cosh cosh
  asinh = unaryOp asinh asinh
  acosh = unaryOp acosh acosh
  atanh = unaryOp atanh atanh

instance (UpperBoundedField a, UpperBoundedField b) =>
         UpperBoundedField (KTuple a b)

instance (LowerBoundedField a, LowerBoundedField b) =>
         LowerBoundedField (KTuple a b)

instance ( Eq b
         , Ord a
         , Ord (KTuple c d)
         , Ord (KTuple a b)
         , QuotientField a c
         , QuotientField b d
         , Integral (KTuple c d)
         , Subtractive c
         , Subtractive d
         ) =>
         QuotientField (KTuple a b) (KTuple c d) where
  properFraction (KT a b) = (KT ia ib, KT ra rb)
    where
      (ia, ra) = properFraction a
      (ib, rb) = properFraction b

instance (Multiplicative a, Signed a, Multiplicative b, Signed b) => Signed (KTuple a b) where
  sign = unaryOp sign sign
  abs = unaryOp abs abs

instance (Normed a c, Normed b d) => Normed (KTuple a b) (KTuple c d) where
  normL1 = unaryOp normL1 normL1
  normL2 = unaryOp normL2 normL2
  normLp (KT pa pb) = unaryOp (normLp pa) (normLp pb)

instance (Metric a a, Metric b b) => Metric (KTuple a b) (KTuple a b) where
  distanceL1 = binOp distanceL1 distanceL1
  distanceL2 = binOp distanceL1 distanceL1
  distanceLp (KT pa pb) = binOp (distanceLp pa) (distanceLp pb)

instance (FromInteger a, FromInteger b) => FromInteger (KTuple a b) where
  fromInteger r = KT (fromInteger r) (fromInteger r)

instance (FromRatio a, FromRatio b) => FromRatio (KTuple a b) where
  fromRatio r = KT (fromRatio r) (fromRatio r)
