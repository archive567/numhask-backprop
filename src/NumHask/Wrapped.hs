{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Wrapped
  ( NH(..)
  ) where

import NumHask.Prelude as NH
import qualified Numeric.Backprop as IBP
import Numeric.Backprop.Explicit as BP
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype NH a = NH
  { unwrap :: a
  } deriving ( Eq
             , Ord
             , Show
             , AdditiveMagma
             , AdditiveAssociative
             , AdditiveCommutative
             , AdditiveUnital
             , AdditiveIdempotent
             , Additive
             , AdditiveInvertible
             , AdditiveGroup
             , MultiplicativeMagma
             , MultiplicativeUnital
             , MultiplicativeAssociative
             , MultiplicativeCommutative
             , MultiplicativeIdempotent
             , Multiplicative
             , MultiplicativeInvertible
             , MultiplicativeGroup
             , Distribution
             , Semiring
             , Ring
             , CRing
             , StarSemiring
             , KleeneAlgebra
             , InvolutiveRing
             , Semifield
             , Field
             , ExpField
             , TrigField
             , UpperBoundedField
             , LowerBoundedField
             , Signed
             , Integral
             , ToInteger
             , FromInteger
             , FromRatio
             , ToRatio
             , Arbitrary
             )

instance ( Eq b
         , Ord a
         , Field a
         , Field b
         , QuotientField a b
         , Integral b
         , MultiplicativeGroup a
         , MultiplicativeGroup b
         ) =>
         QuotientField (NH a) (NH b) where
  properFraction (NH a) =
    let (i, r) = properFraction a
    in (NH i, NH r)

instance (Normed a b) => Normed (NH a) (NH b) where
  normL1 (NH a) = NH $ normL1 a
  normL2 (NH a) = NH $ normL2 a
  normLp (NH p) (NH a) = NH $ normLp p a

instance (Metric a b) => Metric (NH a) (NH b) where
  distanceL1 (NH a) (NH b) = NH $ distanceL1 a b
  distanceL2 (NH a) (NH b) = NH $ distanceL2 a b
  distanceLp (NH p) (NH a) (NH b) = NH $ distanceLp p a b

instance (Epsilon a) => Epsilon (NH a) where
  nearZero (NH a) = nearZero a

-- * Backprop instance for a NH wrapped number
instance (Additive a, MultiplicativeUnital a) => Backprop (NH a) where
  zero _ = NH.zero
  one _ = NH.one
  add = (NH.+)
