{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Higher-Kinded Tuple Trick numbers
module NumHask.KTuple
  ( KTupleF (..),
    KTuple,
    splitKT,
  )
where

import NumHask.Prelude
import Numeric.Backprop hiding (one, zero)

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data KTupleF a b f
  = KT
      { ktA :: HKD f a,
        ktB :: HKD f b
      }
  deriving (Generic)

type KTuple a b = KTupleF a b Identity

instance (Eq a, Eq b) => Eq (KTuple a b) where
  (==) (KT a b) (KT c d) = a == c && b == d

instance (Backprop a, Backprop b) => Backprop (KTupleF a b Identity)

splitKT ::
  (Backprop a, Backprop b, Reifies s W) =>
  BVar s (KTuple a b) ->
  (BVar s a, BVar s b)
splitKT (splitBV -> KT a b) = (a, b)
