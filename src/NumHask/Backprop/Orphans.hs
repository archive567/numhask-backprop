{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NumHask.Backprop.Orphans where

import NumHask.Prelude as NH
import NumHask.Data.Wrapped
import Numeric.Backprop.Explicit

-- * Backprop instance for a NumHask.Data.Wrapped number
instance (Additive a, Multiplicative a) => Backprop (Wrapped a) where
  zero _ = NH.zero
  one _ = NH.one
  add = (NH.+)
