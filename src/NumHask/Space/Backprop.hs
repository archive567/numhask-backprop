{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Backprop instances for NumHask classes.
module NumHask.Space.Backprop ( rotateP', bEllipse ) where

import NumHask.Prelude as NH
import NumHask.Space
import Numeric.Backprop as BP
import NumHask.Backprop ()
import Data.Vinyl.Core

instance (Additive a, Multiplicative a) => Backprop (Point a) where
  zero = const NH.zero
  add = (+)
  one = const NH.one

instance (Backprop a, Reifies s W, TrigField a) =>
  Direction (BVar s (Point a)) (BVar s a) where
  angle = liftOp1 . op1 $
    \p@(Point x y) -> (angle p, \g -> g .* Point (NH.one / (y + x*x/y)) ((-x)/(x*x+y*y)))
  -- https://backprop.jle.im/06-manual-gradients.html
  --
  -- ray :: a -> Point a
  -- rayScaledGrad :: a -> Point a -> a
  --
  -- step 1:
  -- y = f(a) = Point (negate (sin a)) (cos a)
  -- z = g(y) = g(negate (sin a), cos a)
  --
  -- step 2:
  -- Gradient to be returned:
  -- <dz/da> (which is an a)
  --
  -- need to find dz/da in terms of dz/dy
  -- dz/da = dz/dy dy/da
  --       = dz/dy Point (negate (cos x)) (negate (sin x))
  --
  ray = liftOp1 . op1 $ \x ->
    (ray x, \g -> angle (g * Point (negate (cos x)) (negate (sin x))))


-- | rotateP
-- rotateP :: a -> Point a -> Point a
-- rotatePScaledGrad :: a -> Point a -> (a,Point a)
-- (x, y) -> (ax + by + c, dx + ey + d)
-- rotateP a (Point x y) = Point (x * cos a - y * sin a) (x * sin a + y * cos a)
--
-- y = f(a,p) = rotateP a p
-- z = g(f(a,p))
-- dz/dr = dz/dy dy/da
--       = dz/dy * Point (-x * sin a - y * cos a) (x * cos a - y * sin a)
-- dz/dp = dz/dy dy/dp
--       = dz/dy * Point (-x * sin a - y * cos a) (x * cos a - y * sin a)
rotateP' :: (Backprop a, Reifies s W, TrigField a) => BVar s a -> BVar s (Point a) -> BVar s (Point a)
rotateP' = liftOp2 . op2 $ \a (Point x y) ->
  (Point (x * cos a - y * sin a) (x * sin a + y * cos a),
   \g -> (angle $ g * Point (-x * sin a - y * cos a) (x * cos a - y * sin a),
         g * Point (cos a - sin a) (sin a + cos a)
        ))

-- |
-- >>> let p = ArcPosition (Point 0 0) (Point 1 0) (ArcInfo (Point 1 2) (pi/6) True True)
-- >>> (ArcCentroid c r phi ang0 angd) = arcCentroid p
-- >>> backpropN bEllipse (Identity c:&Identity phi:& Identity r:&Identity ang0 :& RNil)
-- (Point 1.5775576228680996 0.8614484656909314,{Identity Point 1.0 1.0, Identity 2.0706131429180084, Identity Point 9.573662665825866e-17 -0.716109157177168, Identity 1.5707963267948968})
bEllipse :: forall s. (Reifies s W) =>
  Rec (BVar s) '[Point Double, Double, Point Double, Double] -> BVar s (Point Double)
bEllipse (c:&phi:&r:&theta:&RNil) = c + rotateP' phi (r * ray theta)

