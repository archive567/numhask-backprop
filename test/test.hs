{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Backprop ()
import NumHask.Hedgehog
import NumHask.Prelude as NH
import Numeric.Backprop as BP
import Test.DocTest
import Hedgehog as H
import qualified Prelude as P

tests :: H.TestLimit -> IO Bool
tests n = do
  bAdditive <- assertProps "additive" n
    (integral_ :: H.Gen Int) additiveBVarProps
  return bAdditive

-- | additive
additiveBVarProps ::
     (Backprop a, Eq a, Show a, Multiplicative a, Additive a) => Gen a -> [(PropertyName, Property)]
additiveBVarProps src =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , ternary src
        (\a b c ->
           evalBP0 ((auto a + auto b) + auto c) ==
           evalBP0 (auto a + (auto b + auto c))))
  , ( "left id: zero + a = a"
    , unary src (\a -> evalBP0 (auto NH.zero + auto a) == evalBP0 (auto a)))
  , ( "right id: a + zero = a"
    , unary src (\a -> evalBP0 (auto a + auto NH.zero) == evalBP0 (auto a)))
  , ( "commutative: a + b == b + a"
    , binary src (\a b -> evalBP0 (auto a + auto b) == evalBP0 (auto b + auto a)))
  ]

main :: IO ()
main = do
  doctest ["app/example.hs"]
  ok <- tests (P.fromInteger 100 :: H.TestLimit)
  unless ok
    exitFailure
