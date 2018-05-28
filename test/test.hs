{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Backprop ()
import NumHask.Laws
import NumHask.Prelude as NH
import Numeric.Backprop as BP
import Test.DocTest
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests = testGroup "NumHask BVar" [testsInt]

-- | additive
additiveBVarLaws ::
     (Backprop a, Eq a, MultiplicativeUnital a, Additive a) => [Law a]
additiveBVarLaws =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , Ternary
        (\a b c ->
           evalBP0 ((auto a + auto b) + auto c) ==
           evalBP0 (auto a + (auto b + auto c))))
  , ( "left id: zero + a = a"
    , Unary (\a -> evalBP0 (auto NH.zero + auto a) == evalBP0 (auto a)))
  , ( "right id: a + zero = a"
    , Unary (\a -> evalBP0 (auto a + auto NH.zero) == evalBP0 (auto a)))
  , ( "commutative: a + b == b + a"
    , Binary (\a b -> evalBP0 (auto a + auto b) == evalBP0 (auto b + auto a)))
  ]

testsInt :: TestTree
testsInt =
  testGroup
    "Int"
    [testGroup "Additive BVar" $ testLawOf ([] :: [Int]) <$> additiveBVarLaws]

main :: IO ()
main = do
  doctest ["app/example.hs"]
  defaultMain tests
