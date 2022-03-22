module Plutarch.ListUtilsSpec (spec) where

import Test.Syd
import Test.Syd.Hedgehog ()

import Plutarch.Prelude
import Plutarch.Test

import Hedgehog (Property)

import Plutarch.Test.Property
import Plutarch.Test.Property.Gen (genInteger, genList)

import Plutarch.ListUtils (pcheckSorted, preverse)

spec :: Spec
spec = do
  describe "extra.listutils" $ do
    describe "properties" $ do
      describe "reverse" $ do
        it "plutarch level reversing behaves like haskell level reversing" reverseTest
    pgoldenSpec $ do
      "reverse" @\ do
        "reverse_[1..5]" @| preverse # marshal [1 .. 5 :: Integer]
      "isSorted" @\ do
        "[1..10]" @| pcheckSorted # marshal [1 .. 10 :: Integer] @-> passert
        "reverse_[1..10]" @| (pnot #$ pcheckSorted #$ marshal $ reverse [1 .. 10 :: Integer]) @-> passert
        "reverse_[]" @| preverse # marshal ([] :: [Integer])

reverseTest :: Property
reverseTest = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (reverse :: [Integer] -> [Integer])
    preverse
    (genList genInteger :* Nil)
