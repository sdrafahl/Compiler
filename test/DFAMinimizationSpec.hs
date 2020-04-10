{-# LANGUAGE NoImplicitPrelude #-}
module DFAMinimizationSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import DFAMinimization
import DFA
import StateMachine

spec :: Spec
spec = do

  describe "minimizeDFA" $ do
    it "Should minimize basic DFA (a)" $ minimizeDFA (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")]) `shouldBe` (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")])
