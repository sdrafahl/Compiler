{-# LANGUAGE NoImplicitPrelude #-}
module DFAMinimizationSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import DFAMinimization
import DFA
import StateMachine
import Minimization
import Data.Map
import Category 

spec :: Spec
spec = do

  describe "minimizeDFA" $ do
    it "Should minimize basic DFA (a)" $ minimize (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")] (fromList [("1",Category "testCat")])) `shouldBe` (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")] (fromList [("0",NoCategory),("1",Category "testCat")]))
    it "Should minimize DFA (ab)" $ minimize (DFA ["0", "1", "2"] "0" ["2"] [(Transition "0" 'a' "1"), (Transition "1" 'b' "2")] (fromList [("2", Category "testCat")])) `shouldBe` (DFA ["0", "1", "2"] "1" ["2"] [(Transition "1" 'a' "0"), (Transition "0" 'b' "2")] (fromList [("0",NoCategory),("1",NoCategory),("2",Category "testCat")]))
    it "Should minimize DFA (a|b) variant 0" $ minimize ((DFA ["0", "1", "2"] "0" ["1", "2"] [(Transition "0" 'b' "1"), (Transition "0" 'a' "2")]) (fromList [("1", Category "testCat"), ("2", Category "testCat")])) `shouldBe` ((DFA ["0", "1"] "0" ["1"] [(Transition "0" 'b' "1"), (Transition "0" 'a' "1")]) (fromList [("0",NoCategory),("1",Category "testCat")]))
    it "Should minimize DFA (a|b) variant 1" $ minimize (DFA ["0", "1"] "0" ["0" ,"1"] [(Transition "0" 'a' "1"), (Transition "1" 'a' "1")] (fromList [("0", Category "testCat"),("1",Category "testCat")])) `shouldBe` (DFA ["0"] "0" ["0"] [(Transition "0" 'a' "0")] (fromList [("0", Category "testCat")]) )
