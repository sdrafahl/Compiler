{-# LANGUAGE NoImplicitPrelude #-}
module DFAToScannerSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import DFAMinimization
import DFA
import StateMachine
import Minimization
import Data.Map
import TokenType
import DFAToScanner
import Scanner
import CharCategoryTable

spec :: Spec
spec = do

  describe "DFAToScanner" $ do
    it "Should create a Scanner given a DFA and a conversion function" $ convertDFAToScanner (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")] (fromList [("1",TokenType "testCat")])) (\a -> "testCharCategory")
      `shouldBe` (Scanner (FailedTable Data.Map.empty) (DFATransitionTable (fromList [(("0","testCharCategory"),"1")])) (DFAacceptingStates (fromList [("1",True)])) (TokenTypeTable (fromList [(GoodOrBadState "1",TokenType "testCat")])) "0" (CharCatTable (fromList [('a',"testCharCategory")])) 0)
