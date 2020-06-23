{-# LANGUAGE NoImplicitPrelude #-}
module DFAToScannerSpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Scanner.DFAMinimization
import Scanner.DFA
import Scanner.StateMachine
import Scanner.Minimization
import Data.Map
import Scanner.TokenType
import Scanner.DFAToScanner
import Scanner.Scanner
import Scanner.CharCategoryTable

spec :: Spec
spec = do

  describe "DFAToScanner" $ do
    it "Should create a Scanner given a DFA and a conversion function" $ convertDFAToScanner (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")] (fromList [("1",TokenType "testCat")])) (\a -> "testCharCategory")
      `shouldBe` (Scanner (FailedTable Data.Map.empty) (DFATransitionTable (fromList [(("0","testCharCategory"),"1")])) (DFAacceptingStates (fromList [("1",True)])) (TokenTypeTable (fromList [(GoodOrBadState "1",TokenType "testCat")])) "0" (CharCatTable (fromList [('a',"testCharCategory")])) 0)
