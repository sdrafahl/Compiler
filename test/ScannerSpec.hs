{-# LANGUAGE NoImplicitPrelude #-}
module ScannerSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Scanner
import Data.Map
import StateMachine


spec :: Spec
spec = do
  describe "Scanner" $ do
    describe "nextWord" $ do
      it "Should return nothing if given empty stream" $ nextWord (FailedTable empty) (InputStream []) 0 (DFAacceptingStates empty) (CharCatTable empty) (DFATransitionTable empty) "testState" (TokenTypeTable empty) `shouldBe` Nothing
      it "Should return a lex for (a) input" $ nextWord (FailedTable empty) (InputStream ['a']) 0 (DFAacceptingStates (insert "TestFinalState" True empty)) (CharCatTable (insert 'a' "testCat" empty)) (DFATransitionTable (insert ("testState", "testCat") "TestFinalState" empty)) "testState" (TokenTypeTable (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType") empty)) `shouldBe` Just ((FailedTable empty), "a", 1, InputStream [])
      it "Should handle the case where the initial state is accepting and should rollback to it" $  nextWord (FailedTable empty) (InputStream ['a']) 0 (DFAacceptingStates (insert "TestInitState" True empty)) (CharCatTable (insert 'a' "testCat" empty)) (DFATransitionTable empty) "TestInitState" (TokenTypeTable (insert (GoodOrBadState "TestInitState") (TokenType "testTokenType") empty)) `shouldBe` Just ((FailedTable empty), [], 0, InputStream ['a'])
