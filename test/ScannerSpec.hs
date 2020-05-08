{-# LANGUAGE NoImplicitPrelude #-}
module ScannerSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Scanner
import Data.Map
import StateMachine
import CharCategoryTable


spec :: Spec
spec = do
  describe "Scanner" $ do
    describe "nextWord" $ do
      it "Should return nothing if given empty stream" $ nextWord (Scanner (FailedTable empty) (DFATransitionTable empty) (DFAacceptingStates empty) (TokenTypeTable empty) "testState" (CharCatTable empty) 0) (InputStream []) `shouldBe`
        ((Scanner (FailedTable empty) (DFATransitionTable empty) (DFAacceptingStates empty) (TokenTypeTable empty) "testState" (CharCatTable empty) 0), [], InputStream [])

      it "Should return a lex for (a) input" $ nextWord (Scanner (FailedTable empty) (DFATransitionTable (insert ("testState", "testCat") "TestFinalState" empty)) (DFAacceptingStates (insert "TestFinalState" True empty)) (TokenTypeTable (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType") empty)) "testState" (CharCatTable (insert 'a' "testCat" empty)) 0) (InputStream ['a']) `shouldBe` ((Scanner (FailedTable empty) (DFATransitionTable (insert ("testState", "testCat") "TestFinalState" empty)) (DFAacceptingStates (insert "TestFinalState" True empty)) (TokenTypeTable (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType") empty)) "testState" (CharCatTable (insert 'a' "testCat" empty)) 1), ['a'], InputStream []) 

      it "Should handle the case where the initial state is accepting and should rollback to it" $
        nextWord (Scanner
                   (FailedTable empty)
                   (DFATransitionTable empty)
                   (DFAacceptingStates (insert "TestInitState" True empty))
                   (TokenTypeTable (insert (GoodOrBadState "TestInitState") (TokenType "testTokenType") empty))
                   "TestInitState"
                   (CharCatTable (insert 'a' "testCat" empty))
                   0
                 ) (InputStream ['a'])
        `shouldBe`
        (
          (Scanner
            (FailedTable empty)
            (DFATransitionTable empty)
            (DFAacceptingStates (insert "TestInitState" True empty))
            (TokenTypeTable (insert (GoodOrBadState "TestInitState") (TokenType "testTokenType") empty))
            "TestInitState"
            (CharCatTable (insert 'a' "testCat" empty))
            0
          ),
          [],
          InputStream ['a']
        )

    it "Should return the string (a) and return the rest of the stream [b] after rolling back, the valid DFA map regex is (a)" $
              nextWord (Scanner
                  (FailedTable empty)
                  (DFATransitionTable (insert ("TestInitState", "testCat") "TestFinalState" (insert ("TestFinalState", "testCat2") "TestExtraState" empty)))
                  (DFAacceptingStates (insert "TestFinalState" True empty))
                  (TokenTypeTable (insert (GoodOrBadState "TestInitState") (TokenType "testTokenType") (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType2") (insert (GoodOrBadState "TestExtraState") (TokenType "testTokenType3") empty))))
                  "TestInitState"
                  (CharCatTable (insert 'a' "testCat" (insert 'b' "testCat2" empty)))
                  0
                 ) (InputStream ['a', 'b'])
        `shouldBe`
        (
          (Scanner
            (FailedTable (insert ("TestExtraState", 2) True empty))
            (DFATransitionTable (insert ("TestInitState", "testCat") "TestFinalState" (insert ("TestFinalState", "testCat2") "TestExtraState" empty)))
            (DFAacceptingStates (insert "TestFinalState" True empty))
            (TokenTypeTable (insert (GoodOrBadState "TestInitState") (TokenType "testTokenType") (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType2") (insert (GoodOrBadState "TestExtraState") (TokenType "testTokenType3") empty))))
            "TestInitState"
            (CharCatTable (insert 'a' "testCat" (insert 'b' "testCat2" empty)))
            1
          ),
          ['a'],
          InputStream ['b']
        )




