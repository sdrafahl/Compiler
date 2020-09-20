{-# LANGUAGE NoImplicitPrelude #-}
module ScannerSpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Scanner.Scanner
import Data.Map
import Scanner.StateMachine
import Scanner.CharCategoryTable


spec :: Spec
spec = do
  describe "Scanner" $ do
    describe "nextWord" $ do
      it "Should return nothing if given empty stream" $ nextWord (Scanner (FailedTable empty) (DFATransitionTable empty) (DFAacceptingStates empty) (TokenTypeTable empty) "testState" (CharCatTable empty) 0) (InputStream []) `shouldBe`
        ((Scanner (FailedTable empty) (DFATransitionTable empty) (DFAacceptingStates empty) (TokenTypeTable empty) "testState" (CharCatTable empty) 0), [], InputStream [], Nothing)

      it "Should return a lex for (a) input" $ nextWord (Scanner (FailedTable empty) (DFATransitionTable (insert ("testState", "testCat") "TestFinalState" empty)) (DFAacceptingStates (insert "TestFinalState" True empty)) (TokenTypeTable (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType") empty)) "testState" (CharCatTable (insert 'a' "testCat" empty)) 0) (InputStream ['a'])
        `shouldBe`
        ((Scanner (FailedTable empty) (DFATransitionTable (insert ("testState", "testCat") "TestFinalState" empty)) (DFAacceptingStates (insert "TestFinalState" True empty)) (TokenTypeTable (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType") empty)) "testState" (CharCatTable (insert 'a' "testCat" empty)) 1), ['a'], InputStream [], Just (TokenType "testTokenType")) 

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
          InputStream ['a'],
          Just (TokenType "testTokenType")
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
          InputStream ['b'],
          Just (TokenType "testTokenType2")
        )
    it "Should handle invalid characters in stream" $
        nextWord (Scanner
                  (FailedTable empty)
                  (DFATransitionTable (insert ("TestInitState", "testCat") "TestFinalState" (insert ("TestFinalState", "testCat2") "TestExtraState" empty)))
                  (DFAacceptingStates (insert "TestFinalState" True empty))
                  (TokenTypeTable (insert (GoodOrBadState "TestInitState") (TokenType "testTokenType") (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType2") (insert (GoodOrBadState "TestExtraState") (TokenType "testTokenType3") empty))))
                  "TestInitState"
                  (CharCatTable (insert 'a' "testCat" (insert 'b' "testCat2" empty)))
                  0
                 ) (InputStream [' ' ,'a', 'b'])
        `shouldBe`
        (
          (Scanner
            (FailedTable (insert ("TestInitState", 0) True empty))
            (DFATransitionTable (insert ("TestInitState", "testCat") "TestFinalState" (insert ("TestFinalState", "testCat2") "TestExtraState" empty)))
            (DFAacceptingStates (insert "TestFinalState" True empty))
            (TokenTypeTable (insert (GoodOrBadState "TestInitState") (TokenType "testTokenType") (insert (GoodOrBadState "TestFinalState") (TokenType "testTokenType2") (insert (GoodOrBadState "TestExtraState") (TokenType "testTokenType3") empty))))
            "TestInitState"
            (CharCatTable (insert 'a' "testCat" (insert 'b' "testCat2" empty)))
            0
          ),
          [],
          InputStream [' ' ,'a' ,'b'],
          Just BadTokenType
        )
  




