{-# LANGUAGE NoImplicitPrelude #-}
module NFAtoDFASpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Scanner.RegularExpression
import Scanner.NFAtoDFA
import Scanner.StateMachine
import Scanner.DFA
import Scanner.NFA
import Data.Map (Map)
import Data.Map
import Scanner.TokenType

spec :: Spec
spec = do
  describe "NFAToDFA" $ do

    it "Should create a basic DFA (a)" $ convert (NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")] (fromList [("1", TokenType "testCat")])) `shouldBe` (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")] (fromList [("0",BadTokenType),("1",TokenType "testCat")]))

    it "Should create a basic DFA (ab)" $ convert (NFA ["0", "1", "2", "3"] "0" ["3"] [(Transition "1" EmptyChar "2"), (Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3")] (fromList [("3", TokenType "testCat")])) `shouldBe` (DFA ["0", "1", "2"] "0" ["2"] [(Transition "0" 'a' "1"), (Transition "1" 'b' "2")] (fromList [("0",BadTokenType),("1",BadTokenType),("2",TokenType "testCat")]))

    it "Should create a basic DFA (a|b)" $ convert (NFA ["1","2","3","4","5","6"] "1" ["6"] [(Transition "1"  EmptyChar  "2"),(Transition "1"  EmptyChar  "3"),(Transition "2"  (Character 'a') "4"),(Transition "3"  (Character 'b')  "5"),(Transition "4" EmptyChar "6"),(Transition "5"  EmptyChar  "6")] (fromList [("6", TokenType "testCat")])) `shouldBe` (DFA ["0", "1", "2"] "0" ["1", "2"] [(Transition "0" 'b' "1"), (Transition "0" 'a' "2")] (fromList [("0",BadTokenType),("1",TokenType "testCat"),("2",TokenType "testCat")]))

    it "should create a basic DFA (a*)" $ convert (NFA ["-1", "0", "1", "2"] "-1" ["2"] [(Transition "-1" EmptyChar "2"), (Transition "1" EmptyChar "2"), (Transition "1" EmptyChar "0"),(Transition "0" (Character 'a') "1"), (Transition "-1" EmptyChar "0")] (fromList [("2", TokenType "testCat")])) `shouldBe` (DFA ["0", "1"] "0" ["0" ,"1"] [(Transition "0" 'a' "1"), (Transition "1" 'a' "1")] (fromList [("0",TokenType "testCat"),("1",TokenType "testCat")]))
