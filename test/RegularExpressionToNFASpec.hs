{-# LANGUAGE NoImplicitPrelude #-}
module RegularExpressionToNFASpec (spec) where

import Misc.Import
import Misc.Util
import Scanner.RegularExpressionToNFA
import Test.Hspec
import Test.Hspec.QuickCheck
import Scanner.RegularExpression
import Data.Map
import Scanner.TokenType

spec :: Spec
spec = do
  describe "reToNFA" $ do
    it "Should create a basic NFA a" $  convert (RegEx "a" (TokenType "testCat")) `shouldBe` NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")] (fromList [("1" ,TokenType "testCat")]) 

    it "Should create a basic AND NFA ab" $ convert (RegEx ['a', 'b'] (TokenType "testCat")) `shouldBe` (NFA ["0", "1", "2", "3"] "0" ["3"] [(Transition "1" EmptyChar "2"), (Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3")]) (fromList [("3" ,TokenType "testCat")])

    it "Should create a basic OR NFA a|b" $ convert (RegEx "a|b" (TokenType "testCat")) `shouldBe` (NFA ["-1", "0", "1", "2", "3", "4"] "-1" ["4"] [(Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3") ,(Transition "-1" EmptyChar "0"), (Transition "-1" EmptyChar "2"), (Transition "1" EmptyChar "4"), (Transition "3" EmptyChar "4")]) (fromList [("4" ,TokenType "testCat")])

    it "Should create a basic KleenClosure NFA a*" $ convert (RegEx "a*" (TokenType "testCat")) `shouldBe` (NFA ["-1", "0", "1", "2"] "-1" ["2"] [(Transition "-1" EmptyChar "2"), (Transition "1" EmptyChar "2"), (Transition "1" EmptyChar "0"),(Transition "0" (Character 'a') "1"), (Transition "-1" EmptyChar "0")]) (fromList [("2" ,TokenType "testCat")])
    
    it "Should create a NFA for b|c*" $ convert (RegEx "b|c*" (TokenType "testCat")) `shouldBe` (NFA ["-1","0","1","-3","2","3","4","5"] "-1" ["5"] [(Transition "0"  (Character 'b')  "1"),(Transition "-3"  EmptyChar  "4"),(Transition "3"  EmptyChar  "4"),(Transition "3"  EmptyChar  "2"),(Transition "2"  (Character 'c')  "3"),(Transition "-3"  EmptyChar  "2"),(Transition "-1"  EmptyChar  "0"),(Transition "-1"  EmptyChar  "-3"),(Transition "1"  EmptyChar  "5"),(Transition "4"  EmptyChar  "5")]) (fromList [("5" ,TokenType "testCat")])

    it "Should create a NFA for (ab|c*)" $ convert (RegEx "ab|c*" (TokenType "testCat")) `shouldBe` (NFA ["0","1","-3","2","3","-5","4","5","6","7"] "0" ["7"] [Transition "1"  EmptyChar  "-3",Transition "0"  (Character 'a')  "1",Transition "2"  (Character 'b')  "3",Transition "-5"  EmptyChar  "6",Transition "5"  EmptyChar  "6",Transition "5"  EmptyChar  "4",Transition "4"  (Character 'c')  "5",Transition "-5"  EmptyChar  "4",Transition "-3"  EmptyChar  "2",Transition "-3"  EmptyChar  "-5",Transition "3"  EmptyChar  "7",Transition "6"  EmptyChar  "7"]) (fromList [("7" ,TokenType "testCat")])

    it "Should create a NFA for ((ab)|c*)" $ convert (RegEx "((ab)|c*)" (TokenType "testCat")) `shouldBe` (NFA ["-1","0","1","2","3","-5","4","5","6","7"] "-1" ["7"] [(Transition "1"  EmptyChar  "2"),(Transition "0"  (Character 'a')  "1"),(Transition "2"  (Character 'b')  "3"),(Transition "-5"  EmptyChar  "6"),(Transition "5"  EmptyChar  "6"),(Transition "5"  EmptyChar  "4"),(Transition "4"  (Character 'c')  "5"),(Transition "-5"  EmptyChar  "4"),(Transition "-1"  EmptyChar  "0"),(Transition "-1"  EmptyChar  "-5"),(Transition "3"  EmptyChar  "7"),(Transition "6"  EmptyChar  "7")]) (fromList [("7" ,TokenType "testCat")])
