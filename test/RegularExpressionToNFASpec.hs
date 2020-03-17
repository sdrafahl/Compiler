{-# LANGUAGE NoImplicitPrelude #-}
module RegularExpressionToNFASpec (spec) where

import Import
import Util
import RegularExpressionToNFA
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "parenSubStringRange" $ do
    it "Should return a substring of the parenthesis" $ parenSubStringRange [Character '(', Character 'a', Character ')'] [] 0 `shouldBe` [Character '(', Character 'a', Character ')']
    it "Should return a substring of the parenthesis with more stuff" $ parenSubStringRange [Character '(', Character 'a', Character 'b', Character ')', Character 'z'] [] 0 `shouldBe` [Character '(', Character 'a', Character 'b', Character ')']
    it "Should return a substring of the parenthesis with nested paren and multiple at the top level" $ parenSubStringRange [Character '(', Character 'a', Character 'b', Character '(', Character 'a', Character 'b', Character ')', Character ')', Character 'z', Character '(', Character 'a', Character 'b', Character ')'] [] 0 `shouldBe` [Character '(', Character 'a', Character 'b', Character '(', Character 'a', Character 'b', Character ')', Character ')']

  describe "baseNfa" $ do
    it "Should create a base NFA for a character" $ baseNfa (Character 'a') `shouldBe` NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")]

  describe "mapKleenClosure" $ do
    describe "Should not map over where there are no sub NFA's" $ do
      it "basic NFA" $ mapKleenClosure [NotFA (Character 'a')] `shouldBe` [NotFA (Character 'a')]

  describe "mapOrs" $ do
    describe "Should not map over where there are no Ors" $ do
      it "basic NFA" $ mapOrs [NotFA (Character 'a')] [] `shouldBe` [NotFA (Character 'a')]
  
  describe "mapSubNFA" $ do
    describe "Should not map over where there are no sub NFA's" $ do
      it "basic NFA" $ mapSubNFA [Character 'a'] [] `shouldBe` [NotFA (Character 'a')]
    describe "Should map over wherever there are parenthesis and replace it with a NFA" $ do 
      it "basic NFA" $ mapSubNFA [Character '(', Character 'a', Character ')'] [] `shouldBe` [(FA (NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")]))] 
    
  describe "reToNFA" $ do
    it "Should create a basic NFA a" $ reToNFA [(Character 'a')] `shouldBe` NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")]
    it "Should create a basic AND NFA ab" $ (reToNFA [(Character 'a'), (Character 'b')]) `shouldBe` (NFA ["0", "1", "2", "3"] "0" ["3"] [(Transition "1" EmptyChar "2"), (Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3")])
    it "Should create a basic OR NFA a|b" $ (reToNFA [(Character 'a'), (Character '|'), (Character 'b')]) `shouldBe` (NFA ["-1", "0", "1", "2", "3", "4"] "-1" ["4"] [(Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3") ,(Transition "-1" EmptyChar "0"), (Transition "-1" EmptyChar "2"), (Transition "1" EmptyChar "4"), (Transition "3" EmptyChar "4")])
    it "Should create a basic KleenClosure NFA a*" $ (reToNFA [(Character 'a'), (Character '*')]) `shouldBe` (NFA ["-1", "0", "1", "2"] "-1" ["2"] [(Transition "-1" EmptyChar "2"), (Transition "1" EmptyChar "2"), (Transition "1" EmptyChar "0"),(Transition "0" (Character 'a') "1"), (Transition "-1" EmptyChar "0")])
    
    it "Should create a NFA for (b|c*)" $ reToNFA [(Character 'b'), (Character '|'), (Character 'c'), (Character '*')] `shouldBe` (NFA ["-1","0","1","-3","2","3","4","5"] "-1" ["5"] [(Transition "0"  (Character 'b')  "1"),(Transition "-3"  EmptyChar  "4"),(Transition "3"  EmptyChar  "4"),(Transition "3"  EmptyChar  "2"),(Transition "2"  (Character 'c')  "3"),(Transition "-3"  EmptyChar  "2"),(Transition "-1"  EmptyChar  "0"),(Transition "-1"  EmptyChar  "-3"),(Transition "1"  EmptyChar  "5"),(Transition "4"  EmptyChar  "5")])

    it "Should create a NFA for (ab|c*)" $ reToNFA [(Character 'a'), (Character 'b'), (Character '|'), (Character 'c'), (Character '*')] `shouldBe` (NFA ["0","1","-3","2","3","-5","4","5","6","7"] "0" ["7"] [Transition "1"  EmptyChar  "-3",Transition "0"  (Character 'a')  "1",Transition "2"  (Character 'b')  "3",Transition "-5"  EmptyChar  "6",Transition "5"  EmptyChar  "6",Transition "5"  EmptyChar  "4",Transition "4"  (Character 'c')  "5",Transition "-5"  EmptyChar  "4",Transition "-3"  EmptyChar  "2",Transition "-3"  EmptyChar  "-5",Transition "3"  EmptyChar  "7",Transition "6"  EmptyChar  "7"])

    it "Should create a NFA for ((ab)|c*)" $ reToNFA [(Character '('), (Character 'a'), (Character 'b'),(Character ')') , (Character '|'), (Character 'c'), (Character '*')] `shouldBe` (NFA ["-1","0","1","2","3","-5","4","5","6","7"] "-1" ["7"] [(Transition "1"  EmptyChar  "2"),(Transition "0"  (Character 'a')  "1"),(Transition "2"  (Character 'b')  "3"),(Transition "-5"  EmptyChar  "6"),(Transition "5"  EmptyChar  "6"),(Transition "5"  EmptyChar  "4"),(Transition "4"  (Character 'c')  "5"),(Transition "-5"  EmptyChar  "4"),(Transition "-1"  EmptyChar  "0"),(Transition "-1"  EmptyChar  "-5"),(Transition "3"  EmptyChar  "7"),(Transition "6"  EmptyChar  "7")])
