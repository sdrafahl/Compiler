{-# LANGUAGE NoImplicitPrelude #-}
module RegularExpressionToNFASpec (spec) where

import Import
import Util
import RegularExpressionToNFA
import Test.Hspec
import Test.Hspec.QuickCheck
import RegularExpression

spec :: Spec
spec = do
  describe "reToNFA" $ do
    it "Should create a basic NFA a" $  convert (RE [(Character 'a')]) `shouldBe` NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")]
    it "Should create a basic AND NFA ab" $ convert (RE [(Character 'a'), (Character 'b')]) `shouldBe` (NFA ["0", "1", "2", "3"] "0" ["3"] [(Transition "1" EmptyChar "2"), (Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3")])
    it "Should create a basic OR NFA a|b" $ convert (RE [(Character 'a'), (Character '|'), (Character 'b')]) `shouldBe` (NFA ["-1", "0", "1", "2", "3", "4"] "-1" ["4"] [(Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3") ,(Transition "-1" EmptyChar "0"), (Transition "-1" EmptyChar "2"), (Transition "1" EmptyChar "4"), (Transition "3" EmptyChar "4")])
    it "Should create a basic KleenClosure NFA a*" $ convert (RE [(Character 'a'), (Character '*')]) `shouldBe` (NFA ["-1", "0", "1", "2"] "-1" ["2"] [(Transition "-1" EmptyChar "2"), (Transition "1" EmptyChar "2"), (Transition "1" EmptyChar "0"),(Transition "0" (Character 'a') "1"), (Transition "-1" EmptyChar "0")])
    
    it "Should create a NFA for (b|c*)" $ convert (RE [(Character 'b'), (Character '|'), (Character 'c'), (Character '*')]) `shouldBe` (NFA ["-1","0","1","-3","2","3","4","5"] "-1" ["5"] [(Transition "0"  (Character 'b')  "1"),(Transition "-3"  EmptyChar  "4"),(Transition "3"  EmptyChar  "4"),(Transition "3"  EmptyChar  "2"),(Transition "2"  (Character 'c')  "3"),(Transition "-3"  EmptyChar  "2"),(Transition "-1"  EmptyChar  "0"),(Transition "-1"  EmptyChar  "-3"),(Transition "1"  EmptyChar  "5"),(Transition "4"  EmptyChar  "5")])

    it "Should create a NFA for (ab|c*)" $ convert (RE [(Character 'a'), (Character 'b'), (Character '|'), (Character 'c'), (Character '*')]) `shouldBe` (NFA ["0","1","-3","2","3","-5","4","5","6","7"] "0" ["7"] [Transition "1"  EmptyChar  "-3",Transition "0"  (Character 'a')  "1",Transition "2"  (Character 'b')  "3",Transition "-5"  EmptyChar  "6",Transition "5"  EmptyChar  "6",Transition "5"  EmptyChar  "4",Transition "4"  (Character 'c')  "5",Transition "-5"  EmptyChar  "4",Transition "-3"  EmptyChar  "2",Transition "-3"  EmptyChar  "-5",Transition "3"  EmptyChar  "7",Transition "6"  EmptyChar  "7"])

    it "Should create a NFA for ((ab)|c*)" $ convert (RE [(Character '('), (Character 'a'), (Character 'b'),(Character ')') , (Character '|'), (Character 'c'), (Character '*')]) `shouldBe` (NFA ["-1","0","1","2","3","-5","4","5","6","7"] "-1" ["7"] [(Transition "1"  EmptyChar  "2"),(Transition "0"  (Character 'a')  "1"),(Transition "2"  (Character 'b')  "3"),(Transition "-5"  EmptyChar  "6"),(Transition "5"  EmptyChar  "6"),(Transition "5"  EmptyChar  "4"),(Transition "4"  (Character 'c')  "5"),(Transition "-5"  EmptyChar  "4"),(Transition "-1"  EmptyChar  "0"),(Transition "-1"  EmptyChar  "-5"),(Transition "3"  EmptyChar  "7"),(Transition "6"  EmptyChar  "7")])
