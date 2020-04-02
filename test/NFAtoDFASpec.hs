{-# LANGUAGE NoImplicitPrelude #-}
module NFAtoDFASpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import RegularExpression
import NFAtoDFA
import StateMachine
import DFA
import NFA
import Data.Map (Map)
import qualified Data.Map as Map

spec :: Spec
spec = do

  
  describe "NFAToDFA" $ do
    it "Should create a basic DFA (a)" $ convert (NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")]) `shouldBe` (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")])
    it "Should create a basic DFA (ab)" $ convert (NFA ["0", "1", "2", "3"] "0" ["3"] [(Transition "1" EmptyChar "2"), (Transition "0" (Character 'a') "1"), (Transition "2" (Character 'b') "3")]) `shouldBe` (DFA ["0", "1", "2"] "0" ["2"] [(Transition "0" 'a' "1"), (Transition "1" 'b' "2")])
    it "Should create a basic DFA (a|b)" $ convert (NFA ["1","2","3","4","5","6"] "1" ["6"] [(Transition "1"  EmptyChar  "2"),(Transition "1"  EmptyChar  "3"),(Transition "2"  (Character 'a')  "4"),(Transition "3"  (Character 'b')  "5"),(Transition "4" EmptyChar "6"),(Transition "5"  EmptyChar  "6")]) `shouldBe` (DFA ["0", "1", "2"] "0" ["1", "2"] [(Transition "0" 'b' "1"), (Transition "0" 'a' "2")])
    
