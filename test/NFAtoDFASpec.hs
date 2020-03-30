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
    it "Should create a basic NFA (a) to DFA (a)" $ convert (NFA ["0", "1"] "0" ["1"] [(Transition "0" (Character 'a') "1")]) `shouldBe`  (DFA ["0", "1"] "0" ["1"] [(Transition "0" 'a' "1")])
