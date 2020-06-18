{-# LANGUAGE NoImplicitPrelude #-}
module Parser.FirstSpec (spec) where

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
import Data.Map
import Data.Set
import Parser.CFG
import Parser.First

--------------------------- CASE A
-- Term -> [ Term
--      -> ] 
---------------------------


spec :: Spec
spec = do
  describe "createFirstMap" $ do
    it "Should create a First Mapping for case A" $ createFirstMap (Data.Set.fromList [ProductionRule (NonTerminal "Term", [Term (Terminal "["), NonTerm (NonTerminal "Term")]),  ProductionRule (NonTerminal "Term" , [Term (Terminal "]")])]) (Data.Set.fromList [Terminal "[", Terminal "]"]) 
      `shouldBe`
      (First (Data.Map.fromList [(Term (Terminal "EOF"),Data.Set.fromList [Terminal "EOF"]),(Term (Terminal "["),Data.Set.fromList [Terminal "["]),(Term (Terminal "]"), Data.Set.fromList [Terminal "]"]),(Term (Terminal "\948"),Data.Set.fromList [Terminal "\948"]),(NonTerm (NonTerminal "Term"),Data.Set.fromList [Terminal "[",Terminal "]",Terminal "\948"])]))

