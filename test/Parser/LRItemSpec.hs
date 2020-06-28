{-# LANGUAGE NoImplicitPrelude #-}
module Parser.LRItemSpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Map (Map)
import Data.Map
import Data.Set
import Parser.CFG
import Parser.First
import Parser.LRItem

------------------------------------- case A
-- Goal -> List
-- List -> List Pair
--      -> Pair
-- Pair -> ( Pair )
--      -> ( )
-------------------------------------
caseAProductions = (Data.Set.fromList [ProductionRule (NonTerminal "Goal", [NonTerm (NonTerminal "List")]), ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "List"), NonTerm (NonTerminal "Pair")]), ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "Pair")]), ProductionRule (NonTerminal "Pair", [Term (Terminal "("), NonTerm (NonTerminal "Pair"), Term (Terminal ")")]), ProductionRule (NonTerminal "Pair", [Term (Terminal "("), Term (Terminal ")")])])
caseALRItem = (Data.Set.fromList [LRItem (NonTerminal "Goal") [StackTop, Token (NonTerm (NonTerminal "List"))] (Terminal "eof")])
caseATerminals = (Data.Set.fromList [Terminal "(", Terminal ")"])
expectedCaseALRitems = Data.Set.fromList [(LRItem (NonTerminal "Goal") [StackTop,Token (NonTerm (NonTerminal "List"))]  (Terminal "eof")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "List")),Token (NonTerm (NonTerminal "Pair"))] (Terminal "(")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "List")),Token (NonTerm (NonTerminal "Pair"))] (Terminal "eof")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "Pair"))] (Terminal "(")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "Pair"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (Term (Terminal ")"))] (Terminal "(")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (Term (Terminal ")"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (NonTerm (NonTerminal "Pair")),Token (Term (Terminal ")"))] (Terminal "(")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (NonTerm (NonTerminal "Pair")),Token (Term (Terminal ")"))] (Terminal "eof"))]

                   
spec :: Spec
spec = do
  describe "closure" $ do
    it "Should apply a closure to the lritem" $ do closure caseATerminals caseAProductions caseALRItem `shouldBe` expectedCaseALRitems



