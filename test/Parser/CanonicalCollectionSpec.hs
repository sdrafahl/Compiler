{-# LANGUAGE NoImplicitPrelude #-}
module Parser.CanonicalCollectionSpec (spec) where

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
import Parser.CanonicalCollection
import Data.List

------------------------------------- case A
-- Goal -> List
-- List -> List Pair
--      -> Pair
-- Pair -> ( Pair )
--      -> ( )
-------------------------------------
caseAProductions = (Data.Set.fromList [ProductionRule (NonTerminal "Goal", [NonTerm (NonTerminal "List")]), ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "List"), NonTerm (NonTerminal "Pair")]), ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "Pair")]), ProductionRule (NonTerminal "Pair", [Term (Terminal "("), NonTerm (NonTerminal "Pair"), Term (Terminal ")")]), ProductionRule (NonTerminal "Pair", [Term (Terminal "("), Term (Terminal ")")])])
caseALRItem = LRItem (NonTerminal "Goal") [StackTop, Token (NonTerm (NonTerminal "List"))] (Terminal "eof")
caseATerminals = (Data.Set.fromList [Terminal "(", Terminal ")"])
expectedCCAndTransations = (CC [], Transitions Data.Map.empty)

cc0 = Data.Set.fromList [(LRItem (NonTerminal "Goal") [StackTop,Token (NonTerm (NonTerminal "List"))]  (Terminal "eof")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "List")),Token (NonTerm (NonTerminal "Pair"))] (Terminal "(")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "List")),Token (NonTerm (NonTerminal "Pair"))] (Terminal "eof")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "Pair"))] (Terminal "(")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "Pair"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (Term (Terminal ")"))] (Terminal "(")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (Term (Terminal ")"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (NonTerm (NonTerminal "Pair")),Token (Term (Terminal ")"))] (Terminal "(")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (NonTerm (NonTerminal "Pair")),Token (Term (Terminal ")"))] (Terminal "eof"))]

cc1 = Data.Set.fromList [(LRItem (NonTerminal "Goal") [Token (NonTerm (NonTerminal "List")), StackTop]  (Terminal "eof")),(LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")"))]  (Terminal "eof")),(LRItem (NonTerminal "List") [Token (NonTerm (NonTerminal "List")), StackTop, Token (NonTerm (NonTerminal "Pair"))]  (Terminal "eof")),(LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")"))]  (Terminal "(")),(LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (Term (Terminal ")"))]  (Terminal "(")),(LRItem (NonTerminal "List") [Token (NonTerm (NonTerminal "List")), StackTop, Token (NonTerm (NonTerminal "Pair"))] (Terminal "(")),(LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (Term (Terminal ")"))] (Terminal "eof"))]

cc2 = Data.Set.fromList [(LRItem (NonTerminal "List") [Token (NonTerm (NonTerminal "Pair")), StackTop]  (Terminal "eof")), (LRItem (NonTerminal "List") [Token (NonTerm (NonTerminal "Pair")), StackTop]  (Terminal "("))]

cc3 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")"))] (Terminal ")")), (LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (Term (Terminal ")"))] (Terminal ")")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), StackTop, Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), StackTop, Token (Term (Terminal ")"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), StackTop, Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")"))] (Terminal "(")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), StackTop, Token (Term (Terminal ")"))] (Terminal "("))]

cc4 = Data.Set.fromList [(LRItem (NonTerminal "List") [Token (NonTerm (NonTerminal "List")), Token (NonTerm (NonTerminal "Pair")), StackTop]  (Terminal "eof")), (LRItem (NonTerminal "List") [Token (NonTerm (NonTerminal "List")), Token (NonTerm (NonTerminal "Pair")), StackTop]  (Terminal "("))]

cc5 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), StackTop, Token (Term (Terminal ")"))]  (Terminal "eof")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), StackTop, Token (Term (Terminal ")"))]  (Terminal "("))]

cc6 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")"))]  (Terminal ")")), (LRItem (NonTerminal "Pair") [StackTop, Token (Term (Terminal "(")), Token (Term (Terminal ")"))]  (Terminal ")")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), StackTop, Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")"))]  (Terminal ")")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), StackTop, Token (Term (Terminal ")"))]  (Terminal ")"))]

cc7 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (Term (Terminal ")")), StackTop]  (Terminal "eof")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (Term (Terminal ")")), StackTop]  (Terminal "("))]

cc8 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")")), StackTop]  (Terminal "eof")), (LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")")), StackTop]  (Terminal "("))]

cc9 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")),Token (NonTerm (NonTerminal "Pair")), StackTop,Token (Term (Terminal ")"))]  (Terminal ")"))]

cc10 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (Term (Terminal ")")), StackTop]  (Terminal ")"))]

cc11 = Data.Set.fromList [(LRItem (NonTerminal "Pair") [Token (Term (Terminal "(")), Token (NonTerm (NonTerminal "Pair")), Token (Term (Terminal ")")), StackTop]  (Terminal ")"))]

ccs =  (CC [cc0, cc3, cc2, cc1, cc6, cc7, cc5, cc4, cc10, cc9, cc8, cc11])

(ccs', trans) = createComprehensiveCC caseATerminals caseAProductions caseALRItem

spec :: Spec
spec = do
  describe "createComprehensiveCC" $ do
    it "Should create all the CC for case A" $ do ccs' `shouldBe` ccs
