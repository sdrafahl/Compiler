{-# LANGUAGE NoImplicitPrelude #-}
module Parser.FollowSpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Map (Map)
import Data.Map
import Data.Set
import Parser.CFG
import Parser.First
import Parser.Follow
import Scanner.RegularExpression

caseAProductions' = (Data.Set.fromList [ProductionRule (NonTerminal "Goal", [NonTerm (NonTerminal "List")]), ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "List"), NonTerm (NonTerminal "Pair")]), ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "Pair")]), ProductionRule (NonTerminal "Pair", [Term (Terminal "("), NonTerm (NonTerminal "Pair"), Term (Terminal ")")]), ProductionRule (NonTerminal "Pair", [Term (Terminal "("), Term (Terminal ")")])])
caseALRItem' = (Data.Set.fromList [LRItem (NonTerminal "Goal") [StackTop, Token (NonTerm (NonTerminal "List"))] (Terminal "eof")])
caseANonTerminals' = (Data.Set.fromList [NonTerminal "List", NonTerminal "Pair", NonTerminal "Goal"])
caseATerminals' = (Data.Set.fromList [Terminal "(", Terminal ")"])
initialAItems' = Data.Set.fromList [(LRItem (NonTerminal "Goal") [StackTop,Token (NonTerm (NonTerminal "List"))]  (Terminal "eof")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "List")),Token (NonTerm (NonTerminal "Pair"))] (Terminal "(")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "List")),Token (NonTerm (NonTerminal "Pair"))] (Terminal "eof")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "Pair"))] (Terminal "(")), (LRItem (NonTerminal "List") [StackTop,Token (NonTerm (NonTerminal "Pair"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (Term (Terminal ")"))] (Terminal "(")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (Term (Terminal ")"))] (Terminal "eof")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (NonTerm (NonTerminal "Pair")),Token (Term (Terminal ")"))] (Terminal "(")), (LRItem (NonTerminal "Pair") [StackTop,Token (Term (Terminal "(")),Token (NonTerm (NonTerminal "Pair")),Token (Term (Terminal ")"))] (Terminal "eof"))]

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

(actionTable, gotoTable) = createTables caseANonTerminals' caseAProductions' caseATerminals' (NonTerm (NonTerminal "Goal")) ccs

regex = Regex ['(', '|', ')']
nfa = convert regex
dfa = convert nfa
dfa' = minimize dfa

getCategory :: Char -> CharCategory
getCategory '(' = "Left Paren"
getCategory ')' = "Right paren"

scanner = convertDFAToScanner getCategory

terminals = Data.Set.fromList [Terminal "(", Terminal ")"]
nonTerminals = Data.Set.fromList [NonTerminal "Goal", NonTerminal "List", NonTerminal "Pair"]
prodRules = Data.Set.fromList [
  ProductionRule ((NonTerminal "Goal"), [NonTerm (NonTerminal "List")]),
  ProductionRule ((NonTerminal "List"), [NonTerm (NonTerminal "List"), NonTerm (NonTerminal "Pair")])
  ]
cfg = (CFG )


spec :: Spec
spec = do
  describe "parse" $ do
    it "Should parse the input and return a stack" $ do parse
    
