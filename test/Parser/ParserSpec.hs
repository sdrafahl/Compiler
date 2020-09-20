{-# LANGUAGE NoImplicitPrelude #-}
module Parser.ParserSpec (spec) where

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
import Parser.LRItem
import Parser.Parser
import Scanner.Scanner
import Parser.ParserTree

caseAProductions' = (Data.Set.fromList [
                        ProductionRule (NonTerminal "Goal", [NonTerm (NonTerminal "List")]),
                        ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "List"), NonTerm (NonTerminal "Pair")]),
                        ProductionRule (NonTerminal "List", [NonTerm (NonTerminal "Pair")]),
                        ProductionRule (NonTerminal "Pair", [Term (Terminal "["), NonTerm (NonTerminal "Pair"), Term (Terminal "]")]),
                        ProductionRule (NonTerminal "Pair", [Term (Terminal "["), Term (Terminal "]")])])
                    
caseALRItem' = (Data.Set.fromList [LRItem (NonTerminal "Goal") [StackTop, Token (NonTerm (NonTerminal "List"))] (Terminal "eof")])
caseANonTerminals' = (Data.Set.fromList [NonTerminal "List", NonTerminal "Pair", NonTerminal "Goal"])
caseATerminals' = (Data.Set.fromList [Terminal "[", Terminal "]"])
startSymbol' = (NonTerminal "Goal")

cfg = (CFG caseANonTerminals' caseATerminals' caseAProductions' startSymbol')
parser = createParserFromCFG cfg
testInputStream = (InputStream "[]")
expectedStack = SyntaxStack []
(resultStack, _, _, tree) = parse testInputStream parser

spec :: Spec
spec = do
  describe "parse" $ do
    it "Should parse the input and return a stack" $ do resultStack `shouldBe` SyntaxStack [St Dollar,St (StackState 0),Tok (NonTerm (NonTerminal "List")),St (StackState 3)]
    it "Should parse the input and return a parse tree" $ do tree `shouldBe` ParseTreeNode (NonTerminal "List") [ParseTreeNode (NonTerminal "Pair") [Leafe (Terminal "["),Leafe (Terminal "]")]]
    
