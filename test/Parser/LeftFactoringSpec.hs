{-# LANGUAGE NoImplicitPrelude #-}
module Parser.LeftFactoringSpec (spec) where

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
import Parser.LeftFactoring

--------------------------- CFG A
-- A -> + A
--   -> + c A
--   -> +
---------------------------
cfgA = (CFG (Data.Set.fromList [NonTerminal "A"]) (Data.Set.fromList [Terminal "+", Terminal "c"]) (Data.Set.fromList [(ProductionRule (NonTerminal "A", [(Term (Terminal "+")), (NonTerm (NonTerminal "A"))])), (ProductionRule (NonTerminal "A", [(Term (Terminal "+")), (Term (Terminal "c")), (NonTerm (NonTerminal "A"))])),(ProductionRule (NonTerminal "A", [(Term (Terminal "+"))]))]) (NonTerm (NonTerminal "A")))


newNonTerms = (Data.Set.fromList [NonTerminal "A", NonTerminal "A'"])
newTerms = (Data.Set.fromList [Terminal "+", Terminal "c"])
prodRules = (Data.Set.fromList [ProductionRule (NonTerminal "A" , [Term (Terminal "+"),NonTerm (NonTerminal "A'")]), ProductionRule (NonTerminal "A'", [Term (Terminal "c"),NonTerm (NonTerminal "A")]), ProductionRule (NonTerminal "A'", [Term (Terminal "δ")]), ProductionRule (NonTerminal "A'",[NonTerm (NonTerminal "A")])])
startingSymbol = (NonTerm (NonTerminal "A"))
expectedCfgA = (CFG newNonTerms newTerms prodRules startingSymbol)


spec :: Spec
spec = do
  describe "leftFactorCFG" $ do
   it "Should refactor the grammar" $ do leftFactorCFG cfgA `shouldBe` expectedCfgA



