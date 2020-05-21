{-# LANGUAGE NoImplicitPrelude #-}
module Parser.CFGSpec (spec) where

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

-------------------- CASE A
-- Fee -> A to Fee -> B Fee'
--     -> B    Fee' -> A Fee'
--                  -> δ
--------------------------

spec :: Spec
spec = do
  -- describe "eleminateLeftRecursion" $ do
  --   it "Should elimate left recursion from case A" $ eleminateLeftRecursion (CFG (Data.Set.fromList [(NonTerminal "Fee")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B")])  (Data.Set.fromList [(ProductionRule (NonTerm (NonTerminal "Fee"), [(NonTerm (NonTerminal "Fee")) ,(Term (Terminal "A"))])), (ProductionRule (NonTerm (NonTerminal "Fee"), [(Term (Terminal "B"))])) ]) (NonTerm (NonTerminal "Fee")))
  --     `shouldBe`
  --     (CFG (Data.Set.fromList [(NonTerminal "Fee"), (NonTerminal "Fee'")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B"), (Terminal "δ")]) (Data.Set.fromList [ProductionRule (NonTerm (NonTerminal "Fee"),[Term (Terminal "B"),NonTerm (NonTerminal "Fee'")]),ProductionRule (NonTerm (NonTerminal "Fee'"),[Term (Terminal "A"),NonTerm (NonTerminal "Fee'")]),ProductionRule (NonTerm (NonTerminal "Fee'"),[Term (Terminal "\948")])]) (NonTerm (NonTerminal "Fee")))
      
  -- describe "removeIndirectCycles" $ do
  --   it "Should elimate left recursion from case A" $ removeIndirectCycles (CFG (Data.Set.fromList [(NonTerminal "Fee")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B")])  (Data.Set.fromList [(ProductionRule (NonTerm (NonTerminal "Fee"), [(NonTerm (NonTerminal "Fee")) ,(Term (Terminal "A"))])), (ProductionRule (NonTerm (NonTerminal "Fee"), [(Term (Terminal "B"))])) ]) (NonTerm (NonTerminal "Fee")))
  --     `shouldBe`
  --     (CFG (Data.Set.fromList [(NonTerminal "Fee"), (NonTerminal "Fee'")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B"), (Terminal "δ")]) (Data.Set.fromList [ProductionRule (NonTerm (NonTerminal "Fee"),[Term (Terminal "B"),NonTerm (NonTerminal "Fee'")]),ProductionRule (NonTerm (NonTerminal "Fee'"),[Term (Terminal "A"),NonTerm (NonTerminal "Fee'")]),ProductionRule (NonTerm (NonTerminal "Fee'"),[Term (Terminal "\948")])]) (NonTerm (NonTerminal "Fee")))
  -- describe "removeIndirectCycles'" $ do
  --   it "Should elimate left recursion from case A" $ removeIndirectCycles' (CFG (Data.Set.fromList [(NonTerminal "Fee")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B")])  (Data.Set.fromList [(ProductionRule (NonTerm (NonTerminal "Fee"), [(NonTerm (NonTerminal "Fee")) ,(Term (Terminal "A"))])), (ProductionRule (NonTerm (NonTerminal "Fee"), [(Term (Terminal "B"))])) ]) (NonTerm (NonTerminal "Fee")))
  --     `shouldBe`
  --     (CFG (Data.Set.fromList [(NonTerminal "Fee")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B")])  (Data.Set.fromList [(ProductionRule (NonTerm (NonTerminal "Fee"), [(NonTerm (NonTerminal "Fee")) ,(Term (Terminal "A"))])), (ProductionRule (NonTerm (NonTerminal "Fee"), [(Term (Terminal "B"))])) ]) (NonTerm (NonTerminal "Fee")))
  describe "mergeTransition" $ do
    it "Should merge a production correctly for the first case" $ (mergeTransition (ProductionRule (NonTerm (NonTerminal "Fee"), [(NonTerm (NonTerminal "B")),(Term (Terminal "x"))])) [ProductionRule ((NonTerm (NonTerminal "B")), [Term (Terminal "a")]), ProductionRule ((NonTerm (NonTerminal "B")), [Term (Terminal "c")])]) `shouldBe` [ProductionRule (NonTerm (NonTerminal "Fee"),[(Term (Terminal "a")),(Term (Terminal "x"))]), ProductionRule (NonTerm (NonTerminal "Fee"), [(Term (Terminal "c")),(Term (Terminal "x"))])]
    it "Should merge a production correctly for the second case" $ (mergeTransition (ProductionRule (NonTerm (NonTerminal "Tee"), [(NonTerm (NonTerminal "B")),(Term (Terminal "x")), (NonTerm (NonTerminal "B"))])) [ProductionRule ((NonTerm (NonTerminal "B")), [NonTerm (NonTerminal "C"), Term (Terminal "a")]), ProductionRule ((NonTerm (NonTerminal "B")), [(NonTerm (NonTerminal "C"))])]) `shouldBe`
      [
        ProductionRule (NonTerm (NonTerminal "Tee"),[NonTerm (NonTerminal "C"),Term (Terminal "a"),Term (Terminal "x"),NonTerm (NonTerminal "C"),Term (Terminal "a")]),
        ProductionRule (NonTerm (NonTerminal "Tee"),[NonTerm (NonTerminal "C"),Term (Terminal "a"),Term (Terminal "x"),NonTerm (NonTerminal "C")]),
        ProductionRule (NonTerm (NonTerminal "Tee"),[NonTerm (NonTerminal "C"),Term (Terminal "x"),NonTerm (NonTerminal "C"),Term (Terminal "a")]),
        ProductionRule (NonTerm (NonTerminal "Tee"),[NonTerm (NonTerminal "C"),Term (Terminal "x"),NonTerm (NonTerminal "C")])
      ]
