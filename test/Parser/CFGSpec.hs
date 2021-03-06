{-# LANGUAGE NoImplicitPrelude #-}
module Parser.CFGSpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
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
  describe "eleminateLeftRecursion" $ do
    it "Should eliminate left recursion from case A" $ eleminateLeftRecursion (CFG (Data.Set.fromList [(NonTerminal "Fee")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B")])  (Data.Set.fromList [(ProductionRule (NonTerminal "Fee", [(NonTerm (NonTerminal "Fee")) ,(Term (Terminal "A"))])), (ProductionRule (NonTerminal "Fee", [ (Term (Terminal "B"))]))]) (NonTerminal "Fee"))
      `shouldBe`
      (CFG (Data.Set.fromList [(NonTerminal "Fee"), (NonTerminal "Fee'")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B"), (Terminal "δ")]) (Data.Set.fromList [ProductionRule (NonTerminal "Fee",[Term (Terminal "B"),NonTerm (NonTerminal "Fee'")]),ProductionRule (NonTerminal "Fee'",[Term (Terminal "A"),NonTerm (NonTerminal "Fee'")]),ProductionRule (NonTerminal "Fee'", [Term (Terminal "\948")])]) (NonTerminal "Fee"))


    it "Should not eliminate direct left recursion where there is none" $ eleminateLeftRecursion (CFG
                                                                            (Data.Set.fromList [(NonTerminal "A"), (NonTerminal "B"), (NonTerminal "C")])
                                                                            (Data.Set.fromList [(Terminal "d"), (Terminal "e")])
                                                                            (Data.Set.fromList [
                                                                                ProductionRule (NonTerminal "A", [NonTerm (NonTerminal "B")]),
                                                                                ProductionRule (NonTerminal "B",[Term (Terminal "C")]),
                                                                                ProductionRule (NonTerminal "C",[NonTerm (NonTerminal "A"), Term (Terminal "d"), Term (Terminal "e")])]
                                                                            )
                                                                            (NonTerminal "A")
                                                                          ) `shouldBe` (CFG
                                                                                        (Data.Set.fromList [(NonTerminal "A"), (NonTerminal "B"), (NonTerminal "C")])
                                                                                        (Data.Set.fromList [(Terminal "d"), (Terminal "e")])
                                                                                        (Data.Set.fromList [
                                                                                            ProductionRule (NonTerminal "A" ,[NonTerm (NonTerminal "B")]),
                                                                                            ProductionRule (NonTerminal "B" ,[Term (Terminal "C")]),
                                                                                            ProductionRule (NonTerminal "C" ,[NonTerm (NonTerminal "A"), Term (Terminal "d"), Term (Terminal "e")])]
                                                                                        )
                                                                                        (NonTerminal "A")
                                                                                       )
  describe "removeIndirectCycles" $ do
    it "Should eliminate left recursion from case A" $ do removeIndirectCycles (CFG (Data.Set.fromList [(NonTerminal "Fee")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B")]) (Data.Set.fromList [(ProductionRule (NonTerminal "Fee", [(NonTerm (NonTerminal "Fee")) ,(Term (Terminal "A"))])), (ProductionRule (NonTerminal "Fee", [(Term (Terminal "B"))]))]) (NonTerminal "Fee"))
      `shouldBe`
      (CFG (Data.Set.fromList [(NonTerminal "Fee"), (NonTerminal "Fee'")]) (Data.Set.fromList [(Terminal "A"), (Terminal "B"), (Terminal "δ")]) (Data.Set.fromList [ProductionRule (NonTerminal "Fee",[Term (Terminal "B"),NonTerm (NonTerminal "Fee'")]), ProductionRule (NonTerminal "Fee'",[Term (Terminal "A"),NonTerm (NonTerminal "Fee'")]), ProductionRule (NonTerminal "Fee'",[Term (Terminal "\948")])]) (NonTerminal "Fee"))
    it "should eliminate left recursion for case B" $ do removeIndirectCycles (CFG (Data.Set.fromList [(NonTerminal "A"), (NonTerminal "B"), (NonTerminal "C")]) (Data.Set.fromList [(Terminal "d"), (Terminal "e")]) (Data.Set.fromList [ProductionRule (NonTerminal "A", [NonTerm (NonTerminal "B")]), ProductionRule (NonTerminal "B",[NonTerm (NonTerminal "C")]), ProductionRule (NonTerminal "C",[NonTerm (NonTerminal "A"), Term (Terminal "d"), Term (Terminal "e")])]) (NonTerminal "A")) `shouldBe` (CFG (Data.Set.fromList [(NonTerminal "A'")]) (Data.Set.fromList [(Terminal "d"), (Terminal "e"), (Terminal "\948")]) (Data.Set.fromList [ProductionRule (NonTerminal "A'", [Term (Terminal "d"), Term (Terminal "e"), NonTerm (NonTerminal "A'")]), ProductionRule (NonTerminal "A'",[Term (Terminal "\948")])]) (NonTerminal "A'"))
  describe "mergeTransition" $ do
    it "Should merge a production correctly for the first case" $ (mergeTransition (ProductionRule (NonTerminal "Fee", [(NonTerm (NonTerminal "B")),(Term (Terminal "x"))])) [ProductionRule (NonTerminal "B", [Term (Terminal "a")]), ProductionRule (NonTerminal "B", [Term (Terminal "c")])]) `shouldBe` [ProductionRule (NonTerminal "Fee",[(Term (Terminal "a")),(Term (Terminal "x"))]), ProductionRule (NonTerminal "Fee", [(Term (Terminal "c")),(Term (Terminal "x"))])]

    it "Should merge a production correctly for the second case" $ (mergeTransition (ProductionRule (NonTerminal "Tee", [(NonTerm (NonTerminal "B")),(Term (Terminal "x")), (NonTerm (NonTerminal "B"))])) [ProductionRule (NonTerminal "B", [NonTerm (NonTerminal "C"), Term (Terminal "a")]), ProductionRule (NonTerminal "B", [(NonTerm (NonTerminal "C"))])]) `shouldBe`
      [
        ProductionRule (NonTerminal "Tee",[NonTerm (NonTerminal "C"),Term (Terminal "a"),Term (Terminal "x"),NonTerm (NonTerminal "C"),Term (Terminal "a")]),
        ProductionRule (NonTerminal "Tee",[NonTerm (NonTerminal "C"),Term (Terminal "a"),Term (Terminal "x"),NonTerm (NonTerminal "C")]),
        ProductionRule (NonTerminal "Tee",[NonTerm (NonTerminal "C"),Term (Terminal "x"),NonTerm (NonTerminal "C"),Term (Terminal "a")]),
        ProductionRule (NonTerminal "Tee",[NonTerm (NonTerminal "C"),Term (Terminal "x"),NonTerm (NonTerminal "C")])
      ]
  describe "mergeProductionPath" $ do
    it "Should merge the productions for the firt case" $ mergeProductionPath
      [
        [ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "B")])],
        [ProductionRule (NonTerminal "B", [Term (Terminal "c")])]
      ]
      []
      `shouldBe`
      [ProductionRule (NonTerminal "Tee",[Term (Terminal "c")])]
  describe "findPathCycles" $ do
    it "Should return a path for first case" $ findPathCycles
      [
        [(NonTerm (NonTerminal "A")), (NonTerm (NonTerminal "F"))]
      ]
      (Data.Set.fromList [ProductionRule (NonTerminal "A",[NonTerm (NonTerminal "F")]), ProductionRule (NonTerminal "F",[NonTerm (NonTerminal "A")])])
      `shouldBe`
      [
        (NonTerm (NonTerminal "A")),
        (NonTerm (NonTerminal "F")),
        (NonTerm (NonTerminal "A"))
      ]
    it "Should return a path for second case" $ findPathCycles
      [
        [(NonTerm (NonTerminal "A")), (NonTerm (NonTerminal "F"))]
      ]
      (Data.Set.fromList [
          ProductionRule (NonTerminal "A",[NonTerm (NonTerminal "F")]),
          ProductionRule (NonTerminal "F",[NonTerm (NonTerminal "C")]),
          ProductionRule (NonTerminal "F",[NonTerm (NonTerminal "D")]),
          ProductionRule (NonTerminal "C",[NonTerm (NonTerminal "A")]),
          ProductionRule (NonTerminal "D",[NonTerm (NonTerminal "A")])
          ]
      )
      `shouldBe`
      [NonTerm (NonTerminal "A"),NonTerm (NonTerminal "F"),NonTerm (NonTerminal "C"),NonTerm (NonTerminal "A")]

    it "Should return a path for third case if there are not paths" $ findPathCycles
      [
        [(NonTerm (NonTerminal "A")), (NonTerm (NonTerminal "F"))]
      ]
      (Data.Set.fromList [
          ProductionRule (NonTerminal "F",[NonTerm (NonTerminal "C")])
          ]
      )
      `shouldBe`
      []
    it "Should return a path for a fourth case" $ findPathCycles
      [
        [(NonTerm (NonTerminal "A")), (NonTerm (NonTerminal "B"))]
      ]
      (Data.Set.fromList [
          ProductionRule (NonTerminal "A",[NonTerm (NonTerminal "B")]),
          ProductionRule (NonTerminal "B",[NonTerm (NonTerminal "C")]),
          ProductionRule (NonTerminal "C",[NonTerm (NonTerminal "A"), Term (Terminal "d"), Term (Terminal "e")])
          ]
      )
      `shouldBe`
      [NonTerm (NonTerminal "A"), NonTerm (NonTerminal "B"), NonTerm (NonTerminal "C"), NonTerm (NonTerminal "A")]
    it "Should return a path given many routes" $ findPathCycles [[NonTerm (NonTerminal "A"),NonTerm (NonTerminal "B")],[NonTerm (NonTerminal "B"),NonTerm (NonTerminal "C")],[NonTerm (NonTerminal "C"),NonTerm (NonTerminal "A")]] (Data.Set.fromList [ProductionRule (NonTerminal "A",[NonTerm (NonTerminal "B")]), ProductionRule (NonTerminal "B",[NonTerm (NonTerminal "C")]), ProductionRule (NonTerminal "C",[NonTerm (NonTerminal "A"), Term (Terminal "d"), Term (Terminal "e")])]) `shouldBe` [NonTerm (NonTerminal "A"), NonTerm (NonTerminal "B"), NonTerm (NonTerminal "C"), NonTerm (NonTerminal "A")]
  describe "convertPathToProductions" $ do
    it "Should convert basic path to list of production" $ convertPathToProductions
        [(NonTerm (NonTerminal "A")), (NonTerm (NonTerminal "B")), (NonTerm (NonTerminal "C"))] (Data.Set.fromList [ProductionRule (NonTerminal "A",[NonTerm (NonTerminal "B")]), ProductionRule (NonTerminal "B",[NonTerm (NonTerminal "C")])]) [] `shouldBe` [[ProductionRule (NonTerminal "A",[NonTerm (NonTerminal "B")])],[ProductionRule (NonTerminal "B",[NonTerm (NonTerminal "C")])]]
  
  describe "getProductions" $ do
    describe "Should get the productions from a set of provided ones where it starts with the left one in the tuple and the first child is the second tuple" $ do
      it "should passs for case A" $ do getProductions (NonTerminal "Tee", NonTerm (NonTerminal "Tee1")) (Data.Set.fromList [(ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "Tee1")]))]) `shouldBe` [(ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "Tee1")]))]
      it "should passs for case B" $ do getProductions (NonTerminal "Tee", NonTerm (NonTerminal "Tee1")) (Data.Set.fromList [(ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "Tee1"), NonTerm (NonTerminal "Tee1")])), (ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "Te"), NonTerm (NonTerminal "Tee1")])), (ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "Tee1")]))]) `shouldBe`
                                            [
                                              (ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "Tee1")])),
                                              (ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "Tee1"), NonTerm (NonTerminal "Tee1")]))
                                            ]
  describe "getGroupedProductions" $ do
    describe "Should group the productions from the parent token" $ do
      it "should pass for case A" $ do getGroupedProductions (Data.Set.fromList
                                                                [
                                                                  ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "A")]),
                                                                  ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "B")]),
                                                                  ProductionRule (NonTerminal "Tee", [NonTerm (NonTerminal "C")]),
                                                                  ProductionRule (NonTerminal "Tee1", [NonTerm (NonTerminal "C")])
                                                                ]) `shouldBe` [
                                           (NonTerminal "Tee", [
                                               [NonTerm (NonTerminal "A")],
                                               [NonTerm (NonTerminal "B")],
                                               [NonTerm (NonTerminal "C")]
                                               ]),
                                           (NonTerminal "Tee1", [
                                               [NonTerm (NonTerminal "C")]
                                               ])
                                           ]
  describe "doesChildProductionHaveANonTerminalAtTheHead" $ do
    it "should evaluate to false for empty list" $ do doesChildProductionHaveANonTerminalAtTheHead [] `shouldBe` False
    it "should evaluate to true with a non empty list with terminal at the head" $ do doesChildProductionHaveANonTerminalAtTheHead [(NonTerm (NonTerminal "Tee")), (Term (Terminal "A"))] `shouldBe` True
    it "should evaluate to false with a non empty list with terminal not at the head" $ do doesChildProductionHaveANonTerminalAtTheHead [(Term (Terminal "A")), (NonTerm (NonTerminal "Tee"))] `shouldBe` False
    
  describe "findACycleFromPathAndChildren" $ do
    it "should not find a cycle" $ do findACycleFromPathAndChildren [
                                        ([NonTerm (NonTerminal "A"), NonTerm (NonTerminal "B")], Data.Set.fromList [[NonTerm (NonTerminal "C")]]),
                                        ([NonTerm (NonTerminal "B"), NonTerm (NonTerminal "C")], Data.Set.fromList [[NonTerm (NonTerminal "A"), Term (Terminal "d"), Term (Terminal "e")]]),
                                        ([NonTerm (NonTerminal "C"), NonTerm (NonTerminal "A")], Data.Set.fromList [[NonTerm (NonTerminal "B")]])
                                        ]
                                        `shouldBe` Nothing
    it "should find a cycle" $ do findACycleFromPathAndChildren [
                                        ([NonTerm (NonTerminal "A"), NonTerm (NonTerminal "B"), NonTerm (NonTerminal "C")], Data.Set.fromList [[NonTerm (NonTerminal "A"), Term (Terminal "d"), Term (Terminal "e")]]),
                                        ([NonTerm (NonTerminal "B"), NonTerm (NonTerminal "C"), NonTerm (NonTerminal "A")], Data.Set.fromList [[NonTerm (NonTerminal "B")]]),
                                        ([NonTerm (NonTerminal "C"), NonTerm (NonTerminal "A"), NonTerm (NonTerminal "B")], Data.Set.fromList [[NonTerm (NonTerminal "C")]])
                                        ]
                                        `shouldBe` Just ([NonTerm (NonTerminal "A"),NonTerm (NonTerminal "B"),NonTerm (NonTerminal "C")], Data.Set.fromList [[NonTerm (NonTerminal "A"),Term (Terminal "d"),Term (Terminal "e")]])
   
