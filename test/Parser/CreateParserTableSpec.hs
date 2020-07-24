{-# LANGUAGE NoImplicitPrelude #-}
module Parser.CreateParserTableSpec (spec) where

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
import Parser.Goto
import Parser.CreateParserTable
import Parser.CanonicalCollection

------------------------------------- case A
-- Goal -> List
-- List -> List Pair
--      -> Pair
-- Pair -> ( Pair )
--      -> ( )
-------------------------------------
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

-- expectedResult = (ActionTable (Data.Map.fromList [((0,Terminal "("),Reduce (NonTerminal "Pair") [Term (Terminal "("),NonTerm (NonTerminal "Pair"),Term (Terminal ")")]),((0,Terminal ")"),Reduce (NonTerminal "Pair") [Term (Terminal "("),NonTerm (NonTerminal "Pair"),Term (Terminal ")")]),((0,Terminal "eof"),Accept),((1,Terminal ")"),Shift 10),((1,Terminal "eof"),Reduce (NonTerminal "Pair") [Term (Terminal "("),NonTerm (NonTerminal "Pair"),Term (Terminal ")")]),((2,Terminal "("),Shift 4),((3,Terminal "("),Shift 4),((4,Terminal "("),Shift 1),((5,Terminal "("),Shift 1),((6,Terminal "("),Shift 1),((7,Terminal "("),Shift 1),((8,Terminal "("),Shift 1)]),GotoTable (Data.Map.fromList [((0,NonTerminal "List"),3),((0,NonTerminal "Pair"),2),((1,NonTerminal "Pair"),6),((3,NonTerminal "Pair"),7),((4,NonTerminal "Pair"),9)]))
goalLrItem = (LRItem (NonTerminal "Goal") [StackTop,Token (NonTerm (NonTerminal "List"))]  (Terminal "eof"))
expectedResult = ((ActionTable (Data.Map.fromList  [((0,Terminal "("),Shift 1), ((1,Terminal "("),Shift 4), ((1,Terminal ")"),Shift 5), ((2,Terminal "("),Reduce (NonTerminal "List") [NonTerm (NonTerminal "Pair")]), ((2,Terminal "eof"),Reduce (NonTerminal "List") [NonTerm (NonTerminal "Pair")]), ((3,Terminal "("),Shift 1),((3,Terminal "eof"),Accept),((4,Terminal "("),Shift 4),((4,Terminal ")"),Shift 8),((5,Terminal "("),Reduce (NonTerminal "Pair") [Term (Terminal "("),(Term (Terminal ")"))]),((5,Terminal "eof"),Reduce (NonTerminal "Pair") [Term (Terminal "("),Term (Terminal ")")]), ((6,Terminal ")"),Shift 10),((7,Terminal "("),Reduce (NonTerminal "List") [NonTerm (NonTerminal "List"),NonTerm (NonTerminal "Pair")]),((7,Terminal "eof"),Reduce (NonTerminal "List") [NonTerm (NonTerminal "List"),NonTerm (NonTerminal "Pair")]), ((8,Terminal ")"),Reduce (NonTerminal "Pair") [Term (Terminal "("),Term (Terminal ")")]),((9,Terminal ")"),Shift 11),((10,Terminal "("),Reduce (NonTerminal "Pair") [Term (Terminal "("),NonTerm (NonTerminal "Pair"),Term (Terminal ")")]), ((10,Terminal "eof"),Reduce (NonTerminal "Pair") [Term (Terminal "("),NonTerm (NonTerminal "Pair"),Term (Terminal ")")]),((11,Terminal ")"),Reduce (NonTerminal "Pair") [Term (Terminal "("),NonTerm (NonTerminal "Pair"),Term (Terminal ")")])]), GotoTable (Data.Map.fromList [((0,NonTerminal "List"),3),((0,NonTerminal "Pair"),2),((1,NonTerminal "Pair"),6),((3,NonTerminal "Pair"),7),((4,NonTerminal "Pair"),9)])))
                                                    
  
spec :: Spec
spec = do
  describe "createTables" $ do
    it "Should create the tables" $ do createTables caseANonTerminals' caseAProductions' caseATerminals' goalLrItem ccs `shouldBe` expectedResult

-- spec :: Spec
-- spec = do
--   describe "createTables" $ do
--     it "Should create the tables" $ do True `shouldBe` True
