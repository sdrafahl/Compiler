{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.LeftFactoring where

import Data.Set
import Data.List
import Parser.CFG

leftFactorCFG :: CFG -> CFG
leftFactorCFG cfg =
  let (newProductions :: [ProductionRule]) = searchProductionsToRefactor (getProductionsFromCFG cfg)
      (newNonTerminals :: Set NonTerminal) = Data.Set.fromList (Data.List.map (\(ProductionRule (p, _)) -> p) newProductions)
      (oldTerminals :: Set Terminal) = getTerminals cfg
      (oldStartSymbol :: StartSymbol) = getStartSymbol cfg
  in  (CFG newNonTerminals oldTerminals (Data.Set.fromList newProductions) oldStartSymbol)
      

searchProductionsToRefactor :: [ProductionRule] -> [ProductionRule]
searchProductionsToRefactor prodRules =
  let (prodRulesToRefactor :: [[ProductionRule]]) = Data.List.groupBy (\(ProductionRule (a1, b1:b1s)) (ProductionRule (a2, b2:b2s)) -> a1 == a2 && b2 == b1) prodRules
      (refactoredGroups :: [[ProductionRule]]) = Data.List.map refactorProductionSets prodRulesToRefactor
  in intercalate [] refactoredGroups

createRefactoredProduction :: NonTerminal -> [NonTerminalOrTerminal] -> ProductionRule
createRefactoredProduction parent [h] = (ProductionRule (parent, [(Term (Terminal "δ"))]))
createRefactoredProduction parent children = (ProductionRule (parent, Data.List.tail children))
  
refactorProductionSets :: [ProductionRule] -> [ProductionRule]
refactorProductionSets ((ProductionRule (a1, (b1:b1x))): xs) =
  let (newRefactoredTerminal :: NonTerminal) = (NonTerminal ((getNameOfNonTerminal a1) ++ "'"))
      (newParentProductionRule :: ProductionRule) = (ProductionRule (a1, [b1, (NonTerm newRefactoredTerminal)]))
      (mapFunction :: [NonTerminalOrTerminal] -> ProductionRule) = createRefactoredProduction newRefactoredTerminal
      (children :: [[NonTerminalOrTerminal]]) = Data.List.map (\(ProductionRule (a1, (b1:b1x))) -> b1:b1x) ((ProductionRule (a1, (b1:b1x))): xs)
      (newChildProductions :: [ProductionRule]) = Data.List.map mapFunction children
  in  newChildProductions ++ [newParentProductionRule]
                
  
  
