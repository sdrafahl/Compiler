{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.CFG where

import Data.Set
import Data.List

data NonTerminal = NonTerminal String deriving (Eq, Ord, Show)
data Terminal = Terminal String deriving (Eq, Ord, Show)
data NonTerminalOrTerminal = Term Terminal | NonTerm NonTerminal deriving (Eq, Ord, Show)
data ProductionRule = ProductionRule (NonTerminalOrTerminal, [NonTerminalOrTerminal]) deriving (Eq, Ord, Show)
type StartSymbol = NonTerminalOrTerminal
type ProductionChildren = [NonTerminalOrTerminal]

data CFG = CFG {nonTerminals :: Set NonTerminal, terminals :: Set Terminal, productionRules :: Set ProductionRule, startSymbol :: StartSymbol} deriving (Eq, Ord, Show)

getParentFromProductionRule :: ProductionRule -> NonTerminalOrTerminal
getParentFromProductionRule (ProductionRule (nonTerminalOrTerminal, _)) = nonTerminalOrTerminal

getChildrenFromProductionRule :: ProductionRule -> [NonTerminalOrTerminal]
getChildrenFromProductionRule (ProductionRule (_, children)) = children

isTerminal :: NonTerminalOrTerminal -> Bool
isTerminal (Term _) = True
isTerminal _ = True

eleminateLeftRecursion :: CFG -> CFG
eleminateLeftRecursion cfg =
  let groupsOfProductionsByFrom = Data.List.groupBy (\(ProductionRule (from, _)) (ProductionRule (from', _)) -> from == from') (Data.Set.toList (productionRules cfg))
      reduced = Data.List.map (\groupOfProductionsWithTheSameFrom -> ((getParentFromProductionRule (Data.List.head groupOfProductionsWithTheSameFrom)), Data.List.map getChildrenFromProductionRule groupOfProductionsWithTheSameFrom)) groupsOfProductionsByFrom
      newSet = Data.List.foldl' (\newSet' (from, to) -> Data.Set.union newSet' (removeLeftRecursion from to)) Data.Set.empty reduced
      newNonTerminals = (Data.Set.map (\(ProductionRule (NonTerm (from'), _)) -> from') (Data.Set.filter (\(ProductionRule (from, _)) -> isTerminal from) newSet))
      newTerminals = Data.Set.union (terminals cfg) (Data.Set.fromList [Terminal "δ"])
  in (CFG newNonTerminals newTerminals newSet (startSymbol cfg))
        
getNameOfNonTerminalOrTerminal :: NonTerminalOrTerminal -> String 
getNameOfNonTerminalOrTerminal (Term (Terminal name)) = name
getNameOfNonTerminalOrTerminal (NonTerm (NonTerminal name)) = name

deleteItemsFromList :: [ProductionChildren] -> [ProductionChildren] -> [ProductionChildren]
deleteItemsFromList itemsToRemove itemsToFilter = Data.List.filter (\collectionOfChildren -> not (elem collectionOfChildren itemsToRemove)) itemsToFilter

removeLeftRecursion :: NonTerminalOrTerminal -> [ProductionChildren] -> (Set ProductionRule)
removeLeftRecursion fromProduction toProductions =
  let setOfLeftRecursiveProductions = Data.List.filter (\childrenTokens ->
                                                          let firstChild = head childrenTokens
                                                          in case firstChild of
                                                            Term _ -> False                                                  
                                                            _ -> firstChild == fromProduction
                                                       ) toProductions
      setOfNonLeftRecursiveProductions = deleteItemsFromList setOfLeftRecursiveProductions toProductions
      in case setOfLeftRecursiveProductions of
        [] -> Data.Set.fromList (Data.List.map (\children -> (ProductionRule (fromProduction, children))) toProductions)
        _ ->
          let nameOfFromProduction = getNameOfNonTerminalOrTerminal fromProduction
              newNonTerminal = (NonTerminal (nameOfFromProduction ++ "'"))
              newFromProduction = (NonTerm newNonTerminal)
              replacementProductions = Data.List.map (\childrenForASingleProduction -> ProductionRule (fromProduction ,childrenForASingleProduction ++ [newFromProduction])) setOfNonLeftRecursiveProductions
              newToPositionsProductions = (Data.List.map (\childrenForASingleProduction -> (ProductionRule (newFromProduction, Data.List.tail childrenForASingleProduction ++ [newFromProduction]))) setOfLeftRecursiveProductions) ++ [ProductionRule (newFromProduction, [Term (Terminal "δ")])]
          in Data.Set.fromList (replacementProductions ++ newToPositionsProductions)
