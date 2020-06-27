{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.CFG where

import Data.Set
import Data.List
import Data.Map
import Debug.Trace

data NonTerminal = NonTerminal String deriving (Eq, Ord, Show)
data Terminal = Terminal String deriving (Eq, Ord, Show)
data NonTerminalOrTerminal = Term Terminal | NonTerm NonTerminal deriving (Eq, Ord, Show)
data ProductionRule = ProductionRule (NonTerminal, [NonTerminalOrTerminal]) deriving (Eq, Ord, Show)
type StartSymbol = NonTerminalOrTerminal
type ProductionChildren = [NonTerminalOrTerminal]
type Path = [NonTerminalOrTerminal]

data CFG = CFG {nonTerminals :: Set NonTerminal, terminals :: Set Terminal, productionRules :: Set ProductionRule, startSymbol :: StartSymbol} deriving (Eq, Ord, Show)

getNameOfNonTerminal :: NonTerminal -> String
getNameOfNonTerminal (NonTerminal n) = n

getParentFromProductionRule :: ProductionRule -> NonTerminal
getParentFromProductionRule (ProductionRule (nonTerminalOrTerminal, _)) = nonTerminalOrTerminal

getChildrenFromProductionRule :: ProductionRule -> [NonTerminalOrTerminal]
getChildrenFromProductionRule (ProductionRule (_, children)) = children

getTerminals :: CFG -> Set Terminal
getTerminals (CFG _ terminals _ _) = terminals

getStartSymbol :: CFG -> StartSymbol
getStartSymbol (CFG _ _ _ start) = start

getProductionsFromCFG :: CFG -> [ProductionRule]
getProductionsFromCFG (CFG _ _ prodRules _) = Data.Set.toList prodRules

isTerminal :: NonTerminalOrTerminal -> Bool
isTerminal (Term (Terminal _)) = True
isTerminal _ = False

getValueFromNonTermOrTerminal :: NonTerminalOrTerminal -> String
getValueFromNonTermOrTerminal (Term (Terminal a)) = a
getValueFromNonTermOrTerminal (NonTerm (NonTerminal a)) = a

eleminateLeftRecursion :: CFG -> CFG
eleminateLeftRecursion cfg =
  let groupsOfProductionsByFrom = (Data.List.groupBy (\(ProductionRule (from, _)) (ProductionRule (from', _)) -> from == from') (Data.Set.toList (productionRules cfg)))
      reduced = (Data.List.map (\groupOfProductionsWithTheSameFrom -> ((getParentFromProductionRule (Data.List.head groupOfProductionsWithTheSameFrom)), Data.List.map getChildrenFromProductionRule groupOfProductionsWithTheSameFrom)) groupsOfProductionsByFrom)
      newSet = (Data.List.foldl' (\newSet' (from, to) -> Data.Set.union newSet' (removeLeftRecursion from to)) Data.Set.empty reduced)
      newNonTerminals = (Data.Set.map (\(ProductionRule (from', _)) -> from') newSet)
      newStartingSymbol =  case (Data.Set.member (startSymbol cfg) (Data.Set.map (\nonTerm -> NonTerm nonTerm) newNonTerminals)) of
                             True -> (startSymbol cfg)
                             _ ->
                               let valueOfStarting = getValueFromNonTermOrTerminal (startSymbol cfg)
                               in  (NonTerm (NonTerminal (valueOfStarting ++ "'")))
  in case newSet == (productionRules cfg) of
       False ->
         let newTerminals = Data.Set.union (terminals cfg) (Data.Set.fromList [Terminal "δ"])
         in (CFG newNonTerminals newTerminals newSet newStartingSymbol)
       True -> cfg
        
getNameOfNonTerminalOrTerminal :: NonTerminalOrTerminal -> String 
getNameOfNonTerminalOrTerminal (Term (Terminal name)) = name
getNameOfNonTerminalOrTerminal (NonTerm (NonTerminal name)) = name

deleteItemsFromList :: [ProductionChildren] -> [ProductionChildren] -> [ProductionChildren]
deleteItemsFromList itemsToRemove itemsToFilter = Data.List.filter (\collectionOfChildren -> not (elem collectionOfChildren itemsToRemove)) itemsToFilter

removeLeftRecursion :: NonTerminal -> [ProductionChildren] -> (Set ProductionRule)
removeLeftRecursion fromProduction toProductions =
  let setOfLeftRecursiveProductions = Data.List.filter (\childrenTokens ->
                                                          let firstChild = head childrenTokens
                                                          in case firstChild of
                                                            Term _ -> False                                                  
                                                            (NonTerm nonTerminal') -> nonTerminal' == fromProduction
                                                       ) toProductions
      setOfNonLeftRecursiveProductions = deleteItemsFromList setOfLeftRecursiveProductions toProductions
      in case setOfLeftRecursiveProductions of
        [] -> Data.Set.fromList (Data.List.map (\children -> (ProductionRule (fromProduction, children))) toProductions)
        _ ->
          let nameOfFromProduction = getNameOfNonTerminalOrTerminal (NonTerm fromProduction)
              newNonTerminal = (NonTerminal (nameOfFromProduction ++ "'"))
              replacementProductions = Data.List.map (\(childrenForASingleProduction :: ProductionChildren) -> ProductionRule (fromProduction , childrenForASingleProduction ++ [(NonTerm newNonTerminal)])) setOfNonLeftRecursiveProductions
              newToPositionsProductions = (Data.List.map (\childrenForASingleProduction -> (ProductionRule (newNonTerminal, Data.List.tail childrenForASingleProduction ++ [NonTerm newNonTerminal]))) setOfLeftRecursiveProductions) ++ [ProductionRule (newNonTerminal, [Term (Terminal "δ")])]
          in Data.Set.fromList (replacementProductions ++ newToPositionsProductions)


getSetOfProductionForGivenFromToken :: NonTerminalOrTerminal -> Set ProductionRule -> Set ProductionRule
getSetOfProductionForGivenFromToken fromToken setOfAllProductionRules = Data.Set.filter (\(ProductionRule (nonTerminalOrTerminal, _)) -> equals nonTerminalOrTerminal fromToken) setOfAllProductionRules

doesChildProductionHaveAtTheHead :: NonTerminalOrTerminal -> [NonTerminalOrTerminal] -> Bool
doesChildProductionHaveAtTheHead  _ [] = False
doesChildProductionHaveAtTheHead token (x:xs) = x == token

doesChildProductionHaveANonTerminalAtTheHead :: [NonTerminalOrTerminal] -> Bool
doesChildProductionHaveANonTerminalAtTheHead [] = False
doesChildProductionHaveANonTerminalAtTheHead ((Term _):xs) = False 
doesChildProductionHaveANonTerminalAtTheHead _ = True

getGroupedProductions :: Set ProductionRule -> [(NonTerminal, [[NonTerminalOrTerminal]])]
getGroupedProductions productionRulesSet =
  Data.Map.toList (Data.Set.foldl (\mapOfProductions (ProductionRule (from, to)) ->
                     let searchResult = Data.Map.lookup from mapOfProductions
                     in case searchResult of
                       Nothing -> Data.Map.insert from [to] mapOfProductions
                       Just listOfChildren ->  Data.Map.insert from (listOfChildren ++ [to]) mapOfProductions
                     ) Data.Map.empty productionRulesSet)

isSomthing :: (Maybe a) -> Bool
isSomthing (Just _) = True
isSomthing _ = False

getProductions :: (NonTerminal, NonTerminalOrTerminal) -> Set ProductionRule -> [ProductionRule]
getProductions (from, to) productionRules =
  Data.Set.toList (Data.Set.filter (\(ProductionRule (from', to')) -> from' == from && to == (head to')) productionRules)
      
convertPathToProductions :: Path -> Set ProductionRule -> [[ProductionRule]] -> [[ProductionRule]]
convertPathToProductions [] _ newListOfProductionsInAPath = newListOfProductionsInAPath
convertPathToProductions path setOfProductions newListOfProductionsInAPath
  | ((Data.List.length path) == 1) = newListOfProductionsInAPath
  | otherwise =
    let (NonTerm firstToken) = head path
        secondToken = head (tail path)
        productionsFromTo = getProductions (firstToken, secondToken) setOfProductions
    in  convertPathToProductions (Data.List.tail path) setOfProductions (newListOfProductionsInAPath ++ [productionsFromTo])

mergeProductionPath :: [[ProductionRule]] -> [ProductionRule] -> [ProductionRule]
mergeProductionPath [] acc = acc
mergeProductionPath productionRulePath [] = mergeProductionPath (Data.List.tail productionRulePath) (Data.List.head productionRulePath)    
mergeProductionPath productionRulePath mergedProductions =
  let firstSetOfProductions = head productionRulePath
      merged = Data.List.foldl' (\productions production -> productions ++ (mergeTransition production firstSetOfProductions)) [] mergedProductions
  in  mergeProductionPath (Data.List.tail productionRulePath) merged
  

removeIndirectCycles :: CFG -> CFG
removeIndirectCycles cfg' =
  let cfg = (eleminateLeftRecursion cfg')
      newCfg = (removeIndirectCycles' cfg)
      cfgWithoutIndirectCycles = case (newCfg == cfg) of
        True -> newCfg
        False -> (removeIndirectCycles newCfg)
  in  eleminateLeftRecursion cfgWithoutIndirectCycles
  
removeIndirectCycles' :: CFG -> CFG
removeIndirectCycles' cfg =
  let (prods :: Set ProductionRule) = (productionRules cfg)
      (basePaths :: [Path]) = (Data.Set.toList (Data.Set.map (\(ProductionRule (from, (children :: [NonTerminalOrTerminal]))) -> [NonTerm from, (Data.List.head children)]) prods))
      cyclePath = (findPathCycles basePaths prods)
      productionRulePaths = (convertPathToProductions cyclePath prods [])
      mergedPath = (mergeProductionPath productionRulePaths [])
      productionsToRemove :: Set ProductionRule = ((Data.Set.fromList (intercalate [] productionRulePaths)))
      productionsRemoved :: Set ProductionRule = (Data.Set.foldl' (\allProductions productionToRemove -> Data.Set.delete productionToRemove allProductions) (productionRules cfg) productionsToRemove)
  in  (CFG (nonTerminals cfg) (terminals cfg) (Data.Set.union productionsRemoved (Data.Set.fromList mergedPath)) (startSymbol cfg))


class SemanticEquals a b where
  equals :: a -> b -> Bool

instance SemanticEquals NonTerminalOrTerminal NonTerminalOrTerminal where
  equals a b = a == b

instance SemanticEquals NonTerminalOrTerminal NonTerminal where
  equals a b = a == (NonTerm b)

instance SemanticEquals NonTerminal NonTerminalOrTerminal where
  equals a b = (NonTerm a) == b

instance SemanticEquals Terminal NonTerminalOrTerminal where
  equals a b = (Term a) == b

instance SemanticEquals NonTerminalOrTerminal Terminal where
  equals a b = a == (Term b)



mergeTransition :: ProductionRule -> [ProductionRule] -> [ProductionRule]
mergeTransition (ProductionRule (from, toTokens)) productionRulesToSubstituteIn =
  let newChildren = Data.List.foldl' (\listOfProds token ->
                      case (isTerminal token) of
                        True ->
                          case listOfProds of
                            [] -> [[token]]
                            _ -> Data.List.map (\(child :: [NonTerminalOrTerminal]) -> child ++ [token]) listOfProds
                        False ->
                          let productionsToExpand = Data.List.filter (\(ProductionRule (from', _)) -> equals from' token) productionRulesToSubstituteIn
                              children = Data.List.map getChildrenFromProductionRule productionsToExpand
                          in  case listOfProds of
                            [] -> children
                            _ -> Data.List.foldl' (\paths path -> paths ++ (Data.List.map (\childrenToAppend -> path ++ childrenToAppend) children)) [] listOfProds
                      ) [] toTokens
  in Data.List.map (\children -> (ProductionRule (from, children))) newChildren     


findACycleFromPathAndChildren :: [(Path, Set [NonTerminalOrTerminal])] -> Maybe ([NonTerminalOrTerminal], Set [NonTerminalOrTerminal])
findACycleFromPathAndChildren listOfPathsAndTheirChild = find (\(pathOfChildren, setOfChildren) -> isSomthing (Data.List.find (\children -> doesChildProductionHaveAtTheHead (head pathOfChildren) children) (Data.Set.toList setOfChildren))) listOfPathsAndTheirChild
          
findPathCycles :: [Path] -> Set ProductionRule -> Path
findPathCycles listOfPaths setOfProductionRules =
  let endsOfThePaths :: [NonTerminalOrTerminal] = Data.List.map (\(path :: [NonTerminalOrTerminal]) -> Data.List.last path) listOfPaths
      newEndSets = Data.List.map (\(setOfProductions :: Set ProductionRule) -> Data.Set.map getChildrenFromProductionRule setOfProductions) (Data.List.map (\(end :: NonTerminalOrTerminal) -> getSetOfProductionForGivenFromToken end setOfProductionRules) endsOfThePaths)
      listOfPathsAndTheirChild = zip listOfPaths newEndSets
      maybeASetAndPath = findACycleFromPathAndChildren listOfPathsAndTheirChild
  in case maybeASetAndPath of
        Nothing ->
          let pathAndChildren = Data.List.map (\(path, setOfChildren) -> (path, Data.Set.toList setOfChildren)) listOfPathsAndTheirChild
              pathsAndTerminalChildren = Data.List.filter (\(path, listOfChildren) ->
                                                             let terminalChildren = Data.List.filter (\child -> doesChildProductionHaveANonTerminalAtTheHead child) listOfChildren
                                                             in  case terminalChildren of
                                                               [] -> False
                                                               _ -> True
                                                     ) pathAndChildren
          in case pathsAndTerminalChildren of
            [] -> []
            _ ->
              let newPaths = Data.List.foldl' (\newPaths' (path, children) -> newPaths' ++ Data.List.map (\child -> (path ++ [head child])) children) [] pathsAndTerminalChildren
              in  findPathCycles newPaths setOfProductionRules
        Just (path, setOfChildren) ->
          let Just childWithSymbol = Data.List.find (\child -> (head child) == (head path)) (Data.Set.toList setOfChildren)
          in  path ++ [head childWithSymbol]
              
