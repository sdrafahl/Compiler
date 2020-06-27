{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Parser.LRItem where

import RecursiveAlgorithms.FixedPoint
import Parser.CFG
import Parser.First
import Data.Set
import Data.List

data TokenOrPlaceholder = Token NonTerminalOrTerminal | StackTop deriving (Eq, Ord, Show)

data LRItem = LRItem {a :: NonTerminal, b :: [TokenOrPlaceholder], lookahead :: Terminal} deriving (Eq, Ord, Show)

closure :: Set Terminal -> Set ProductionRule -> Set LRItem -> Set LRItem
closure terms prods lritems =
  let (fixedPointAlgo :: Set LRItem -> Set LRItem) = fixedPointAlgorithm terms prods
  in  fixPointOperation lritems fixedPointAlgo

fixedPointAlgorithm ::  Set Terminal -> Set ProductionRule -> Set LRItem -> Set LRItem
fixedPointAlgorithm terms prods lritem =
  let (foldOverLrItems' :: Set LRItem -> LRItem -> Set LRItem) = foldOverLrItems terms prods
  in  Data.Set.foldl' foldOverLrItems' lritem lritem 
      
instance FixedPointAlgorithm (Set LRItem) where
  fixPointOperation lrItemSet closureOperation =
    let lrItemSet' = closureOperation lrItemSet
    in  case lrItemSet' == lrItemSet of
      True -> lrItemSet'
      False -> fixPointOperation lrItemSet' closureOperation

dropBeforeContext :: [TokenOrPlaceholder] -> [TokenOrPlaceholder]
dropBeforeContext tokens =
  let (Just index) = (elemIndex StackTop tokens)
  in  Data.List.drop (index - 1) tokens
  

isC :: [TokenOrPlaceholder] -> Bool
isC [(Token (NonTerm _))] = True
isC _ = False

isProductionGroupHead :: NonTerminal -> [ProductionRule] -> Bool
isProductionGroupHead nonTerm prodRules =
  let (ProductionRule (a, _)) = Data.List.head prodRules
  in  a == nonTerm
      
groupProductionRulesByParent :: [ProductionRule] -> NonTerminal -> [[ProductionRule]]
groupProductionRulesByParent prodRules p =
  let isProductionGroupHead' = isProductionGroupHead p
  in Data.List.filter isProductionGroupHead' (Data.List.groupBy (\(ProductionRule (a, b)) (ProductionRule (a1, b1)) -> a == a1) prodRules)

foldOverLrItems :: Set Terminal -> Set ProductionRule -> Set LRItem -> LRItem -> Set LRItem
foldOverLrItems terminals prodRules setOfLrItems lrItem =
  let (lrItemAndChildren :: [[TokenOrPlaceholder]]) = Data.List.map (\(LRItem a b lookahead) -> dropBeforeContext b) (Data.Set.toList setOfLrItems)
      (lrItemAndChildrenWithC :: [[TokenOrPlaceholder]]) = Data.List.filter (\a -> isC a) lrItemAndChildren
      (lrItemAndChildrenWithC' :: [TokenOrPlaceholder]) = Data.List.map (\b -> head b) lrItemAndChildrenWithC
      (listOfproductions :: [ProductionRule]) = Data.Set.toList prodRules
      (cProductions :: [ProductionRule]) = Data.List.filter (\(ProductionRule (a, bs)) -> Data.List.elem (Token (NonTerm a)) lrItemAndChildrenWithC') listOfproductions
      (theFirsts :: Set Terminal) = getFirst' prodRules terminals
      (foldOverProductionsAlgorithm :: Set LRItem -> ProductionRule -> Set LRItem) = foldOverProductions theFirsts
  in  Data.List.foldl' foldOverProductionsAlgorithm setOfLrItems cProductions 

foldOverProductions :: Set Terminal -> Set LRItem -> ProductionRule -> Set LRItem
foldOverProductions terms lr prod =
  let (foldOverTerminalsAlgorithm :: (Set LRItem) -> Terminal -> (Set LRItem)) = foldOverTerminals prod
  in  Data.List.foldl' foldOverTerminalsAlgorithm lr terms

foldOverTerminals :: ProductionRule -> Set LRItem -> Terminal -> Set LRItem
foldOverTerminals (ProductionRule (a, b)) acc term =
  let (b' :: [TokenOrPlaceholder]) = Data.List.map (\nonTerminalOrTerminal -> Token nonTerminalOrTerminal) b
      (newSet :: Set LRItem) = Data.Set.fromList [(LRItem a (StackTop:b') term)]
  in Data.Set.union acc newSet
      
     
getFirst' :: Set ProductionRule -> Set Terminal -> Set Terminal
getFirst' prodRules terminals' =
  let (firstMap :: First) = createFirstMap prodRules terminals'
      (nonTermHeads :: [NonTerminalOrTerminal]) = Data.Set.toList (Data.Set.map (\(ProductionRule (a, _)) -> NonTerm a) prodRules)
  in  getFirstForAllChildren nonTermHeads firstMap

