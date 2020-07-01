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
import Debug.Trace
import Data.Maybe
import Parser.Follow
import Parser.FirstPlus

data TokenOrPlaceholder = Token NonTerminalOrTerminal | StackTop deriving (Eq, Ord, Show)

data LRItem = LRItem {a :: NonTerminal, b :: [TokenOrPlaceholder], lookahead :: Terminal} deriving (Eq, Ord, Show)

splitAtContext :: [TokenOrPlaceholder] -> ([TokenOrPlaceholder], [TokenOrPlaceholder])
splitAtContext tokens =
  let (Just index) = (elemIndex StackTop tokens)
  in  Data.List.splitAt index tokens

closure :: Set Terminal -> Set ProductionRule -> Set LRItem -> Set LRItem
closure terms prods lritems =
  let (fixedPointAlgo :: Set LRItem -> Set LRItem) = fixedPointAlgorithm terms prods
  in  fixPointOperation lritems fixedPointAlgo

fixedPointAlgorithm :: Set Terminal -> Set ProductionRule -> Set LRItem -> Set LRItem
fixedPointAlgorithm terms prods lritem =
  let (foldOverLrItemsAlgo :: Set LRItem -> LRItem -> Set LRItem) = foldOverLrItems prods terms
  in  Data.Set.foldl' foldOverLrItemsAlgo lritem lritem
      
instance FixedPointAlgorithm (Set LRItem) where
  fixPointOperation lrItemSet closureOperation =
    let lrItemSet' = closureOperation lrItemSet
    in  case lrItemSet' == lrItemSet of
      True -> lrItemSet'
      False -> fixPointOperation lrItemSet' closureOperation

foldOverLrItems :: Set ProductionRule -> Set Terminal -> Set LRItem -> LRItem -> Set LRItem
foldOverLrItems prods terms accLrItem (LRItem a b l) =
  let ((leftOfStack :: [TokenOrPlaceholder]),(rightWithStack :: [TokenOrPlaceholder])) = splitAtContext b
      (rightOfStack :: [TokenOrPlaceholder]) = Data.List.tail rightWithStack
      (rightOfC :: [TokenOrPlaceholder]) = case rightOfStack of
        [_] -> []
        _ -> Data.List.tail rightOfStack
      (Token headOfRightOfStack) = Data.List.head rightOfStack
  in  case headOfRightOfStack of
        (Term _) -> accLrItem
        (NonTerm nonTerm) ->
          let (isCProductionFilter :: ProductionRule -> Bool) = isCProduction nonTerm
              (cProductions :: Set ProductionRule) = Data.Set.filter isCProductionFilter prods
              (first :: First) = createFirstMap prods terms
              (foldOverProductionsAlgo :: Set LRItem -> ProductionRule -> Set LRItem) = foldOverProductions rightOfC l first (LRItem a b l)
          in  Data.Set.foldl' foldOverProductionsAlgo accLrItem cProductions
              

isCProduction :: NonTerminal -> ProductionRule -> Bool
isCProduction nonTerm (ProductionRule (p, _)) = p == nonTerm

foldOverProductions :: [TokenOrPlaceholder] -> Terminal -> First -> LRItem -> Set LRItem -> ProductionRule -> Set LRItem
foldOverProductions righOfC a first lrItem lrItems prodRule =
  let (righOfC' :: [NonTerminalOrTerminal]) = Data.List.map (\(Token u) -> u) righOfC
      (firstSet :: Set Terminal) = getFirst' a righOfC' first
      (foldOverTerminalsAlgo :: Set LRItem -> Terminal -> Set LRItem) = foldOverTerminals prodRule
  in Data.Set.foldl' foldOverTerminalsAlgo lrItems firstSet


foldOverTerminals :: ProductionRule -> Set LRItem -> Terminal -> Set LRItem
foldOverTerminals (ProductionRule (a, b)) accLrItem term =
  let (childrenTokens :: [TokenOrPlaceholder]) = Data.List.map (\b' -> Token b') b 
  in  Data.Set.insert (LRItem a (StackTop:childrenTokens) term) accLrItem
      
getFirst' :: Terminal -> [NonTerminalOrTerminal] -> First -> Set Terminal
getFirst' a [] _ = Data.Set.fromList [a]
getFirst' a nonTermOrTerms first
  | Data.Set.member (Terminal "δ") firstOfDelta = Data.Set.insert a firstOfDelta
  | otherwise = firstOfDelta
  where
    (firstOfDelta :: Set Terminal) = getFirstForAllChildren nonTermOrTerms first 
