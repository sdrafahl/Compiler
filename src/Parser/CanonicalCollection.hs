{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Parser.CanonicalCollection where

import Data.Set
import Data.List
import Data.Map
import Parser.LRItem
import Parser.CFG
import RecursiveAlgorithms.FixedPoint
import Parser.Goto
import Debug.Trace
import Data.Maybe

data CC = CC [Set LRItem] deriving (Eq, Ord, Show)
data Transitions = Transitions (Map (NonTerminalOrTerminal, Set LRItem) (Set LRItem)) deriving (Eq, Ord, Show)
data Marked = Marked (Set (Set LRItem)) deriving (Eq, Ord, Show)

isMarked :: Set LRItem -> Marked -> Bool
isMarked lrItem (Marked s) = Data.Set.member lrItem s

findCC :: CC -> Set LRItem -> Maybe Int
findCC (CC lrItems) lrItem = Data.List.findIndex (\lrItem' -> lrItem' == lrItem) lrItems
  
mark :: Set LRItem -> Marked -> Marked
mark lrItem (Marked s) = Marked (Data.Set.insert lrItem s)

record :: (NonTerminalOrTerminal, Set LRItem) -> Set LRItem -> Transitions -> Transitions
record key val (Transitions m) = (Transitions (Data.Map.insert key val m))

getGoalLrItem :: CFG -> LRItem
getGoalLrItem (CFG _ _ prods startSymbol') =
  let (ProductionRule (a', b')) = Data.List.head (Data.Set.toList (Data.Set.filter (\(ProductionRule (a, b)) -> a == startSymbol') prods))
      (b'' :: [TokenOrPlaceholder]) = Data.List.map (\t -> (Token t)) b'
      (lrItem :: LRItem) = (LRItem a' b'' (Terminal "eof"))
  in  lrItem
      
createComprehensiveCC :: Set Terminal -> Set ProductionRule -> LRItem -> (CC, Transitions)
createComprehensiveCC terms prods goalLrItem =
  let (cc0 :: Set LRItem) = closure terms prods (Data.Set.fromList [goalLrItem])
      (initCC :: CC) = (CC [cc0])
      (initTransitions :: Transitions) = (Transitions Data.Map.empty)
      (initMarked :: Marked) = (Marked Data.Set.empty)
      (ccFixedPointAlgorithm' :: ((Marked,CC, Transitions) -> (Marked,CC, Transitions))) = ccFixedPointAlgorithm prods terms
      (_, (CC lrs), c) = (fixPointOperation (initMarked,initCC, initTransitions) ccFixedPointAlgorithm')
      b' = (CC (Data.List.filter (\st -> (not (Data.List.null st))) lrs))
   in (b', c)
      
instance FixedPointAlgorithm (Marked,CC, Transitions) where
  fixPointOperation i algo =
    let i' = algo i
    in  case i' == i of
          True -> i
          False -> fixPointOperation i' algo

getAllUnMarkedCC :: CC -> Marked -> [Set LRItem]
getAllUnMarkedCC (CC lrItems) marked = Data.List.filter (\l -> not (isMarked l marked)) lrItems

ccFixedPointAlgorithm :: Set ProductionRule -> Set Terminal -> (Marked,CC, Transitions) -> (Marked,CC, Transitions)
ccFixedPointAlgorithm prods terms (marked ,accCC, accTrans) =
  let (unMarkedccs :: [Set LRItem]) = getAllUnMarkedCC accCC marked
      (marked'' :: Marked) = (Data.List.foldl' (\marked' cci' -> mark cci' marked') marked unMarkedccs)
      (foldOverUnMarked' :: (CC, Transitions) -> Set LRItem -> (CC, Transitions)) = foldOverUnMarked terms prods
      (a', b') = Data.List.foldl' foldOverUnMarked' (accCC, accTrans) unMarkedccs
   in (marked'' ,a', b')
      
foldOverUnMarked :: Set Terminal -> Set ProductionRule -> (CC, Transitions) -> Set LRItem -> (CC, Transitions)
foldOverUnMarked terms prods (accCC, trans) cci =
  let (everyXFollowingStackTop :: Set [NonTerminalOrTerminal]) = (Data.Set.map takeEveryTokenAfterStackTop cci)
      (foldOverCCi' :: (CC, Transitions) -> [NonTerminalOrTerminal] -> (CC, Transitions)) = foldOverCCi prods terms cci
  in  Data.Set.foldl' foldOverCCi' (accCC, trans) everyXFollowingStackTop

  
foldOverCCi :: Set ProductionRule -> Set Terminal -> Set LRItem -> (CC, Transitions) -> [NonTerminalOrTerminal] -> (CC, Transitions)
foldOverCCi prods terms cci ((CC lritems), trans) xs =
  let (foldOverxs' :: (CC, Transitions) -> NonTerminalOrTerminal -> (CC, Transitions)) = foldOverxs prods terms cci
  in  Data.List.foldl' foldOverxs' ((CC lritems), trans) xs
  
  
foldOverxs :: Set ProductionRule -> Set Terminal -> Set LRItem -> (CC, Transitions) -> NonTerminalOrTerminal -> (CC, Transitions)
foldOverxs prods terms cci ((CC lritems), trans) x =
  let (temp :: Set LRItem) =  goto prods terms cci x
      (acc' :: CC) = case Data.List.elem temp lritems of
                      False -> (CC (lritems ++ [temp]))
                      True -> (CC lritems)
      (tm' :: Transitions) = (record (x, temp) temp trans)
  in  (acc', tm')
  
