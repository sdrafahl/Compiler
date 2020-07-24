{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Parser.CreateParserTable where

import Data.Map
import Parser.CFG
import Parser.CanonicalCollection
import Data.Set
import Parser.LRItem
import Parser.Goto
import Data.List
import Parser.Goto
import Debug.Trace
import Data.Maybe

data Action = Shift Int | Reduce NonTerminal [NonTerminalOrTerminal] | Accept deriving (Eq, Ord, Show)
data ActionTable = ActionTable (Map (Int, Terminal) Action) deriving (Eq, Ord, Show)
data GotoTable = GotoTable (Map (Int, NonTerminal) Int) deriving (Eq, Ord, Show)

getAction :: (Int, Terminal) -> ActionTable -> Maybe Action
getAction key (ActionTable m) = Data.Map.lookup key m

getGoto :: (Int, NonTerminal) -> GotoTable -> Maybe Int
getGoto key (GotoTable m) = Data.Map.lookup key m

addToActionTable :: (Int, Terminal) -> Action -> ActionTable -> ActionTable
addToActionTable (j, c) action (ActionTable m) = (ActionTable (Data.Map.insert (j, c) action m))

addGoto :: (Int, NonTerminal) -> Int -> GotoTable -> GotoTable
addGoto key value (GotoTable m) = (GotoTable (Data.Map.insert key value m))

createTables :: Set NonTerminal -> Set ProductionRule -> Set Terminal -> LRItem -> CC -> (ActionTable, GotoTable)
createTables nonTerms prods terms start (CC (lritems :: [Set LRItem])) =
  let (foldThroughCC' :: (ActionTable, GotoTable, Int) -> Set LRItem -> (ActionTable, GotoTable, Int)) = foldThroughCC nonTerms prods terms start (CC lritems)
      (at, gt, _) = (Data.List.foldl' foldThroughCC' ((ActionTable Data.Map.empty), (GotoTable Data.Map.empty), 0) (lritems))
  in  (at, gt)

foldThroughCC :: Set NonTerminal -> Set ProductionRule -> Set Terminal -> LRItem -> CC -> (ActionTable, GotoTable, Int) -> Set LRItem -> (ActionTable, GotoTable, Int)
foldThroughCC nonTerms prods terms ss cc (actionTable, gotoTable, index) cci =
  let (foldThroughlrItemsAlgo :: ActionTable -> LRItem -> ActionTable) = foldThroughlrItems cci cc ss prods terms index
      (foldOverNonTerminalsAlgo :: GotoTable -> NonTerminal -> GotoTable) = foldOverNonTerminals cc prods terms cci index
      (at :: ActionTable) = (Data.Set.foldl' foldThroughlrItemsAlgo actionTable cci)
      (gt :: GotoTable) = Data.Set.foldl' foldOverNonTerminalsAlgo gotoTable nonTerms
  in  (at, gt, index + 1)
      
foldThroughlrItems :: Set LRItem -> CC -> LRItem -> Set ProductionRule -> Set Terminal -> Int ->  ActionTable -> LRItem -> ActionTable
foldThroughlrItems  cci cc startSymbol' prods terms index accActionTable lrItem =
  let (a' :: NonTerminal) =  getParentOfLrItem lrItem
      (beforeStack :: [NonTerminalOrTerminal]) = takeEveryTokenBeforeStackTop lrItem
      (b' :: [NonTerminalOrTerminal]) = takeEveryTokenAfterStackTop lrItem
      (c' :: Terminal) = getLookAhead lrItem
      (accActionTable' :: ActionTable) = (addAction cci prods terms startSymbol' index a' beforeStack b' c' cc accActionTable)
  in  accActionTable'

ccContainsccj :: Set ProductionRule -> Set Terminal -> Set LRItem -> [NonTerminalOrTerminal] -> CC -> Maybe Int
ccContainsccj prods terms cci righOfStack cc = 
  let c = Data.List.head righOfStack
  in  case c of
        (Term _) ->
          let (ccj :: Set LRItem) = goto prods terms cci c
              maybeJ = (findCC cc ccj)
          in  maybeJ
        _ -> Nothing

mapListToTokens :: [NonTerminalOrTerminal] -> [TokenOrPlaceholder]
mapListToTokens l = Data.List.map (\t -> Token t) l
                    
addAction :: Set LRItem -> Set ProductionRule -> Set Terminal -> LRItem ->  Int -> NonTerminal -> [NonTerminalOrTerminal] -> [NonTerminalOrTerminal] -> Terminal -> CC -> ActionTable -> ActionTable
addAction cci prods terms (LRItem p c _) i a' b' rightOfStack lookahead' cc actionTable
 | (0 < length rightOfStack) && (isTerminal (Data.List.head rightOfStack)) && (isJust maybeJ) =
     let (Term c') = Data.List.head rightOfStack
         (Just j) = maybeJ
     in  addToActionTable (i, c') (Shift j) actionTable
 | ((Terminal "eof") == lookahead') && (p == a') && (rightOfStack == []) = (addToActionTable (i, (Terminal "eof")) Accept actionTable)
 | rightOfStack == [] = (addToActionTable (i, lookahead') (Reduce a' b') actionTable)
 | otherwise = actionTable
 where (maybeJ :: Maybe Int) = ccContainsccj prods terms cci rightOfStack cc --

foldOverNonTerminals :: CC -> Set ProductionRule -> Set Terminal -> Set LRItem -> Int ->  GotoTable -> NonTerminal -> GotoTable
foldOverNonTerminals cc prods terms cci i gotoTable n =
  let (ccj :: Set LRItem) = goto prods terms cci (NonTerm n)
      (j :: Maybe Int) = findCC cc ccj
  in  case j of
        Just j ->
          let (gotoTable' :: GotoTable) = addGoto (i, n) j gotoTable
          in  gotoTable'
        Nothing -> gotoTable
