{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.ParserTree where

import Data.Set
import Data.List
import Data.Map
import Debug.Trace
import Parser.CFG

data ParserTree = ParseTreeNode NonTerminal [ParserTree] | Leafe Terminal deriving (Eq, Ord, Show)
data ParseTreeStack = ParseTreeStack [ParserTree] deriving (Eq, Ord, Show)

getGrammarFromParseTree :: ParserTree -> NonTerminalOrTerminal
getGrammarFromParseTree (ParseTreeNode g _) = NonTerm g
getGrammarFromParseTree (Leafe g) = Term g

getChildrenInTree :: ParserTree -> [ParserTree]
getChildrenInTree (ParseTreeNode _ a) = a
getChildrenInTree (Leafe _) = []

createLeafe :: Terminal -> ParserTree
createLeafe term = Leafe term

pushparseTreeStack :: ParseTreeStack -> ParserTree -> ParseTreeStack
pushparseTreeStack (ParseTreeStack lst) tre = (ParseTreeStack (lst ++ [tre]))

popnStack :: ParseTreeStack -> Int -> (ParseTreeStack, [ParserTree])
popnStack (ParseTreeStack lsty) n =
  let (amountToPop :: Int) = Data.List.length lsty - n
      (remainderStack :: [ParserTree], r) = Data.List.splitAt amountToPop lsty
  in  ((ParseTreeStack remainderStack), r)

addParentToParseTree :: NonTerminal -> ParseTreeStack -> Int -> (ParserTree, ParseTreeStack)
addParentToParseTree nonTerm treeStack reduceLength =
  let (remainingStack :: ParseTreeStack, removed :: [ParserTree]) = popnStack treeStack reduceLength
      (newTree :: ParserTree) = (ParseTreeNode nonTerm removed)
  in  (newTree, remainingStack)

pushTree :: ParseTreeStack -> ParserTree -> ParseTreeStack
pushTree (ParseTreeStack st) tr = (ParseTreeStack (st ++ [tr]))

createTreeFromStack :: ParseTreeStack -> ParserTree
createTreeFromStack  (ParseTreeStack [a]) = a
createTreeFromStack  (ParseTreeStack (x:_)) = x

