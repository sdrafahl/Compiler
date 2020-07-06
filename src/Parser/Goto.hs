{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Parser.Goto where

import Parser.LRItem
import Parser.CFG
import Data.Set
import Data.List
import Debug.Trace
import Data.Map

goto :: Set ProductionRule -> Set Terminal -> Set LRItem -> NonTerminalOrTerminal -> Set LRItem
goto prods terms s x =
  let (foldOverLRItemsAlgo :: Set LRItem -> LRItem -> Set LRItem) = foldOverLRItems x
      (moved' :: Set LRItem) = (Data.Set.foldl' foldOverLRItemsAlgo Data.Set.empty s)
  in  closure terms prods moved'

foldOverLRItems :: NonTerminalOrTerminal -> Set LRItem -> LRItem -> Set LRItem
foldOverLRItems x accLrItems (LRItem alpha c a') =
  let ((b' :: [TokenOrPlaceholder]), (c' :: [TokenOrPlaceholder])) = splitAtContext c
  in  case c' of
    [StackTop] -> accLrItems
    (StackTop:x':xs) -> case x' == (Token x) of
      True -> Data.Set.union accLrItems (Data.Set.fromList [LRItem alpha ((b' ++ [Token x] ++ [StackTop] ++ xs)) a'])
      False -> accLrItems
    _ -> accLrItems

