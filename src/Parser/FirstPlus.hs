{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Parser.FirstPlus where

import Parser.First
import Parser.Follow
import Parser.CFG
import Data.Set

data FirstPlus = FirstPlus First Follow

getFirstPlus :: ProductionRule -> FirstPlus -> Set Terminal
getFirstPlus productionRule (FirstPlus first follow)
  | not (Data.Set.member (Terminal "δ") (getFirstForAllChildren (getChildrenFromProductionRule productionRule) first)) = getFirstForAllChildren (getChildrenFromProductionRule productionRule) first
  | otherwise = Data.Set.union (getFirstForAllChildren (getChildrenFromProductionRule productionRule) first) (getTerminalsFromFollow follow (getParentFromProductionRule productionRule))
  

