{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Parser.First where

import Data.Set
import Data.List
import Data.Map
import Debug.Trace
import Parser.CFG
import Data.Maybe

data First = First (Map NonTerminalOrTerminal (Set Terminal)) deriving (Eq, Show)


addMappingToFirst :: NonTerminalOrTerminal -> Terminal -> First -> First
addMappingToFirst key value (First values) =
  let ((setForTheKey :: Maybe (Set Terminal))) = Data.Map.lookup key values
      (newSetForTheKey :: Set Terminal) = case setForTheKey of
        Nothing -> Data.Set.fromList [value]
        Just existingSet -> Data.Set.insert value existingSet
  in  First (Data.Map.insert key newSetForTheKey values)

addFirst :: First -> NonTerminalOrTerminal -> Set Terminal -> First
addFirst (First values') key values = (First (Data.Map.insert key values values'))

getFirst :: First -> NonTerminalOrTerminal -> Set Terminal
getFirst (First values) tokenKey = case Data.Map.lookup tokenKey values of
                                     Nothing -> Data.Set.empty
                                     Just terms -> terms

class FoldingAlgorithm dataToFold init algo where
  foldingAlgorithm :: dataToFold -> init -> algo -> init

instance FoldingAlgorithm (Set ProductionRule) First (First -> ProductionRule -> First) where
  foldingAlgorithm productionRules' intiStartMap algo = (Data.Set.foldl' algo intiStartMap productionRules')

class FixedPointAlgorithm d where
  fixPointOperation :: d -> (d -> d) -> d

instance FixedPointAlgorithm First where
  fixPointOperation first firstOperation =
    let first' = firstOperation first
    in case first' == first of
      True -> first
      False -> fixPointOperation first' firstOperation


data Flag = ReachedAllChildren | DidNotReachAllChildren
addTerminalsToRHS :: Set Terminal -> [NonTerminalOrTerminal] -> First -> (Set Terminal, Flag)
addTerminalsToRHS rhs [] _ = (rhs, ReachedAllChildren)
addTerminalsToRHS rhs [_] _ = (rhs, ReachedAllChildren)
addTerminalsToRHS rhs (x1:(x2:xs)) first
  | Data.Set.member (Terminal "δ") (getFirst first x1) = addTerminalsToRHS (Data.Set.union rhs (Data.Set.delete (Terminal "δ") (getFirst first x2))) (x2:xs) first
  | otherwise = (rhs, DidNotReachAllChildren)
      

addTerminalsToFirst :: First -> ProductionRule  -> First
addTerminalsToFirst first productionRule =
  let ((x :: NonTerminalOrTerminal):(xs :: [NonTerminalOrTerminal])) = getChildrenFromProductionRule productionRule
      (rhs :: Set Terminal) = Data.Set.delete (Terminal "δ") (getFirst first x)
      (rhs' :: Set Terminal, (flag :: Flag)) = addTerminalsToRHS rhs (x:xs) first
      (rhs'' :: Set Terminal) = case flag of
                                  ReachedAllChildren -> Data.Set.insert (Terminal "δ") rhs'
                                  DidNotReachAllChildren -> rhs'
      (a :: NonTerminal) = getParentFromProductionRule productionRule
      (rhs''' :: Set Terminal) = Data.Set.union (rhs'') (getFirst first (NonTerm a))
  in  addFirst first (NonTerm a) rhs'''

addTerminalsForEachProduction :: (Set ProductionRule) -> First -> First
addTerminalsForEachProduction prodRules first = foldingAlgorithm prodRules first addTerminalsToFirst

addTerminalsUntilFixedPoint :: First -> (Set ProductionRule) -> First
addTerminalsUntilFixedPoint first prodRules = fixPointOperation first fixedPointFirstAlgorithm
  where (fixedPointFirstAlgorithm :: (First -> First)) = addTerminalsForEachProduction prodRules

-- Main Algorithm                                     
createFirstMap :: Set ProductionRule -> Set Terminal -> First
createFirstMap setOfProductionRules terminals' =
  let (intialFirst :: First) = (First Data.Map.empty)
      (intialFirstWithTerminalsSet :: First) = (Data.Set.foldl' (\first terminal -> addMappingToFirst (Term terminal) terminal first) intialFirst terminals')
      (intialFirstWithTerminalsSetWithEmptyString :: First) = addMappingToFirst (Term (Terminal "δ")) (Terminal "δ") intialFirstWithTerminalsSet
      (intialFirstWithTerminalsSetWithEmptyStringAndEOF :: First) = addMappingToFirst (Term (Terminal "EOF")) (Terminal "EOF") intialFirstWithTerminalsSetWithEmptyString
  in  addTerminalsUntilFixedPoint intialFirstWithTerminalsSetWithEmptyStringAndEOF setOfProductionRules

