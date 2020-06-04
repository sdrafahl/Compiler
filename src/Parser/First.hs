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

getFirst :: First -> NonTerminalOrTerminal -> Maybe (Set Terminal)
getFirst (First values) tokenKey = Data.Map.lookup tokenKey values

class FoldingAlgorithm dataToFold init algo res where
  algorithm :: dataToFold -> init -> algo -> res

instance FoldingAlgorithm (Set ProductionRule) First (First -> ProductionRule -> First) First where
  algorithm productionRules' intiStartMap algo = (Data.Set.foldl' algo intiStartMap productionRules')

class FixedPointAlgorithm d where
  fixPointOperation :: d -> (d -> d) -> d

instance FixedPointAlgorithm First where
  fixPointOperation first firstOperation =
    let first' = firstOperation first
    in case first' == first of
      True -> first
      False -> fixPointOperation first' firstOperation

-- Main Algorithm                                     
-- createFirstMap :: Set ProductionRule -> Set Terminal -> First
-- createFirstMap setOfProductionRules terminals =
--   let (intialFirst :: First) = (First Data.Map.empty)
--       (intialFirstWithTerminalsSet :: First) = Data.Set.foldl' intialFirst (\first terminal -> addMappingToFirst (Term terminal) terminal first) terminals
--       (intialFirstWithTerminalsSetWithEmptyString :: First) = addMappingToFirst (Term (Terminal "δ")) (Term (Terminal "δ")) intialFirstWithTerminalsSet
--       (intialFirstWithTerminalsSetWithEmptyStringAndEOF :: First) = addMappingToFirst (Term (Terminal "EOF")) (Term (Terminal "EOF")) intialFirstWithTerminalsSet
      

