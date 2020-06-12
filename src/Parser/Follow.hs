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
import Parser.First

data Follow = Follow (Map NonTerminal (Set Terminal)) deriving (Eq, Show)

getTerminalsFromFollow :: Follow -> NonTerminal -> Set Terminal
getTerminalsFromFollow (Follow mappings) key = case (Data.Map.lookup key mappings) of
                                                 Just terminals' -> terminals'
                                                 Nothing -> Data.Set.empty

addTerminalsForSomeKey :: Follow -> NonTerminal -> Set Terminal -> Follow
addTerminalsForSomeKey (Follow mappings) key value =
  let existingValue = getTerminalsFromFollow (Follow mappings) key
      unionedTerminals = Data.Set.union value existingValue
      newMap = Data.Map.insert key unionedTerminals mappings
  in  (Follow newMap)

unionFollows :: Follow -> Follow -> Follow
unionFollows (Follow mappings1) (Follow mappings2) = (Follow (Data.Map.union mappings1 mappings2))

addValueToAllKeys :: Follow -> Terminal -> Follow
addValueToAllKeys (Follow mappings) terminal  =
  let newMappings = Data.Map.map (\setOfTerminals -> Data.Set.insert terminal setOfTerminals) mappings
  in (Follow newMappings)

instance FoldingAlgorithm (Set ProductionRule) Follow (Follow -> ProductionRule -> Follow) where
  foldingAlgorithm productionRules' initFollow  algo = (Data.Set.foldl' algo initFollow productionRules')

instance FixedPointAlgorithm Follow where
  fixPointOperation follow followOperation =
    let follow' = followOperation follow
    in case follow' == follow of
      True -> follow
      False -> fixPointOperation follow' followOperation

createFollow :: First -> (Set ProductionRule) -> Follow
createFollow first prodRules =
  let (fixedPointAlgorithm :: Follow -> Follow) = fixedPointAlgorithm first prodRules
      (follow :: Follow) = (Follow Data.Map.empty)
      (fixedPointAlgorithm :: Follow -> Follow) = fixedPointAlgorithm first prodRules
      (follow' :: Follow) = fixPointOperation follow fixedPointAlgorithm
  in  addValueToAllKeys follow (Terminal "δ") 
      
fixedPointAlgorithm :: First -> (Set ProductionRule) -> Follow -> Follow
fixedPointAlgorithm first prodRules follow =
  let (productionRulesAlgorithm :: Follow -> ProductionRule -> Follow) = addFollowingForEachProductionRule first
  in  foldingAlgorithm prodRules follow productionRulesAlgorithm 
  
addFollowingForEachProductionRule :: First -> Follow -> ProductionRule -> Follow
addFollowingForEachProductionRule first follow productionRule =
  let (trailer :: Set Terminal) = getTerminalsFromFollow follow (getParentFromProductionRule productionRule)
      (childrenOfProductionRule :: [NonTerminalOrTerminal]) = getChildrenFromProductionRule productionRule
      (algorithmToScanThroughChildren :: (Follow, Set Terminal) -> NonTerminalOrTerminal -> (Follow, Set Terminal)) = addFollowingForGivenRightChildren first
      (follow', _) = foldingAlgorithm childrenOfProductionRule follow algorithmToScanThroughChildren
  in  follow'

instance FoldingAlgorithm [NonTerminalOrTerminal] Follow (Follow, Set Terminal) -> NonTerminalOrTerminal -> (Follow, Set Terminal) where
  foldingAlgorithm children' initFollow algo = (Data.Set.foldl' algo initFollow (Data.List.reverse children'))

addFollowingForGivenRightChildren :: First -> (Follow, Set Terminal) -> NonTerminalOrTerminal -> (Follow, Set Terminal)
addFollowingForGivenRightChildren _ accFollows (Term _) = accFollows
addFollowingForGivenRightChildren first (accFollow, trailer) (NonTerm nonTerm) =
  let (followingForChild :: Set Terminal) = getTerminalsFromFollow accFollow
      (accFollow' :: Follow) = addTerminalsForSomeKey accFollow nonTerm followingForChild
      (firstTerminals :: Set Terminal) = getFirst first (NonTerm nonTerm)
      (trailer' :: First) = case (Data.Set.member (Terminal "δ") firstTerminals) of
                              True ->
                                let (firstTerminals' :: Set Terminals) = Data.Set.delete (Terminal "δ") firstTerminals
                                in  Data.Set.union firstTerminals' trailer
                              False -> firstTerminals
  in (accFollow', firstTerminals)


          
      
      
      
