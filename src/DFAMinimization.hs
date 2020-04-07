module DFAMinimization () where

import StateMachine
import DFA
import Data.Map

type DFAPartition = [State]
type AlphaCharacter = Char
type AlphaBet = [AlphaCharacter]
type T = [DFAPartition]

getPartitionFromState :: DFA -> State -> AlphaCharacter -> T -> DFAPartition 
getPartitionFromState dfa state character partitions =
  let toState = toState (head (filter (\(Transition fromState input toState) -> input == character && fromState == state) (transitions dfa)))
  in  filter (\partition -> elem toState partition) partitions

splitThePartition :: T -> DFA -> DFAPartition -> AlphaCharacter -> DFAPartition
splitThePartition partitions dfa partionToSplit alphaCharacter =
  let stateAndCorrespondingPartitions = (map (\stateInPartition -> (stateInPartition, getPartitionFromState dfa stateInPartition alphaCharacter partitions)) partionToSplit)
      splitPartitionMap = foldr (\(state, partitions) partMap -> (Map.insert state partitions) ) (Map.empty) stateAndCorrespondingPartitions
      
      

split :: AlphaBet -> DFAPartition -> DFAPartition


minimizeDFA :: DFA -> DFA
minimizeDFA dfa =
  

