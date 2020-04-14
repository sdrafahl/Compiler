{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
module NFAtoDFA (Conversion(..)) where
import NFA
import DFA
import StateMachine
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Conversion

type Q = [[State]]
type WorkList = [[State]]
type T = Map [State] [(Char, [State])]
type Alpha = [Char]

getAllTransitionsFrom :: [State] -> NFA -> InputCharacter -> [Transition InputCharacter]
getAllTransitionsFrom givenStates nfa transCharacter = (filter (\(Transition fromState inp toState) -> inp == transCharacter) (filter (\transition -> elem (fromState transition) givenStates) (NFA.transitions nfa)))

delta :: [State] -> NFA -> InputCharacter -> [State]
delta givenStates nfa transCharacter = map (\trans -> (toState trans)) (getAllTransitionsFrom givenStates nfa transCharacter)

emptyCharClosure :: [State] -> NFA -> [State]
emptyCharClosure states nfa = states ++ (map (\trans -> (toState trans)) (getAllTransitionsFrom states nfa EmptyChar))

discoverDeltaSets :: Char -> NFA -> T -> Q -> [State] -> WorkList -> (Q, T, WorkList)
discoverDeltaSets alphaBetCharacter nfa t q workListCollection worklist =
  let toSets = (emptyCharClosure (delta workListCollection nfa (Character alphaBetCharacter)) nfa)
      mabyTransitions = Map.lookup workListCollection t
      transitions = maybe [] (\trans -> trans) mabyTransitions
      newLookupTable = Map.insert workListCollection (transitions ++ [(alphaBetCharacter, toSets)]) t
  in (case (elem toSets q, null toSets) of
        (_, True) -> (q, t, worklist)
        (False, False) -> (q ++ [toSets], newLookupTable, worklist ++ [toSets])
        (True, False) -> (q, newLookupTable, worklist)
     )

getAlphabet :: NFA -> [Char]
getAlphabet (NFA state startState terminalStates transitions) = (Set.toList (Set.fromList (map (\(Transition from input to) -> convertInputCharacter input) transitions)))

convertInputCharacter :: InputCharacter -> Char
convertInputCharacter (Character ch) = ch
convertInputCharacter EmptyChar = ' '

processWorkList :: NFA -> Q -> WorkList -> T -> Alpha -> (Q, T, WorkList)
processWorkList nfa q [] t alphaBet = (q, t, [])
processWorkList nfa qSets workList transitionTable alpha =
  let q = (head workList)
      (resQ, resT, restOfTheWorklist) = (foldr (\alphaCharacter (qSet, transitions, workl) -> discoverDeltaSets alphaCharacter nfa transitions qSet q workl) (qSets, transitionTable, (tail workList)) alpha)
  in  processWorkList nfa resQ restOfTheWorklist resT alpha

convertSubsetsToDFA :: (Q, T) -> [State] -> [[State]] -> DFA
convertSubsetsToDFA (q, t) startNfaSubbset terminalNfaSubsets =
  let (dfaToNFAMap, nfaToDFAMap, count) = foldr (\nfaSubset (dfaTNFAMap, nfaTDFAMap, count) ->
                                                   let newDFAState = show count
                                                       newdfaToNFAMap = (Map.insert newDFAState nfaSubset dfaTNFAMap)
                                                       newNfaToDFAMap = (Map.insert nfaSubset newDFAState nfaTDFAMap)
                                                   in (newdfaToNFAMap, newNfaToDFAMap, count + 1)
                                                ) (Map.empty, Map.empty, 0) (reverse q)
      transitions = Map.foldr (\dfaNode (trans) ->
                            let nfaNeighbors = maybe [] (\b -> b) (Map.lookup (maybe [] (\a -> a) (Map.lookup dfaNode dfaToNFAMap)) t)
                                dfaNeighborsAndCharacters = map (\(transCharacter, nfaStates) -> (transCharacter, maybe [] (\a -> a) (Map.lookup nfaStates nfaToDFAMap))) nfaNeighbors
                            in (map (\(transCharacter, dfaState) -> (Transition dfaNode transCharacter dfaState)) dfaNeighborsAndCharacters) ++ trans
                            ) ([]) nfaToDFAMap
      newStartState = maybe [] (\a -> a) (Map.lookup startNfaSubbset nfaToDFAMap)
      newTerminalStates = map (\nfaSubset -> maybe [] (\a -> a) (Map.lookup nfaSubset nfaToDFAMap)) terminalNfaSubsets
  in (DFA (map (\(dfaState, _) -> dfaState) (Map.toList dfaToNFAMap)) newStartState newTerminalStates transitions)

terminalSubsets :: Q -> NFA -> [[State]]
terminalSubsets subSets nfa = filter (\subSet -> foldr (\state result -> result || elem state (NFA.terminalStates nfa)) False subSet) subSets

startSubsets :: Q -> NFA -> [State]
startSubsets subsets nfa = head (filter (\subSet -> foldr (\state result -> result || state == NFA.startState nfa) False subSet) subsets)
                
convertNFAToDFA :: NFA -> DFA
convertNFAToDFA nfa =
  let q0 = emptyCharClosure [NFA.startState nfa] nfa
      q = [q0]
      worklist = [q0]
      (qConfig, transTable, work) = processWorkList nfa q worklist Map.empty (getAlphabet nfa)
      startSubset = startSubsets qConfig nfa
      terminalSubsetsFromNFA = terminalSubsets qConfig nfa
  in  convertSubsetsToDFA (qConfig, transTable) startSubset terminalSubsetsFromNFA


instance Conversion NFA DFA where
  convert nfa = convertNFAToDFA nfa 
