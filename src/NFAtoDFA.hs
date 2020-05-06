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
import TokenType
import Data.List

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
convertInputCharacter :: InputCharacter -> Char
convertInputCharacter (Character ch) = ch
convertInputCharacter EmptyChar = ' '

processWorkList :: NFA -> Q -> WorkList -> T -> Alpha -> (Q, T, WorkList)
processWorkList nfa q [] t alphaBet = (q, t, [])
processWorkList nfa qSets workList transitionTable alpha =
  let q = (head workList)
      (resQ, resT, restOfTheWorklist) = (foldr (\alphaCharacter (qSet, transitions, workl) -> discoverDeltaSets alphaCharacter nfa transitions qSet q workl) (qSets, transitionTable, (tail workList)) alpha)
  in  processWorkList nfa resQ restOfTheWorklist resT alpha

convertSubsetsToDFA :: (Q, T) -> [State] -> [[State]] -> Map State TokenType -> [State] -> DFA
convertSubsetsToDFA (q, t) startNfaSubbset terminalNfaSubsets categoryMap terminalStates =
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
      categoryMap' = getDFATokenTypeMapFromNFATokenTypeMap dfaToNFAMap categoryMap terminalStates
  in (DFA (map (\(dfaState, _) -> dfaState) (Map.toList dfaToNFAMap)) newStartState newTerminalStates transitions categoryMap')

getDFATokenTypeMapFromNFATokenTypeMap :: Map State [State] -> Map State TokenType -> [State] -> Map State TokenType
getDFATokenTypeMapFromNFATokenTypeMap newDFAToDFAPartition nfaTokenTypeMap terminalStates = Map.fromList (Data.List.map
                                                                                (\(dfaState, collectionOfNFAStates) ->
                                                                                    (case (Data.List.filter
                                                                                           (\(_, maybeTokenType) ->
                                                                                               case maybeTokenType of
                                                                                                 Just BadTokenType -> False
                                                                                                 Just _ -> True
                                                                                                 _ -> False
                                                                                           ) (Data.List.map (\nfaState -> (elem nfaState terminalStates ,Map.lookup nfaState nfaTokenTypeMap)) collectionOfNFAStates)
                                                                                          )
                                                                                      of
                                                                                       [] -> (dfaState, BadTokenType)
                                                                                       categories -> (dfaState, Data.List.foldr (\(isTerminal, category) chosenSoFar -> case isTerminal of
                                                                                                               True -> maybe BadTokenType (\a -> a) category
                                                                                                               False -> chosenSoFar
                                                                                                               ) (maybe BadTokenType (\a -> a) (snd (head categories))) categories)
                                                                                    )) (Map.toList newDFAToDFAPartition))

terminalSubsets :: Q -> NFA -> [[State]]
terminalSubsets subSets nfa = filter (\subSet -> foldr (\state result -> result || elem state (NFA.terminalStates nfa)) False subSet) subSets

startSubsets :: Q -> NFA -> [State]
startSubsets subsets nfa = head (filter (\subSet -> foldr (\state result -> result || state == NFA.startState nfa) False subSet) subsets)
                
convertNFAToDFA :: NFA -> DFA
convertNFAToDFA nfa =
  let q0 = emptyCharClosure [NFA.startState nfa] nfa
      q = [q0]
      worklist = [q0]
      (qConfig, transTable, work) = processWorkList nfa q worklist Map.empty (getAlphaBet nfa)
      startSubset = startSubsets qConfig nfa
      terminalSubsetsFromNFA = terminalSubsets qConfig nfa
  in  convertSubsetsToDFA (qConfig, transTable) startSubset terminalSubsetsFromNFA (NFA.categories nfa) (NFA.terminalStates nfa)


instance Conversion NFA DFA where
  convert nfa = convertNFAToDFA nfa 
