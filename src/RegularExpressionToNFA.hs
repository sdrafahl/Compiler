module RegularExpressionToNFA (reToNFA) where

import Data.Optional
import Data.Set
import Data.List
import Data.IORef
import Data.Maybe

type State = Integer
type InputCharacter = EmptyChar | Char
data Transition = Transition {fromState :: State, input :: InputCharacter, toState :: State}
data NFA = NFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition]}


homoMorphism :: NFA -> (State -> State) -> NFA
homoMorphism (NFA states startState terminalStates transitions) f = NFA (Data.List.map f states) (f startState) (Data.List.map f terminalStates) (Data.List.map (\transition -> (Transition (f (fromState transition)) (input transition) (f (toState transition)))) transitions)

baseNfa :: InputCharacter -> NFA
baseNfa input = NFA [0 ,1] 0 [1] [Transition 0 input 1]
andNfa :: NFA -> NFA -> NFA
andNfa leftNfa rightNfa =
  let lastNodeName = last (states leftNfa)
      mapNewNfa = (\nodeName -> lastNodeName + nodeName)
      newRightNfa = homoMorphism rightNfa mapNewNfa
      newConnectingTransitions =  Data.List.map (\terminalState -> Transition terminalState EmptyChar (startState rightNfa)) (terminalStates leftNfa)
      leftTransitions = transitions leftNfa
      rightTransitions = transitions newRightNfa
      allTransitions = newConnectingTransitions ++ leftTransitions ++ rightTransitions
      in NFA (states leftNfa ++ states newRightNfa) (startState leftNfa) (terminalStates rightNfa) allTransitions
         
orNfa :: NFA -> NFA -> NFA
orNfa leftNfa rightNfa =
  let mapNewNfa = (\nodeName -> (last (states leftNfa)) + nodeName)
      newRightNfa = homoMorphism rightNfa mapNewNfa
      newStartingState = (head (states leftNfa)) - 1
      newTerminalState = (last (states newRightNfa)) + 1
      allStates = newStartingState : (states leftNfa) ++ (states newRightNfa) ++ [newTerminalState]
      newFinalTransitions = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar newTerminalState))) ((terminalStates leftNfa) ++ (terminalStates newRightNfa))
      newTransitions = [(Transition newStartingState EmptyChar (startState leftNfa)), (Transition newStartingState EmptyChar (startState newRightNfa))] ++ newFinalTransitions
      allTransitions = (transitions leftNfa) ++ (transitions newRightNfa) ++ newTransitions
      in NFA allStates newStartingState [newTerminalState] allTransitions 
      
kleanClosureNfa :: NFA -> NFA
kleanClosureNfa nfa =
  let newStartingState = (head (states nfa)) - 1
      newTerminalState = (last (states nfa)) + 1
      allStates = newStartingState : (states nfa) ++ [newTerminalState]
      newLoopTransitions = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar (startState nfa))) (terminalStates nfa))
      allTransitions = [(Transition newStartingState EmptyChar newTerminalState), (Transition () EmptyChar newTerminalState), newLoopTransitions] ++ (transitions nfa)
      in (NFA allStates newStartingState [newTerminalState] allTransitions)


data REOrNFA = NFA | InputCharacter 

parenSubStringRange :: [InputCharacter] -> [InputCharacter] -> Integer -> [InputCharacter]
parenSubStringRange (')' : input) stack 1 = stack
parenSubStringRange ('(' : input) stack depth = parenSubStringRange input ('(' : stack) depth + 1
parenSubStringRange (')' : input) stack depth = parenSubStringRange input (')' : stack) depth - 1
parenSubStringRange input stack depth = parenSubStringRange (tail input) ((head input) : stack) depth

mapSubNFA :: [REOrNFA] -> [REOrNFA] -> [REOrNFA]
mapSubNFA [] mapped = mapped
mapSubNFA ('(' : input) mapped =
  let subStringParen = parenSubStringRange ('(' : input) [] 0
      subRe = (tail (take (length subStringParen) subStringParen))
      nfa = reToNFA subRe
      newMappedStack = mapped ++ [nfa]
      lengthToSkip = (length subStringParen)
      in subNFA (drop lengthToSkip ('(' : input)) newMappedStack
mapSubNFA input stack = subNFA (tail input) stack

mapKleenClosure :: [REOrNFA] -> [REOrNFA]
mapKleenClosure input =
  let indices = (map (\index -> index - 1) (elemIndices '*' input))
      mappedArray = mapAtIndices indices input
      in remove '*' mappedArray
      
mapAtIndices :: [integer] -> [REOrNFA] -> [REOrNFA]
mapAtIndices = [] stack = stack
mapAtIndices index : indices stack =
  let value = stack!!index
      baseNFA = case value of (NFA _) -> value 
                       otherwise -> baseNfa stack!!index
      in mapAtIndices indices (setAt index (kleanClosureNfa baseNFA) stack)

mapOrs :: [REOrNFA] -> [REOrNFA] -> [REOrNFA]
mapOrs input stack
  | length input == 2 = stack
  | (head (tail input)) == '|' =
      let leftNfa = case (head input) of
                      (NFA _) -> head input
                      otherwise -> baseNfa (head input)
          rightNfa = case (head (tail (tail input))) of
                       (NFA _) -> (head (tail (tail input)))
                       otherwise -> baseNfa (head (tail (tail input)))
          in mapOrs (drop 3 input) ((orNfa leftNfa rightNfa) : stack) 
  | mapOrs (tail input) ((head input) : stack)

mapBaseCharacters :: [REOrNFA] -> [NFA]
mapBaseCharacters input = map (\nfaOrChar -> case nfaOrChar of
                                  (NFA _) -> nfaOrChar
                                  otherwise -> baseNfa
                              ) input
  
reToNFA :: [InputCharacter] -> NFA
reToNFA input = 
  let mappedParen = mapSubNFA input []
      mappedKlean = mapKleenClosure mappedParen
      mappedOrs = mapOrs mappedKlean []
      mappedBase = mapBaseCharacters mappedOrs
      in foldl (\nfa nextNfa -> andNfa nfa nextNfa) (head mappedBase) (tail mappedBase)
