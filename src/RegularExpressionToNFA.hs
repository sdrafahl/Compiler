{-# LANGUAGE MultiParamTypeClasses #-}
module RegularExpressionToNFA (
    NFA (..),
    InputCharacter (..),
    Transition (..),
    REOrNFA (..),
    Conversion(..),
    reToNFA
  ) where

import Data.List
import Data.List.Index
import Data.Char
import Debug.Trace
import System.IO.Unsafe
import Conversion
import RegularExpression
import NFA

instance Show REOrNFA where
  show (FA nfa) = show nfa
  show (NotFA inCharacter) = show inCharacter

data REOrNFA = FA NFA | NotFA Char deriving (Eq)

homoMorphism :: NFA -> (State -> State) -> NFA
homoMorphism (NFA states startState terminalStates transitions) f = NFA (Data.List.map f states) (f startState) (Data.List.map f terminalStates) (Data.List.map (\transition -> (Transition (f (fromState transition)) (input transition) (f (toState transition)))) transitions)

getLastState :: [State] -> State
getLastState collectionOfStates = foldr (\currentMax nextNfa -> show (max (read (currentMax)::Int) (read (nextNfa)::Int))) (last collectionOfStates) collectionOfStates

absoluteLinearScale :: Int -> Int -> Int
absoluteLinearScale input additional
  | input >= 0 = input + additional + 1
  | input < 0 = input - additional - 1

baseNfa :: Char -> NFA
baseNfa input = NFA ["0" ,"1"] "0" ["1"] [Transition "0" (Character input) "1"]
andNfa :: NFA -> NFA -> NFA
andNfa leftNfa rightNfa =
  let lastNodeName = getLastState (states leftNfa)
      mapNewNfa = (\nodeName -> (show (absoluteLinearScale (read nodeName::Int) (read lastNodeName::Int))))
      newRightNfa = homoMorphism rightNfa mapNewNfa
      newConnectingTransitions =  Data.List.map (\terminalState -> Transition terminalState EmptyChar (startState newRightNfa)) (terminalStates leftNfa)
      leftTransitions = transitions leftNfa
      rightTransitions = transitions newRightNfa
      allTransitions = newConnectingTransitions ++ leftTransitions ++ rightTransitions
      in NFA (states leftNfa ++ states newRightNfa) (startState leftNfa) (terminalStates newRightNfa) allTransitions
         
orNfa :: NFA -> NFA -> NFA
orNfa leftNfa rightNfa =
  let mapNewNfa = (\nodeName -> (show (absoluteLinearScale  (read nodeName::Int) (read (getLastState (states leftNfa))::Int))))
      newRightNfa = homoMorphism rightNfa mapNewNfa
      newStartingState = show ((read (head (states leftNfa))::Int) - 1)
      newTerminalState = show ((read (last (states newRightNfa))::Int) + 1)
      allStates = newStartingState : (states leftNfa) ++ (states newRightNfa) ++ [newTerminalState]
      newFinalTransitions = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar newTerminalState))) ((terminalStates leftNfa) ++ (terminalStates newRightNfa))
      newTransitions = [(Transition newStartingState EmptyChar (startState leftNfa)), (Transition newStartingState EmptyChar (startState newRightNfa))] ++ newFinalTransitions
      allTransitions = (transitions leftNfa) ++ (transitions newRightNfa) ++ newTransitions
      in NFA allStates newStartingState [newTerminalState] allTransitions 
      
kleanClosureNfa :: NFA -> NFA
kleanClosureNfa nfa =
  let newStartingState = show ((read (head (states nfa))::Int) - 1)
      newTerminalState = show ((read (last (states nfa))::Int) + 1)
      allStates = newStartingState : (states nfa) ++ [newTerminalState]
      newLoopTransitions = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar (startState nfa))) (terminalStates nfa))
      newTransitionsToNewTerminal = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar newTerminalState)) (terminalStates nfa))
      allTransitions = [(Transition newStartingState EmptyChar newTerminalState)] ++ newTransitionsToNewTerminal ++ newLoopTransitions ++ (transitions nfa) ++ [(Transition newStartingState EmptyChar (startState nfa))]
      in (NFA allStates newStartingState [newTerminalState] allTransitions)

parenSubStringRange :: [Char] -> [Char] -> Int -> [Char]
parenSubStringRange (')' : input) stack 1 = stack ++ [')']
parenSubStringRange ('(' : input) stack depth = parenSubStringRange input (stack ++ ['(']) (depth + 1)
parenSubStringRange (')' : input) stack depth = parenSubStringRange input (stack ++ [')']) (depth - 1)
parenSubStringRange input stack depth = parenSubStringRange (tail input) (stack ++ [(head input)]) depth

mapSubNFA :: [Char] -> [REOrNFA] -> [REOrNFA]
mapSubNFA [] mapped = mapped
mapSubNFA ('(' : input) mapped =
  let test = '(' : input
      subStringParen = parenSubStringRange ('(' : input) [] 0
      subRe = (tail (take ((length subStringParen) - 1) subStringParen))
      nfa = reToNFA (RE subRe)
      newMappedStack = mapped ++ [FA nfa]
      lengthToSkip = (length subStringParen)
      in mapSubNFA (drop lengthToSkip ('(' : input)) newMappedStack
mapSubNFA input mapped = mapSubNFA (tail input) (mapped ++ [(NotFA (head input))])

mapKleenClosure :: [REOrNFA] -> [REOrNFA]
mapKleenClosure input =
  let indices = (map (\index -> index - 1) (elemIndices (NotFA '*') input))
      mappedArray = mapAtIndices indices input
      in filter (\va -> va /= (NotFA '*') )  mappedArray
      
mapAtIndices :: [Int] -> [REOrNFA] -> [REOrNFA]
mapAtIndices [] stack = stack
mapAtIndices (index : indices) stack =
  let value = stack!!index
      baseNFA = (case value of
                    (FA fa) -> fa
                    (NotFA notFa) -> baseNfa notFa
                )
      in mapAtIndices indices (setAt index (FA (kleanClosureNfa baseNFA)) stack)

mapOrs :: [REOrNFA] -> [REOrNFA] -> [REOrNFA]
mapOrs input stack
  | length input <= 2 = stack ++ (reverse input)
  | (head (tail input)) == (NotFA '|') =
      let leftNfa = case (head input) of
                      (FA fa) -> fa
                      (NotFA notFA) -> baseNfa notFA
          rightNfa = case (head (tail (tail input))) of
                       (FA fa) -> fa
                       (NotFA notFA) -> baseNfa notFA
          in mapOrs (drop 3 input) (FA (orNfa leftNfa rightNfa) : stack) 
  | otherwise = mapOrs (tail input) ((head input) : stack)

mapBaseCharacters :: [REOrNFA] -> [NFA]
mapBaseCharacters inputNfas = map (\nfaOrChar -> case nfaOrChar of
                                  (FA fa) -> fa
                                  (NotFA notFa) -> baseNfa notFa
                              ) inputNfas

reToNFA :: RegEx -> NFA
reToNFA (RE input) =
  let mappedToNfas = mapBaseCharacters (mapOrs (mapKleenClosure (mapSubNFA input [])) [])
  in foldr (\nfa nextNfa -> andNfa nfa nextNfa) (head mappedToNfas) (tail mappedToNfas)                              

instance Conversion RegEx NFA where
  convert re = reToNFA re
