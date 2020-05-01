{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module NFA (
    Transition(..),
    NFA(..),
    baseNfa,
    homoMorphism,
    andNfa,
    orNfa,
    kleanClosureNfa
  ) where

import StateMachine
import Category
import Data.Map
import Data.List

data NFA = NFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition InputCharacter], categories :: Map State Category} deriving (Eq)

instance Show (Transition InputCharacter) where
  show (Transition frmState input toState) = "Transition " ++ frmState ++ " " ++ (show input)  ++ " " ++ toState

instance Show NFA where
  show (NFA states startState terminalStates transitions cat) = "DFA " ++ (show states) ++ " " ++ startState ++ " " ++ (show terminalStates) ++ " " ++ (show transitions) ++ " " ++ (show cat) 

instance Show InputCharacter where
  show EmptyChar = " EmptyChar "
  show (Character ch) = " " ++ [ch] ++ " " 

baseNfa :: Char -> Category -> NFA
baseNfa input category = NFA ["0" ,"1"] "0" ["1"] [Transition "0" (Character input) "1"] (fromList [("1", category)])


homoMorphism :: NFA -> (State -> State) -> NFA
homoMorphism (NFA states startState terminalStates transitions category) f = NFA (Data.List.map f states) (f startState) (Data.List.map f terminalStates) (Data.List.map (\transition -> (Transition (f (fromState transition)) (input transition) (f (toState transition)))) transitions) category


getLastState :: [State] -> State
getLastState collectionOfStates = Data.List.foldr (\currentMax nextNfa -> show (max (read (currentMax)::Int) (read (nextNfa)::Int))) (last collectionOfStates) collectionOfStates

absoluteLinearScale :: Int -> Int -> Int
absoluteLinearScale input additional
  | input >= 0 = input + additional + 1
  | input < 0 = input - additional - 1

andNfa :: NFA -> NFA -> Category -> NFA
andNfa leftNfa rightNfa newCategory =
  let lastNodeName = getLastState (states leftNfa)
      mapNewNfa = (\nodeName -> (show (absoluteLinearScale (read nodeName::Int) (read lastNodeName::Int))))
      newRightNfa = homoMorphism rightNfa mapNewNfa
      newConnectingTransitions =  Data.List.map (\terminalState -> Transition terminalState EmptyChar (startState newRightNfa)) (terminalStates leftNfa)
      leftTransitions = transitions leftNfa
      rightTransitions = transitions newRightNfa
      allTransitions = newConnectingTransitions ++ leftTransitions ++ rightTransitions
      in NFA (states leftNfa ++ states newRightNfa) (startState leftNfa) (terminalStates newRightNfa) allTransitions (Data.Map.union (categories leftNfa) (categories rightNfa))
         
orNfa :: NFA -> NFA -> Category -> NFA
orNfa leftNfa rightNfa newCategory =
  let mapNewNfa = (\nodeName -> (show (absoluteLinearScale  (read nodeName::Int) (read (getLastState (states leftNfa))::Int))))
      newRightNfa = homoMorphism rightNfa mapNewNfa
      newStartingState = show ((read (head (states leftNfa))::Int) - 1)
      newTerminalState = show ((read (last (states newRightNfa))::Int) + 1)
      allStates = newStartingState : (states leftNfa) ++ (states newRightNfa) ++ [newTerminalState]
      newFinalTransitions = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar newTerminalState))) ((terminalStates leftNfa) ++ (terminalStates newRightNfa))
      newTransitions = [(Transition newStartingState EmptyChar (startState leftNfa)), (Transition newStartingState EmptyChar (startState newRightNfa))] ++ newFinalTransitions
      allTransitions = (transitions leftNfa) ++ (transitions newRightNfa) ++ newTransitions
      in NFA allStates newStartingState [newTerminalState] allTransitions (Data.Map.union (Data.Map.union (categories leftNfa) (categories rightNfa)) (Data.Map.fromList [(newTerminalState, newCategory)]))
      
kleanClosureNfa :: NFA -> Category -> NFA
kleanClosureNfa nfa newCategory =
  let newStartingState = show ((read (head (states nfa))::Int) - 1)
      newTerminalState = show ((read (last (states nfa))::Int) + 1)
      allStates = newStartingState : (states nfa) ++ [newTerminalState]
      newLoopTransitions = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar (startState nfa))) (terminalStates nfa))
      newTransitionsToNewTerminal = (Data.List.map (\terminalState -> (Transition terminalState EmptyChar newTerminalState)) (terminalStates nfa))
      allTransitions = [(Transition newStartingState EmptyChar newTerminalState)] ++ newTransitionsToNewTerminal ++ newLoopTransitions ++ (transitions nfa) ++ [(Transition newStartingState EmptyChar (startState nfa))]
      in (NFA allStates newStartingState [newTerminalState] allTransitions) (Data.Map.union (categories nfa) (fromList [(newTerminalState, newCategory)]))
