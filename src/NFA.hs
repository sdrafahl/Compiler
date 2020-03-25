module NFA (
    Transition(..),
    NFA(..),
    InputCharacter(..),
    State
  ) where

type State = String
data Transition = Transition {fromState :: State, input :: InputCharacter, toState :: State} deriving (Eq)
data NFA = NFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition]} deriving (Eq)
data InputCharacter = EmptyChar | Character Char deriving (Eq)

instance Show Transition where
  show (Transition frmState input toState) = "Transition " ++ frmState ++ " " ++ (show input)  ++ " " ++ toState

instance Show NFA where
  show (NFA states startState terminalStates transitions) = "NFA " ++ (show states) ++ " " ++ startState ++ " " ++ (show terminalStates) ++ " " ++ (show transitions)

instance Show InputCharacter where
  show EmptyChar = " EmptyChar "
  show (Character ch) = " " ++ [ch] ++ " " 
