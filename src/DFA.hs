module DFA () where

data DFA = DFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition]} deriving (Eq)

type State = String
data Transition = Transition {fromState :: State, input :: InputCharacter, toState :: State} deriving (Eq)
data InputCharacter =  Character Char deriving (Eq)
