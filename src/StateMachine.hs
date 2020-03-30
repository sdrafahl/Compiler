module StateMachine (
  State,
  InputCharacter(..),
  Transition(..)
  ) where

type State = String
data InputCharacter = EmptyChar | Character Char deriving (Eq)
data Transition a = Transition {fromState :: State, input :: a, toState :: State} deriving (Eq)
