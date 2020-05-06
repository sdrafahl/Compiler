module StateMachine (
  State,
  InputCharacter(..),
  Transition(..),
  GetAlpha(..)
  ) where

type State = String
data InputCharacter = EmptyChar | Character Char deriving (Eq, Ord)
data Transition a = Transition {fromState :: State, input :: a, toState :: State} deriving (Eq)
data StateMachine a =  StateMachine a


class GetAlpha a where
  getAlphaBet :: a -> [Char]
