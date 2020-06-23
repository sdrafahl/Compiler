{-# LANGUAGE FlexibleInstances #-}
module Scanner.DFA (
  DFA(..),
  State,
  Transition,
  GetAlpha(..)
  ) where

import Scanner.StateMachine
import Data.Map
import Data.Set
import Data.List
import Scanner.TokenType

data DFA = DFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition Char], categories :: Map State TokenType} deriving (Eq)

instance Show DFA where
  show (DFA states startState terminalStates transitions category) = "DFA states: " ++ (show states) ++ " startState: " ++ startState ++ " terminalStates: " ++ (show terminalStates) ++ " transitions: " ++ (show transitions) ++ " category: " ++ (show category)

instance Show (Transition Char) where
  show (Transition frmState input toState) = "Transition fromState: " ++ frmState ++ " transCharacter: " ++ show input  ++ " toState: " ++ toState

instance GetAlpha DFA where
  getAlphaBet (DFA state startState terminalStates transitions _) = (Data.Set.toList (Data.Set.fromList (Data.List.map (\(Transition from input to) -> input) transitions)))

