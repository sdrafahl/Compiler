{-# LANGUAGE FlexibleInstances #-}
module DFA (
  DFA(..),
  State,
  Transition  
  ) where

import StateMachine

data DFA = DFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition Char]} deriving (Eq)


instance Show DFA where
  show (DFA states startState terminalStates transitions) = "DFA states: " ++ (show states) ++ " startState: " ++ startState ++ " terminalStates: " ++ (show terminalStates) ++ " transitions: " ++ (show transitions)

instance Show (Transition Char) where
  show (Transition frmState input toState) = "Transition fromState: " ++ frmState ++ " transCharacter: " ++ show input  ++ " toState: " ++ toState

