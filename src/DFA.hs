{-# LANGUAGE FlexibleInstances #-}
module DFA (
  DFA(..),
  State,
  Transition  
  ) where

import StateMachine
import Category
import Data.Map

data DFA = DFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition Char], categories :: Map State Category} deriving (Eq)

instance Show DFA where
  show (DFA states startState terminalStates transitions category) = "DFA states: " ++ (show states) ++ " startState: " ++ startState ++ " terminalStates: " ++ (show terminalStates) ++ " transitions: " ++ (show transitions) ++ " category: " ++ (show category)

instance Show (Transition Char) where
  show (Transition frmState input toState) = "Transition fromState: " ++ frmState ++ " transCharacter: " ++ show input  ++ " toState: " ++ toState

