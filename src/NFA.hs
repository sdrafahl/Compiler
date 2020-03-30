{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module NFA (
    Transition(..),
    NFA(..),
    State
  ) where

import StateMachine

data NFA = NFA {states :: [State], startState :: State, terminalStates :: [State], transitions :: [Transition InputCharacter]} deriving (Eq)

instance Show (Transition InputCharacter) where
  show (Transition frmState input toState) = "Transition " ++ frmState ++ " " ++ (show input)  ++ " " ++ toState

instance Show NFA where
  show (NFA states startState terminalStates transitions) = "DFA " ++ (show states) ++ " " ++ startState ++ " " ++ (show terminalStates) ++ " " ++ (show transitions)

instance Show InputCharacter where
  show EmptyChar = " EmptyChar "
  show (Character ch) = " " ++ [ch] ++ " " 
