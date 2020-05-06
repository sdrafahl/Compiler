{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module DFAToScanner (convertDFAToScanner) where

import Scanner
import DFA
import Data.Map
import StateMachine
import Data.List
import CharCategoryTable

createCharCategoryTableFromDFA :: DFA -> (Char -> CharCategory) -> Map Char CharCategory
createCharCategoryTableFromDFA dfa charToCategory =
  fromList (Data.List.map (\alphaCharacter -> (alphaCharacter, charToCategory alphaCharacter)) dfaAlpha)
  where dfaAlpha = getAlphaBet dfa

createDfaTransitionTableFromNFA :: DFA -> (Char -> CharCategory) -> DFATransitionTable
createDfaTransitionTableFromNFA (DFA _ _ _ transitions _) inputCharacterToCategory =
  (DFATransitionTable (fromList (Data.List.map (\(Transition fromState character toState) -> ((fromState, inputCharacterToCategory character), toState)) transitions)))

convertDFAToScanner :: DFA -> (Char -> CharCategory) -> Scanner
convertDFAToScanner (DFA states startState terminalStates transitions tokenTypeTable) charToCat = (Scanner dfaTransitionTable dfaacceptingStates tokenTypeTable' startState charCatTable)
  where dfaTransitionTable = createDfaTransitionTableFromNFA dfa charToCat
        charCatTable =  createCharCategoryTableFromDFA dfa charToCat
        dfa = (DFA states startState terminalStates transitions tokenTypeTable)
        dfaacceptingStates =  (DFAacceptingStates (fromList (Data.List.map (\state -> (state, True)) terminalStates)))
        tokenTypeTable' = (TokenTypeTable (mapKeys (\state -> GoodOrBadState state) tokenTypeTable))




