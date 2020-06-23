{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scanner.DFAToScanner (convertDFAToScanner) where

import Scanner.Scanner
import Scanner.DFA
import Data.Map
import Scanner.StateMachine
import Data.List
import Scanner.CharCategoryTable

createCharCategoryTableFromDFA :: DFA -> (Char -> CharCategory) -> Map Char CharCategory
createCharCategoryTableFromDFA dfa charToCategory =
  fromList (Data.List.map (\alphaCharacter -> (alphaCharacter, charToCategory alphaCharacter)) dfaAlpha)
  where dfaAlpha = getAlphaBet dfa

createDfaTransitionTableFromNFA :: DFA -> (Char -> CharCategory) -> DFATransitionTable
createDfaTransitionTableFromNFA (DFA _ _ _ transitions _) inputCharacterToCategory =
  (DFATransitionTable (fromList (Data.List.map (\(Transition fromState character toState) -> ((fromState, inputCharacterToCategory character), toState)) transitions)))

convertDFAToScanner :: DFA -> (Char -> CharCategory) -> Scanner
convertDFAToScanner (DFA states startState terminalStates transitions tokenTypeTable) charToCat = (Scanner failedTable dfaTransitionTable dfaacceptingStates tokenTypeTable' startState charCatTable initialStreamPosition) 
  where dfaTransitionTable = createDfaTransitionTableFromNFA dfa charToCat
        charCatTable = (CharCatTable (createCharCategoryTableFromDFA dfa charToCat))
        dfa = (DFA states startState terminalStates transitions tokenTypeTable)
        dfaacceptingStates =  (DFAacceptingStates (fromList (Data.List.map (\state -> (state, True)) terminalStates)))
        tokenTypeTable' = (TokenTypeTable (mapKeys (\state -> GoodOrBadState state) tokenTypeTable))
        initialStreamPosition = 0
        failedTable = (FailedTable Data.Map.empty)




