{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scanner () where

import Data.Map
import StateMachine

type InputCharacter = Char
type Lexeme = String
type InputPosition = Integer
type CharCategory = String

data InputStream = InputStream [Scanner.InputCharacter]
data Word = Word String
data StateStack = StateStack [(State, InputPosition)]
data CharCatTable = CharCatTable (Map Scanner.InputCharacter CharCategory)
data DFATransitionTable = DFATransitionTable (Map (State, CharCategory) State)
data FailedTable = FailedTable (Map (State, InputPosition) Bool)
data DFAacceptingStates = DFAacceptingStates (Map State Bool)

exploreDFA :: InputStream -> Lexeme -> FailedTable -> InputPosition -> State -> StateStack -> DFAacceptingStates -> CharCatTable -> DFATransitionTable -> (Lexeme, InputPosition, InputStream, StateStack)
exploreDFA inputStream lex failedTable inputPos currentState stateStack dfaAcceptingStates charCatTable dfaTransitionTable =
  case (hasFailed failedTable (currentState, inputPos)) of
    True -> (lex, inputPos, inputStream, stateStack)
    False ->
      case (nextChar inputStream) of
        Nothing -> (lex, inputPos, inputStream, stateStack)
        Just (inputCharacter, inputStream) ->
          case transition dfaTransitionTable currentState (getCatagory charCatTable inputCharacter) of
            Nothing -> (lex, inputPos, inputStream, stateStack)
            Just toState -> exploreDFA inputStream (lex ++ [inputCharacter]) failedTable (inputPos + 1) (toState) newStateStack dfaAcceptingStates charCatTable dfaTransitionTable 
      where newStateStack = push (case (isAcceptingState dfaAcceptingStates currentState) of
                              True -> (StateStack [])
                              False -> stateStack) (currentState, inputPos)
      
      
          

-- let newStack = push stateStack (currentState, inputPos)
      
--nextWord :: InputStream -> (InputStream ,Scanner.Word)
--nextWord inputStream =


---------------------------------------
-- Stream Helper Functions
---------------------------------------
nextChar :: InputStream -> Maybe (Scanner.InputCharacter, InputStream)
nextChar (InputStream []) = Nothing
nextChar (InputStream listOfInputCharacters) = Just (head listOfInputCharacters, InputStream (tail listOfInputCharacters))

---------------------------------------
-- DFATransitionTable Helper Functions
---------------------------------------
transition :: DFATransitionTable -> State -> CharCategory -> Maybe State
transition (DFATransitionTable dfaTransTable) fromState category = (Data.Map.lookup (fromState, category) dfaTransTable)


---------------------------------------
-- StateStack Helper Functions
---------------------------------------
push :: StateStack -> (State, InputPosition) -> StateStack
push (StateStack stateList) newStateAndPosition = (StateStack (newStateAndPosition : stateList))

---------------------------------------
-- FailedTable Helper Functions
---------------------------------------
hasFailed :: FailedTable -> (State, InputPosition) -> Bool
hasFailed (FailedTable failedMap) stateAndInputPos = (member stateAndInputPos failedMap)

---------------------------------------
-- DFAacceptingStates Helper Functions
---------------------------------------
isAcceptingState :: DFAacceptingStates -> State -> Bool
isAcceptingState (DFAacceptingStates lookupTable) givenState = (member givenState lookupTable)

---------------------------------------
-- CharCatTable Helper Functions
---------------------------------------
getCatagory :: CharCatTable -> Scanner.InputCharacter -> CharCategory
getCatagory (CharCatTable tbl) inputChar = maybe "no specific category" (\a -> a) (Data.Map.lookup inputChar tbl)
