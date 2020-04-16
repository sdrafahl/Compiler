{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scanner (Scanner(..)) where

import Data.Map
import StateMachine
import Data.Stream

type DFATable = Map (CharCatagory, State) State
type TokenCategory = String
type TokenType = Map State TokenCategory

type InputStream = Stream String
type InputPosition = Int
type FailedTable = Map (GoodOrBadState, InputPosition) Bool
type AcceptingStates = Map State State
type Stack = [(State, InputPosition)]
type Tokens = [String]
type Lexeme = String
type IsAcceptingState = (GoodOrBadState -> Bool)
type CharCatagory = String
type CharCatagoryTable = Map Char CharCatagory
type Word = String
data GoodOrBadState = State | BadState

nextChar :: Scanner.Word -> Maybe Char
nextChar word = maybe Nothing (\(h, _) -> h) (uncons word)

safeTail :: [a] -> [a]
safeTail list =  (maybe [] (\(_, ta) -> ta) (uncons list))

append :: Lexeme -> Char -> Lexeme
append lex inp = lex ++ [inp]

getNextState :: DFATable -> CharCatagory -> GoodOrBadState -> GoodOrBadState
getNextState table catagory currentState = maybe BadState (\a -> a) (lookup (catagory, currentState) table)

searchLookupTable :: CharCatagoryTable -> Maybe Char -> CharCatagory
searchLookupTable catTable Nothing = "No Specific Catagory"
searchLookupTable catTable Just inputChar = maybe "No Specific Catagory" (\a -> a) (lookup inputChar catTable)

readCharIntoDFA :: Scanner.Word -> DFATable -> InputPosition -> Lexeme -> FailedTable -> CharCatagoryTable -> IsAcceptingState -> Stack -> GoodOrBadState -> (InputPosition, Stack, Lexeme)
readCharIntoDFA input dfaTable inputPosition lexeme failedTable catagoryTable isAcceptingState stateStack currentState =
  case member (currentState, inputPosition) failedTable of
    True -> (inputPosition, stateStack, lexeme)
    False ->
      let newStack = case (isAcceptingState currentState) of
            True -> [(currentState, inputPosition)]
            False -> ([(currentState, inputPosition)] ++ stateStack)
      in readCharIntoDFA (safeTail input) dfaTable (inputPosition + 1) (append lexeme (nextChar input)) failedTable isAcceptingState newStack (getNextState dfaTable (searchLookupTable catagoryTable (nextChar input)))




--nextWord :: InputStream -> DFATable 


-- class Scanner where
--   isValid :: String -> Tokens
