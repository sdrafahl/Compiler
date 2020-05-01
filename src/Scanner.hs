{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scanner (
  nextWord,
  InputStream(..),
  StateStack(..),
  CharCatTable(..),
  DFATransitionTable(..),
  FailedTable(..),
  DFAacceptingStates(..),
  TokenTypeTable(..),
  TokenType(..),
  GoodOrBadState(..),
  Show(..)
  ) where

import Data.Map
import StateMachine
import CharCategoryTable

type Lexeme = String
type InputPosition = Integer

type InitialState = State

data InputStream = InputStream [CharCategoryTable.InputCharacter] deriving (Eq)
data StateStack = StateStack [(GoodOrBadState, InputPosition)] deriving (Eq, Ord)
data DFATransitionTable = DFATransitionTable (Map (State, CharCategory) State) deriving (Eq, Ord)
data FailedTable = FailedTable (Map (State, InputPosition) Bool) deriving (Eq)
data DFAacceptingStates = DFAacceptingStates (Map State Bool) deriving (Eq, Ord)
data TokenTypeTable = TokenTypeTable (Map GoodOrBadState TokenType) deriving (Eq, Ord)
data TokenType = TokenType String | BadTokenType deriving (Eq, Ord)
data GoodOrBadState = GoodOrBadState State | BadState deriving (Eq, Ord)

instance Show FailedTable where
  show (FailedTable tbl) = show tbl

instance Show InputStream where
  show (InputStream strm) = show strm 

nextWord :: FailedTable -> InputStream -> InputPosition -> DFAacceptingStates -> CharCatTable -> DFATransitionTable -> InitialState -> TokenTypeTable -> Maybe (FailedTable, Lexeme, InputPosition, InputStream)
nextWord _ (InputStream []) _ _ _ _ _ _ = Nothing
nextWord failedTable (InputStream inputStream) inputPos dfaAcceptingStates charCatTable dfaTransTable initialState tokenTypeTable =
  let (lex, inputPos', inputStream', stateStack) = exploreDFA (InputStream inputStream) "" failedTable 0 (GoodOrBadState initialState) (StateStack [((GoodOrBadState initialState), inputPos)]) dfaAcceptingStates charCatTable dfaTransTable
      (tokenType, failedTable', lex', inputStream'', inputPos'') = rollBackToLongestWordInStack stateStack lex tokenTypeTable failedTable dfaAcceptingStates inputStream'
  in  Just (failedTable', lex', inputPos'', inputStream'')


exploreDFA :: InputStream -> Lexeme -> FailedTable -> InputPosition -> GoodOrBadState -> StateStack -> DFAacceptingStates -> CharCatTable -> DFATransitionTable -> (Lexeme, InputPosition, InputStream, StateStack)
exploreDFA inputStream lex failedTable inputPos currentState stateStack dfaAcceptingStates charCatTable dfaTransitionTable =
  case (hasFailed failedTable (currentState, inputPos)) of
    True -> (lex, inputPos, inputStream, stateStack)
    False ->
      case (nextChar inputStream) of
        Nothing -> (lex, inputPos, inputStream, stateStack) 
        Just (inputCharacter, inputStream') ->
          case transition dfaTransitionTable currentState (getCatagory charCatTable inputCharacter) of
            BadState -> (lex ++ [inputCharacter], inputPos + 1, inputStream', (push stateStack (BadState, inputPos + 1)))
            GoodOrBadState toState' ->
              let newStateStack = push (case (isAcceptingState dfaAcceptingStates (GoodOrBadState toState')) of
                              True -> (StateStack [])
                              False -> stateStack) (GoodOrBadState toState', inputPos + 1)
      
              in exploreDFA inputStream' (lex ++ [inputCharacter]) failedTable (inputPos + 1) (GoodOrBadState toState') newStateStack dfaAcceptingStates charCatTable dfaTransitionTable
            
            
rollBackToLongestWordInStack :: StateStack -> Lexeme  -> TokenTypeTable -> FailedTable -> DFAacceptingStates -> InputStream -> (TokenType, FailedTable, Lexeme, InputStream, InputPosition)
rollBackToLongestWordInStack stateStack lex tokenTypeTable failedTable acceptingStates inputStream =
  case (pop stateStack) of
    (Nothing, _) -> (BadTokenType, failedTable, lex, inputStream, 0)
    (Just (currentState, inputPosition), newStack) ->
       case (isAcceptingState acceptingStates currentState) of
         True -> ((lookupTokenType tokenTypeTable currentState), failedTable, lex, inputStream, inputPosition)
         False -> case Scanner.getLast lex of
           Just charToPutBackOnStream -> rollBackToLongestWordInStack newStack (Scanner.truncate lex) tokenTypeTable (markAsFailed failedTable (currentState, inputPosition)) acceptingStates (rollBack inputStream charToPutBackOnStream)
           Nothing -> rollBackToLongestWordInStack newStack (Scanner.truncate lex) tokenTypeTable (markAsFailed failedTable (currentState, inputPosition)) acceptingStates inputStream
           
---------------------------------------
-- TokenTypeTable Helper Functions
---------------------------------------
lookupTokenType :: TokenTypeTable -> GoodOrBadState -> TokenType
lookupTokenType (TokenTypeTable tokenTypeTable) givenState = maybe (TokenType "no existing type") (\a -> a) (Data.Map.lookup givenState tokenTypeTable)

---------------------------------------
-- Lexeme Helper Functions
---------------------------------------
truncate :: Lexeme -> Lexeme
truncate [] = ""
truncate lex = init lex
getLast :: Lexeme -> Maybe Char
getLast [] = Nothing
getLast lex = Just (last lex)


---------------------------------------
-- Stream Helper Functions
---------------------------------------
nextChar :: InputStream -> Maybe (CharCategoryTable.InputCharacter, InputStream)
nextChar (InputStream []) = Nothing
nextChar (InputStream listOfInputCharacters) = Just (head listOfInputCharacters, InputStream (tail listOfInputCharacters))

rollBack :: InputStream -> CharCategoryTable.InputCharacter -> InputStream
rollBack (InputStream inStream) characterToPutBackOnStream = (InputStream (characterToPutBackOnStream : inStream))

---------------------------------------
-- DFATransitionTable Helper Functions
---------------------------------------
transition :: DFATransitionTable -> GoodOrBadState -> CharCategory -> GoodOrBadState
transition _ BadState category = BadState
transition (DFATransitionTable dfaTransTable) (GoodOrBadState fromState) category =
  let maybeState = (Data.Map.lookup (fromState, category) dfaTransTable)
  in   case maybeState of
        Just state -> (GoodOrBadState state)
        Nothing -> BadState
  
---------------------------------------
-- StateStack Helper Functions
---------------------------------------
push :: StateStack -> (GoodOrBadState, InputPosition) -> StateStack
push (StateStack stateList) newStateAndPosition = (StateStack (newStateAndPosition : stateList))
pop :: StateStack -> ((Maybe (GoodOrBadState, InputPosition)), StateStack)
pop (StateStack []) = (Nothing, (StateStack []))
pop (StateStack stateList) = ((Just (head stateList)), (StateStack (tail stateList)))

---------------------------------------
-- FailedTable Helper Functions
---------------------------------------
hasFailed :: FailedTable -> (GoodOrBadState, InputPosition) -> Bool
hasFailed _ (BadState, _) = False
hasFailed (FailedTable failedMap) ((GoodOrBadState state), inputPosition) = (member (state, inputPosition) failedMap)
markAsFailed :: FailedTable -> (GoodOrBadState, InputPosition) -> FailedTable
markAsFailed failedTable (BadState, _) = failedTable
markAsFailed (FailedTable failedMap) ((GoodOrBadState state), inputPosition) = (FailedTable (insert (state, inputPosition) True failedMap))

---------------------------------------
-- DFAacceptingStates Helper Functions
---------------------------------------
isAcceptingState :: DFAacceptingStates -> GoodOrBadState -> Bool
isAcceptingState _ BadState = False
isAcceptingState (DFAacceptingStates lookupTable) (GoodOrBadState givenState)  = (member givenState lookupTable)

---------------------------------------
-- CharCatTable Helper Functions
---------------------------------------
getCatagory :: CharCatTable -> CharCategoryTable.InputCharacter -> CharCategory
getCatagory (CharCatTable tbl) inputChar = maybe "no specific category" (\a -> a) (Data.Map.lookup inputChar tbl)
