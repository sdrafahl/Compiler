{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scanner () where

import Data.Map
import StateMachine

type InputCharacter = Char
type Lexeme = String
type CharactersRemovedFromStream = [Scanner.InputCharacter]
type InputPosition = Integer
type CharCategory = String

data InputStream = InputStream [Scanner.InputCharacter]
data Word = Word String
data StateStack = StateStack [(GoodOrBadState, InputPosition)]
data CharCatTable = CharCatTable (Map Scanner.InputCharacter CharCategory)
data DFATransitionTable = DFATransitionTable (Map (State, CharCategory) State)
data FailedTable = FailedTable (Map (State, InputPosition) Bool)
data DFAacceptingStates = DFAacceptingStates (Map State Bool)
data TokenTypeTable = TokenTypeTable (Map GoodOrBadState TokenType)
data TokenType = TokenType String | BadTokenType
data GoodOrBadState = GoodOrBadState State | BadState

exploreDFA :: InputStream -> Lexeme -> FailedTable -> InputPosition -> GoodOrBadState -> StateStack -> DFAacceptingStates -> CharCatTable -> DFATransitionTable -> (Lexeme, InputPosition, InputStream, StateStack)
exploreDFA inputStream lex failedTable inputPos currentState stateStack dfaAcceptingStates charCatTable dfaTransitionTable =
  case (hasFailed failedTable (currentState, inputPos)) of
    True -> (lex, inputPos, inputStream, stateStack)
    False ->
      case (nextChar inputStream) of
        Nothing -> (lex, inputPos, inputStream, stateStack)
        Just (inputCharacter, inputStream) ->
          case transition dfaTransitionTable currentState (getCatagory charCatTable inputCharacter) of
            BadState -> (lex, inputPos, inputStream, (push stateStack (BadState, inputPos)))
            GoodOrBadState toState -> exploreDFA inputStream (lex ++ [inputCharacter]) failedTable (inputPos + 1) (GoodOrBadState toState) newStateStack dfaAcceptingStates charCatTable dfaTransitionTable 
      where newStateStack = push (case (isAcceptingState dfaAcceptingStates currentState) of
                              True -> (StateStack [])
                              False -> stateStack) (currentState, inputPos)
      

            
rollBackToLongestWordInStack :: StateStack -> Lexeme -> CharactersRemovedFromStream -> TokenTypeTable -> FailedTable -> DFAacceptingStates -> (TokenType, FailedTable, Lexeme)           
rollBackToLongestWordInStack stateStack lex charactersRemovedFromStream tokenTypeTable failedTable acceptingStates =
  case (pop stateStack) of
    (Nothing, _) -> (BadTokenType, failedTable, lex)
    (Just (currentState, inputPosition), newStack) ->
       case (isAcceptingState acceptingStates currentState) of
         True -> ((lookupTokenType tokenTypeTable currentState), failedTable, lex)
         False -> rollBackToLongestWordInStack newStack (Scanner.truncate lex) lex tokenTypeTable (markAsFailed failedTable (currentState, inputPosition)) acceptingStates
      
 
-- let newStack = push stateStack (currentState, inputPos)
      
--nextWord :: InputStream -> (InputStream ,Scanner.Word)
--nextWord inputStream =



---------------------------------------
-- TokenTypeTable Helper Functions
---------------------------------------
lookupTokenType :: TokenTypeTable -> GoodOrBadState ->  TokenType
lookupTokenType (TokenTypeTable tokenTypeTable) (GoodOrBadState givenState) = maybe (TokenType "no existing type") (\a -> a) (Data.Map.lookup givenState tokenTypeTable)

---------------------------------------
-- Lexeme Helper Functions
---------------------------------------
truncate :: Lexeme -> Lexeme
truncate [] = ""
truncate lex = tail lex


---------------------------------------
-- Stream Helper Functions
---------------------------------------
nextChar :: InputStream -> Maybe (Scanner.InputCharacter, InputStream)
nextChar (InputStream []) = Nothing
nextChar (InputStream listOfInputCharacters) = Just (head listOfInputCharacters, InputStream (tail listOfInputCharacters))

rollBack :: InputStream -> Scanner.InputCharacter -> InputStream
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
getCatagory :: CharCatTable -> Scanner.InputCharacter -> CharCategory
getCatagory (CharCatTable tbl) inputChar = maybe "no specific category" (\a -> a) (Data.Map.lookup inputChar tbl)
