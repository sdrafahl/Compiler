{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Parser.Parser where

import Data.Set
import Data.List
import Data.Map
import Parser.LRItem
import Parser.CFG
import RecursiveAlgorithms.FixedPoint
import Parser.Goto
import Debug.Trace
import Data.Maybe
import Scanner.Scanner
import Parser.CreateParserTable
import Parser.CanonicalCollection
import Scanner.RegularExpression
import Scanner.RegularExpressionToNFA
import Scanner.DFAToScanner
import Parser.CanonicalCollection
import Scanner.CharCategoryTable
import Scanner.DFA
import Scanner.NFA
import Scanner.Minimization
import Scanner.Conversion
import Scanner.NFAtoDFA
import Scanner.DFAToScanner
import Scanner.DFAMinimization

data BottomUpParserStates = BottomUpParserStates CC ActionTable GotoTable deriving (Eq, Ord, Show)
data Parser = Parser Scanner BottomUpParserStates deriving (Eq, Ord, Show)
data StackState = StackState Int | Dollar deriving (Eq, Ord, Show)
data StackStateOrToken = Tok NonTerminalOrTerminal | St StackState deriving (Eq, Ord, Show)
data SyntaxStack = SyntaxStack [StackStateOrToken] deriving (Eq, Ord, Show)
data Accepted = NotAccepted | Accepted | Failed deriving (Eq, Ord, Show)
data ParserResult = FailedToParse | SuccessInParsing deriving (Eq, Ord, Show)

push :: StackStateOrToken -> SyntaxStack -> SyntaxStack
push stackOrToken (SyntaxStack st) = (SyntaxStack (st ++ [stackOrToken]))

popAndPeek :: SyntaxStack -> (Maybe StackStateOrToken, SyntaxStack)
popAndPeek (SyntaxStack []) = (Nothing, (SyntaxStack []))
popAndPeek (SyntaxStack (x:xs)) = (Just x, (SyntaxStack xs))

pop :: SyntaxStack -> SyntaxStack
pop (SyntaxStack []) = (SyntaxStack [])
pop (SyntaxStack xs) = (SyntaxStack (init xs))

popn :: Int -> SyntaxStack -> SyntaxStack
popn 0 s = s
popn n s = popn (n - 1) (pop s)

getTopOfStack :: SyntaxStack -> Maybe StackStateOrToken
getTopOfStack (SyntaxStack []) = Nothing
getTopOfStack (SyntaxStack xs) = Just (last xs)

createStack :: SyntaxStack
createStack = (SyntaxStack [])

foldOverRegexAlgo :: RegEx -> Terminal -> RegEx
foldOverRegexAlgo (RegEx [] ty) (Terminal s) = (RegEx s ty)
foldOverRegexAlgo (RegEx e ty) (Terminal s) = (RegEx (e ++ ['|'] ++ s) ty)

createCategory :: Char -> CharCategory
createCategory c = [c]

createParserFromCFG :: CFG -> Parser
createParserFromCFG (CFG nonterms terms prodRules startSymbol') =
  let (regex :: RegEx) = Data.Set.foldl' foldOverRegexAlgo (RegEx [] (TokenType "default")) terms
      (nfa :: NFA) = convert regex
      (dfa :: DFA) = convert nfa
      (dfa' :: DFA) = minimize dfa
      (scanner :: Scanner) = convertDFAToScanner dfa' createCategory
      (goal :: LRItem) = getGoalLrItem (CFG nonterms terms prodRules startSymbol')
      ((cc :: CC), (trans :: Transitions)) = createComprehensiveCC terms prodRules goal     
      ((at :: ActionTable), (gt :: GotoTable)) = createTables nonterms prodRules terms goal cc
      (states' :: BottomUpParserStates) = (BottomUpParserStates cc at gt)
      (parser :: Parser) = (Parser scanner states')
  in  parser      
      
parse :: InputStream -> Parser -> (SyntaxStack, InputStream, ParserResult)
parse inStream (Parser scanner states') =
  let (stack :: SyntaxStack) = createStack
      (stack' :: SyntaxStack) = push (St Dollar) stack
      (stack'' :: SyntaxStack) = push (St (StackState 0)) stack'
      (scanner' :: Scanner, word :: Lexeme, inStream' :: InputStream,_) = nextWord scanner inStream
  in  parse' word inStream' scanner' states' stack''

parse' :: Lexeme -> InputStream -> Scanner -> BottomUpParserStates -> SyntaxStack -> (SyntaxStack, InputStream, ParserResult)
parse' word inStream scanner parserStateMachine stack =
  let (result :: Accepted, stack''' :: SyntaxStack, word' :: Lexeme, (scanner'' :: Scanner, inStream'' :: InputStream)) = parseOnce (scanner, inStream) word stack parserStateMachine
  in  case result of
        Accepted -> (stack''', inStream'', SuccessInParsing)
        NotAccepted -> parse' word' inStream'' scanner'' parserStateMachine stack'''
        Failed -> (stack''', inStream'', FailedToParse)
  
parseOnce :: (Scanner, InputStream) -> Lexeme -> SyntaxStack -> BottomUpParserStates -> (Accepted, SyntaxStack, Lexeme, (Scanner, InputStream))
parseOnce (scanner, inStream) word stack (BottomUpParserStates (cc :: CC) (actionTable :: ActionTable) (gotoTable :: GotoTable)) =
  let (topOfStack :: Maybe StackStateOrToken) = getTopOfStack stack
      (word'' :: Terminal) = case word of
        "" -> Terminal "eof"
        _ -> (Terminal word)
  in  case  topOfStack of
    Nothing -> (Failed, stack, word, (scanner, inStream))
    (Just (St (StackState s))) ->
      let (action :: Maybe Action) = (getAction (s, word'') actionTable)
      in  case action of
            (Just (Reduce a' b')) ->
              let (cardinality :: Int) = Data.List.length b'
                  (stack' :: SyntaxStack) = popn (cardinality * 2) stack
                  (Just (St (StackState state))) = getTopOfStack stack'
                  (stack'' :: SyntaxStack) = push (Tok (NonTerm a')) stack'
                  (Just stateInt) = getGoto (state, a') gotoTable
                  (stack''' :: SyntaxStack) = push (St (StackState stateInt)) stack''
              in  (NotAccepted, stack''', word, (scanner, inStream))
            (Just (Shift si)) ->
              let (stack' :: SyntaxStack) = push (Tok (Term (Terminal word))) stack
                  (stack'' :: SyntaxStack) = push (St (StackState si)) stack'
                  (scanner' :: Scanner, word' :: Lexeme, inStream' :: InputStream, _) = nextWord scanner inStream
              in  (NotAccepted, stack'', word', (scanner', inStream'))
            (Just Accept) -> (Accepted, stack, word, (scanner, inStream))
            _ -> (Failed, stack, word, (scanner, inStream))
