{-# LANGUAGE MultiParamTypeClasses #-}
module Scanner.RegularExpressionToNFA (
    NFA (..),
    InputCharacter (..),
    Transition (..),
    REOrNFA (..),
    Conversion(..),
    reToNFA
  ) where

import Data.List
import Data.List.Index
import Data.Char
import Debug.Trace
import System.IO.Unsafe
import Scanner.Conversion
import Scanner.RegularExpression
import Scanner.NFA
import Scanner.StateMachine
import Scanner.TokenType
import Data.Map

instance Show REOrNFA where
  show (FA nfa) = show nfa
  show (NotFA inCharacter) = show inCharacter

data REOrNFA = FA NFA | NotFA Char deriving (Eq)

parenSubStringRange :: [Char] -> [Char] -> Int -> [Char]
parenSubStringRange (')' : input) stack 1 = stack ++ [')']
parenSubStringRange ('(' : input) stack depth = parenSubStringRange input (stack ++ ['(']) (depth + 1)
parenSubStringRange (')' : input) stack depth = parenSubStringRange input (stack ++ [')']) (depth - 1)
parenSubStringRange input stack depth = parenSubStringRange (tail input) (stack ++ [(head input)]) depth

mapSubNFA :: [Char] -> [REOrNFA] -> [REOrNFA]
mapSubNFA [] mapped = mapped
mapSubNFA ('(' : input) mapped =
  let test = '(' : input
      subStringParen = parenSubStringRange ('(' : input) [] 0
      subRe = (tail (Data.List.take ((length subStringParen) - 1) subStringParen))
      nfa = reToNFA (RegEx subRe BadTokenType)
      newMappedStack = mapped ++ [FA nfa]
      lengthToSkip = (length subStringParen)
      in mapSubNFA (Data.List.drop lengthToSkip ('(' : input)) newMappedStack
mapSubNFA input mapped = mapSubNFA (tail input) (mapped ++ [(NotFA (head input))])

mapKleenClosure :: [REOrNFA] -> [REOrNFA]
mapKleenClosure input =
  let indices = (Data.List.map (\index -> index - 1) (elemIndices (NotFA '*') input))
      mappedArray = mapAtIndices indices input
      in Data.List.filter (\va -> va /= (NotFA '*') )  mappedArray
      
mapAtIndices :: [Int] -> [REOrNFA] -> [REOrNFA]
mapAtIndices [] stack = stack
mapAtIndices (index : indices) stack =
  let value = stack!!index
      baseNFA = (case value of
                    (FA fa) -> fa
                    (NotFA notFa) -> baseNfa notFa BadTokenType
                )
      in mapAtIndices indices (setAt index (FA (kleanClosureNfa baseNFA BadTokenType)) stack)

mapOrs :: [REOrNFA] -> [REOrNFA] -> [REOrNFA]
mapOrs input stack
  | length input <= 2 = stack ++ (reverse input)
  | (head (tail input)) == (NotFA '|') =
      let leftNfa = case (head input) of
                      (FA fa) -> fa
                      (NotFA notFA) -> baseNfa notFA BadTokenType
          rightNfa = case (head (tail (tail input))) of
                       (FA fa) -> fa
                       (NotFA notFA) -> baseNfa notFA BadTokenType
          in mapOrs (Data.List.drop 3 input) (FA (orNfa leftNfa rightNfa BadTokenType) : stack) 
  | otherwise = mapOrs (tail input) ((head input) : stack)

mapBaseCharacters :: [REOrNFA] -> [NFA]
mapBaseCharacters inputNfas = Data.List.map (\nfaOrChar -> case nfaOrChar of
                                  (FA fa) -> fa
                                  (NotFA notFa) -> baseNfa notFa BadTokenType
                              ) inputNfas

addTokenType :: NFA -> TokenType -> NFA
addTokenType (NFA states startState terminalStates  transitions  category) category' = (NFA states startState terminalStates transitions (Data.Map.fromList (zip terminalStates (replicate (length terminalStates) category'))))

reToNFA :: RegEx -> NFA
reToNFA (RegEx input category) =
  let mappedToNfas = mapBaseCharacters (mapOrs (mapKleenClosure (mapSubNFA input [])) [])
      combinedNFA = Data.List.foldr (\nfa nextNfa -> andNfa nfa nextNfa BadTokenType) (head mappedToNfas) (tail mappedToNfas)
  in addTokenType combinedNFA category

instance Conversion RegEx NFA where
  convert re = reToNFA re
