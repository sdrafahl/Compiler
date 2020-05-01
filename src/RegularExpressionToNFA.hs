{-# LANGUAGE MultiParamTypeClasses #-}
module RegularExpressionToNFA (
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
import Conversion
import RegularExpression
import NFA
import StateMachine
import Category
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
      nfa = reToNFA (RegEx subRe NoCategory)
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
                    (NotFA notFa) -> baseNfa notFa NoCategory
                )
      in mapAtIndices indices (setAt index (FA (kleanClosureNfa baseNFA NoCategory)) stack)

mapOrs :: [REOrNFA] -> [REOrNFA] -> [REOrNFA]
mapOrs input stack
  | length input <= 2 = stack ++ (reverse input)
  | (head (tail input)) == (NotFA '|') =
      let leftNfa = case (head input) of
                      (FA fa) -> fa
                      (NotFA notFA) -> baseNfa notFA NoCategory
          rightNfa = case (head (tail (tail input))) of
                       (FA fa) -> fa
                       (NotFA notFA) -> baseNfa notFA NoCategory
          in mapOrs (Data.List.drop 3 input) (FA (orNfa leftNfa rightNfa NoCategory) : stack) 
  | otherwise = mapOrs (tail input) ((head input) : stack)

mapBaseCharacters :: [REOrNFA] -> [NFA]
mapBaseCharacters inputNfas = Data.List.map (\nfaOrChar -> case nfaOrChar of
                                  (FA fa) -> fa
                                  (NotFA notFa) -> baseNfa notFa NoCategory
                              ) inputNfas

addCategory :: NFA -> Category -> NFA
addCategory (NFA states startState terminalStates  transitions  category) category' = (NFA states startState terminalStates transitions (Data.Map.fromList (zip terminalStates (replicate (length terminalStates) category'))))

reToNFA :: RegEx -> NFA
reToNFA (RegEx input category) =
  let mappedToNfas = mapBaseCharacters (mapOrs (mapKleenClosure (mapSubNFA input [])) [])
      combinedNFA = Data.List.foldr (\nfa nextNfa -> andNfa nfa nextNfa NoCategory) (head mappedToNfas) (tail mappedToNfas)
  in addCategory combinedNFA category

instance Conversion RegEx NFA where
  convert re = reToNFA re
