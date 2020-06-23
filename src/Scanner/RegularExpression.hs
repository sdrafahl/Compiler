module Scanner.RegularExpression (
  RegEx(..)
  ) where

import Scanner.TokenType

data RegEx = RegEx [Char] TokenType

