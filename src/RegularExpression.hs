module RegularExpression (
  RegEx(..)
  ) where

import TokenType

data RegEx = RegEx [Char] TokenType

