{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CharCategoryTable (
  CharCatTable(..),
  InputCharacter,
  CharCategory
  ) where

import Data.Map

type InputCharacter = Char
type CharCategory = String

data CharCatTable = CharCatTable (Map InputCharacter CharCategory) deriving (Eq, Ord)
