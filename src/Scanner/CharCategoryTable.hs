{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Scanner.CharCategoryTable (
  CharCatTable(..),
  CharCategory
  ) where

import Data.Map

type CharCategory = String

data CharCatTable = CharCatTable (Map Char CharCategory) deriving (Eq, Ord, Show)
