{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CharCategoryTable (
  CharCatTable(..),
  CharCategory
  ) where

import Data.Map

type CharCategory = String

data CharCatTable = CharCatTable (Map Char CharCategory) deriving (Eq, Ord)
