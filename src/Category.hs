{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Category (
  Category(..)
  ) where

data Category = Category String | NoCategory deriving (Eq, Show)
