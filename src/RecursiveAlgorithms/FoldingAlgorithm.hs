{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module RecursiveAlgorithms.FoldingAlgorithm where

class FoldingAlgorithm dataToFold init algo where
  foldingAlgorithm :: dataToFold -> init -> algo -> init
