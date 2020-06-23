{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module RecursiveAlgorithms.FixedPoint where

class FixedPointAlgorithm d where
  fixPointOperation :: d -> (d -> d) -> d
