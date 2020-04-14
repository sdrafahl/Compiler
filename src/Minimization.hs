{-# LANGUAGE MultiParamTypeClasses #-}
module Minimization where

class Minimization a where
  minimize :: a -> a
