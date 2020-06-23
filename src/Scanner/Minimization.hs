{-# LANGUAGE MultiParamTypeClasses #-}
module Scanner.Minimization where

class Minimization a where
  minimize :: a -> a
