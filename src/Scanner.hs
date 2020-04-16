{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scanner (Scanner(..)) where

import Data.Map
import StateMachine

type DFATable = Map State (Map Char State)

class Scanner where
  isValid :: String -> Bool

