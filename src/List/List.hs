{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module List.List where

class ThereIsAnItemInOneListThatIsInTheOther a where
  hasSharedValue :: [a] -> [a] -> Bool

instance (Eq a) => ThereIsAnItemInOneListThatIsInTheOther a where
  hasSharedValue al bl = alistconb al bl

alistconb :: (Eq a) => [a] -> [a] -> Bool
alistconb [] _ = False
alistconb (x:xs) blist
  | elem x blist = True
  | not (elem x blist) = alistconb xs blist
alistconb _ _ = False
