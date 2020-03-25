{-# LANGUAGE MultiParamTypeClasses #-}
module Conversion where

class Conversion from to where
  convert :: from -> to
