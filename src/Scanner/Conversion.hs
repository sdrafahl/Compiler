{-# LANGUAGE MultiParamTypeClasses #-}
module Scanner.Conversion where

class Conversion from to where
  convert :: from -> to
