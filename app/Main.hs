{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import RIO
import RIO.Process
import Options.Applicative.Simple
import System.IO
import qualified Paths_Scanner


main :: IO ()
main = do
  putStrLn "the compiler"
  
