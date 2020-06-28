{-# LANGUAGE NoImplicitPrelude #-}
module Parser.FirstSpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Map (Map)
import Data.Map
import Data.Set
import Parser.CFG
import Parser.First

--------------------------- CASE A
-- Term -> [ Term
--      -> ] 
---------------------------

--------------------------- CASE B
-- Term -> [ Exp
--      -> ]
-- Exp  -> ( ) Term
---------------------------

--------------------------- CASE C
-- Term -> Exp Exp
-- Exp  -> ( Term
---------------------------
caseCProductions = (Data.Set.fromList [ProductionRule (NonTerminal "Term", [NonTerm (NonTerminal "Exp"), NonTerm (NonTerminal "Exp")]), ProductionRule (NonTerminal "Exp", [Term (Terminal "("), NonTerm (NonTerminal "Term")])])
caseCTerminals = (Data.Set.fromList [Terminal "("])
caseCFirst = createFirstMap caseCProductions caseCTerminals

--------------------------- CASE D
-- Term -> ( Exp )
-- Exp  -> +
---------------------------
caseDProductions = (Data.Set.fromList [ProductionRule (NonTerminal "Term", [Term (Terminal "(") ,NonTerm (NonTerminal "Exp"), Term (Terminal ")")]), ProductionRule (NonTerminal "Exp", [Term (Terminal "+")])])
caseDTerminals = (Data.Set.fromList [Terminal "(", Terminal ")", Terminal "+"])
caseDFirst = (createFirstMap caseDProductions caseDTerminals)


spec :: Spec
spec = do
  describe "createFirstMap" $ do
    it "Should create a First Mapping for case A" $ do createFirstMap (Data.Set.fromList [ProductionRule (NonTerminal "Term", [Term (Terminal "["), NonTerm (NonTerminal "Term")]), ProductionRule (NonTerminal "Term" , [Term (Terminal "]")])]) (Data.Set.fromList [Terminal "[", Terminal "]"]) 
      `shouldBe`
      (First (Data.Map.fromList [(Term (Terminal "EOF"),Data.Set.fromList [Terminal "EOF"]),(Term (Terminal "["),Data.Set.fromList [Terminal "["]),(Term (Terminal "]"), Data.Set.fromList [Terminal "]"]),(Term (Terminal "\948"),Data.Set.fromList [Terminal "\948"]),(NonTerm (NonTerminal "Term"), Data.Set.fromList [Terminal "[",Terminal "]"])]))
    it "Should create a First Mapping for case B" $ do createFirstMap (Data.Set.fromList [ProductionRule (NonTerminal "Term", [Term (Terminal "["), NonTerm (NonTerminal "Exp")]),  ProductionRule (NonTerminal "Term" , [Term (Terminal "]")]), ProductionRule (NonTerminal "Exp", [Term (Terminal "("), Term (Terminal ")"), NonTerm (NonTerminal "Term")])]) (Data.Set.fromList [Terminal "[", Terminal "]", Terminal "(", Terminal ")"]) `shouldBe` First (Data.Map.fromList [(Term (Terminal "("), Data.Set.fromList [Terminal "("]),(Term (Terminal ")"),Data.Set.fromList [Terminal ")"]),(Term (Terminal "EOF"),Data.Set.fromList [Terminal "EOF"]),(Term (Terminal "["),Data.Set.fromList [Terminal "["]),(Term (Terminal "]"), Data.Set.fromList [Terminal "]"]),(Term (Terminal "\948"),Data.Set.fromList [Terminal "\948"]),(NonTerm (NonTerminal "Exp"),Data.Set.fromList [Terminal "("]),(NonTerm (NonTerminal "Term"), Data.Set.fromList [Terminal "[",Terminal "]"])])
    it "Should create a First Mapping for case C" $ do caseCFirst `shouldBe` First (Data.Map.fromList [(Term (Terminal "("),Data.Set.fromList [Terminal "("]),(Term (Terminal "EOF"),Data.Set.fromList [Terminal "EOF"]),(Term (Terminal "\948"),Data.Set.fromList [Terminal "\948"]),(NonTerm (NonTerminal "Exp"),Data.Set.fromList [Terminal "("]),(NonTerm (NonTerminal "Term"),Data.Set.fromList [Terminal "("])])
    it  "Should create a First Mapping for case D" $ do caseDFirst `shouldBe` First (Data.Map.fromList [(Term (Terminal "("),Data.Set.fromList [Terminal "("]),(Term (Terminal ")"),Data.Set.fromList [Terminal ")"]),(Term (Terminal "+"),Data.Set.fromList [Terminal "+"]),(Term (Terminal "EOF"),Data.Set.fromList [Terminal "EOF"]),(Term (Terminal "\948"),Data.Set.fromList [Terminal "\948"]),(NonTerm (NonTerminal "Exp"),Data.Set.fromList [Terminal "+"]),(NonTerm (NonTerminal "Term"),Data.Set.fromList [Terminal "("])])
