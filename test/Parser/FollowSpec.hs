{-# LANGUAGE NoImplicitPrelude #-}
module Parser.FollowSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import RegularExpression
import NFAtoDFA
import StateMachine
import DFA
import NFA
import Data.Map (Map)
import Data.Map
import Data.Set
import Parser.CFG
import Parser.First
import Parser.Follow

--------------------------- CASE A
-- Term -> [ Term
--      -> ] 
---------------------------
caseAProductions = (Data.Set.fromList [ProductionRule (NonTerminal "Term", [Term (Terminal "["), NonTerm (NonTerminal "Term")]),  ProductionRule (NonTerminal "Term" , [Term (Terminal "]")])])
caseATerminals = (Data.Set.fromList [Terminal "[", Terminal "]"])
caseAFirst = createFirstMap caseAProductions caseATerminals

--------------------------- CASE B
-- Term -> Exp Exp
-- Exp  -> ( Term
---------------------------
caseBProductions = (Data.Set.fromList [ProductionRule (NonTerminal "Term", [NonTerm (NonTerminal "Exp"), NonTerm (NonTerminal "Exp")]), ProductionRule (NonTerminal "Exp", [Term (Terminal "("), NonTerm (NonTerminal "Term")])])
caseBTerminals = (Data.Set.fromList [Terminal "("])
caseBFirst = createFirstMap caseBProductions caseBTerminals


--------------------------- CASE D
-- Term -> ( Exp )
-- Exp  -> +
---------------------------
caseDProductions = (Data.Set.fromList [ProductionRule (NonTerminal "Term", [Term (Terminal "(") ,NonTerm (NonTerminal "Exp"), Term (Terminal ")")]), ProductionRule (NonTerminal "Exp", [Term (Terminal "+")])])
caseDTerminals = (Data.Set.fromList [Terminal "(", Terminal ")", Terminal "+"])
caseDFirst = (createFirstMap caseDProductions caseDTerminals)


spec :: Spec
spec = do
  describe "createFollow" $ do
   it "Should create a Follow for case A" $ do createFollow caseAFirst caseAProductions `shouldBe` Follow (Data.Map.fromList [(NonTerminal "Term",Data.Set.fromList [Terminal "eof"])])
   it "Should create a Follow for case B" $ do createFollow caseBFirst caseBProductions `shouldBe` Follow (Data.Map.fromList [(NonTerminal "Exp",Data.Set.fromList [Terminal "(",Terminal "eof"]),(NonTerminal "Term",Data.Set.fromList [Terminal "(",Terminal "eof"])])
   it "Should create a Follow for case C" $ do createFollow caseDFirst caseDProductions `shouldBe` Follow (Data.Map.fromList [(NonTerminal "Exp",Data.Set.fromList [Terminal ")",Terminal "eof"])])
