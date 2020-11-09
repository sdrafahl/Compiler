{-# LANGUAGE NoImplicitPrelude #-}
module Parser.AttributeMapSpec (spec) where

import Misc.Import
import Misc.Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Map (Map)
import Data.Map
import Data.Set
import Data.List
import Parser.CFG
import Parser.AttributeMap
import Parser.ParserTree

listNonTerm = (NonTerminal "List")
listLefttLeafe = (Terminal "(")
listRightLeafe = (Terminal ")")
listSyntheticPropKey = (NonTerm listNonTerm)
leftLeafeSynth = (Term listLefttLeafe)
rightLeafeSynth = (Term listRightLeafe)
listEvalFunc :: [AttributeValue] -> AttributeValue
listEvalFunc intAttributes = Data.List.foldl' (\(IntAttribute acc) (IntAttribute n) -> (IntAttribute (n + acc))) (IntAttribute 0) intAttributes
attributeSetListA = (AttributeSet (Data.Map.fromList [("list", (SynthesizedAttributeEval "l" listEvalFunc))]))
attributeForLeafe = (AttributeSet (Data.Map.fromList [("l", (SynthesizedAttribute "l" (IntAttribute 1)))]))

attributeMapA = (AttributeGrammarMap (Data.Map.fromList [(NonTerm listNonTerm, attributeSetListA), (Term listLefttLeafe, attributeForLeafe), (Term listRightLeafe, attributeForLeafe)]))
parseTreeA = (ParseTreeNode listNonTerm [(Leafe listLefttLeafe), (Leafe listRightLeafe)])
expectedProcessedIndexMapA = ProcessedIndexMap (Data.Map.fromList [((TreeNodeIndex [0],"list"),IntAttribute 2),((TreeNodeIndex [0,0],"l"),IntAttribute 1),((TreeNodeIndex [0,1],"l"),IntAttribute 1)])

spec :: Spec
spec = do
  describe "AttributeMapSpec" $ do
    describe "evaluateParseTreeWithAttributes" $ do
      describe "Parse Tree A" $ do
        it "Should evaluate parse tree" $ evaluateParseTreeWithAttributes parseTreeA attributeMapA `shouldBe` expectedProcessedIndexMapA
        

