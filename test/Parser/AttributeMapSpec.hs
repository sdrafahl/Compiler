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

------------------------------------------------------------ Case A
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
------------------------------------------------------------ Case B

nonTermParent = (NonTerminal "P")
childTerm = (Terminal "x")
inheritedParentKey = (NonTerm nonTermParent)
leafInher = (Term childTerm)
caseBInherFunc :: AttributeValue -> AttributeValue
caseBInherFunc s = s
attributeSetChildren = (AttributeSet (Data.Map.fromList [("P", (Inherited "name" caseBInherFunc))]))
attributeSetParent = (AttributeSet (Data.Map.fromList [("name", (SynthesizedAttribute "name" (StringAttribute "rob")))]))

attributeMapB = (AttributeGrammarMap (Data.Map.fromList [(NonTerm nonTermParent, attributeSetParent), (Term childTerm, attributeSetChildren)]))
parseTreeB = (ParseTreeNode nonTermParent [(Leafe childTerm), (Leafe childTerm), (Leafe childTerm)])
expectedProcessedIndexMapB =  ProcessedIndexMap (Data.Map.fromList [((TreeNodeIndex [0],"name"),StringAttribute "rob"),((TreeNodeIndex [0,0],"P"),StringAttribute "rob"),((TreeNodeIndex [0,1],"P"),StringAttribute "rob"),((TreeNodeIndex [0,2],"P"),StringAttribute "rob")])

spec :: Spec
spec = do
  describe "AttributeMapSpec" $ do
    describe "evaluateParseTreeWithAttributes" $ do
      describe "Parse Tree A" $ do
        it "Should evaluate parse tree A" $ evaluateParseTreeWithAttributes parseTreeA attributeMapA `shouldBe` expectedProcessedIndexMapA
        it "Should evaluate parse tree B" $ evaluateParseTreeWithAttributes parseTreeB attributeMapB `shouldBe` expectedProcessedIndexMapB
        

