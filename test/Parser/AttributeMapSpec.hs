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
------------------------------------------------------------ Case B

nonTermParent0 = (NonTerminal "P0")
nonTermParent1 = (NonTerminal "P1")
child3 = (Terminal "P2")
nonTermParent0Key = (NonTerm nonTermParent0)
nonTermParent1Key = (NonTerm nonTermParent1)
nonTermParent2Key = (Term child3)

evalParentC :: AttributeValue -> AttributeValue
evalParentC s = s
evalChild2C :: [AttributeValue] -> AttributeValue
evalChild2C (x:xs) = x
evalChild2C [] = (StringAttribute "")
evalChild1C :: [AttributeValue] -> AttributeValue
evalChild1C xs = Data.List.foldl' (\(StringAttribute acc) (StringAttribute c) -> (StringAttribute (acc ++ c)) ) (StringAttribute "") xs

attributeSetParent0 = (AttributeSet (Data.Map.fromList [("name", (SynthesizedAttribute "name" (StringAttribute "parent 0")))]))
attributeSetParent1 = (AttributeSet (Data.Map.fromList [("parentName", (Inherited "name" evalParentC)),("childName" ,(SynthesizedAttributeEval "name" evalChild2C)),("name" ,(SynthesizedAttributeInteralEval ["childName", "parentName"] evalChild1C))]))
attributeSetParent2 = (AttributeSet (Data.Map.fromList [("name", (SynthesizedAttribute "name" (StringAttribute "parent 2")))]))
attributeMapC = (AttributeGrammarMap (Data.Map.fromList [(NonTerm nonTermParent0, attributeSetParent0), (NonTerm nonTermParent1, attributeSetParent1), (Term child3, attributeSetParent2)]))
parseTreeC = (ParseTreeNode nonTermParent0 [(ParseTreeNode nonTermParent1 [(Leafe child3)])])
expectedProcessedIndexMapC =  ProcessedIndexMap (Data.Map.fromList [((TreeNodeIndex [0],"name"),StringAttribute "parent 0"),((TreeNodeIndex [0,0],"childName"),StringAttribute "parent 2"),((TreeNodeIndex [0,0],"name"),StringAttribute "parent 0parent 2"),((TreeNodeIndex [0,0],"parentName"),StringAttribute "parent 0"),((TreeNodeIndex [0,0,0],"name"),StringAttribute "parent 2")])




spec :: Spec
spec = do
  describe "AttributeMapSpec" $ do
    describe "evaluateParseTreeWithAttributes" $ do
      describe "Parse Tree A" $ do        
        it "Should evaluate parse tree C" $ evaluateParseTreeWithAttributes parseTreeC attributeMapC `shouldBe` expectedProcessedIndexMapC
        it "Should evaluate parse tree A" $ evaluateParseTreeWithAttributes parseTreeA attributeMapA `shouldBe` expectedProcessedIndexMapA
        it "Should evaluate parse tree B" $ evaluateParseTreeWithAttributes parseTreeB attributeMapB `shouldBe` expectedProcessedIndexMapB        



