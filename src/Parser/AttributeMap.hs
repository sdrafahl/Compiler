{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.AttributeMap where

import List.List
import Data.Set
import Data.List
import Data.Map
import Debug.Trace
import Parser.CFG
import Parser.ParserTree
import Parser.Parser
import Data.Maybe

data AttributeValue = StringAttribute String | IntAttribute Int | BoolAttribute Bool deriving (Eq, Ord, Show)
data Attribute = SynthesizedAttribute String AttributeValue | SynthesizedAttributeInteralEval [String] ([AttributeValue] -> AttributeValue) | SynthesizedAttributeEval String ([AttributeValue] -> AttributeValue) | Inherited String (AttributeValue -> AttributeValue) | NullAttribute deriving (Eq, Ord, Show)
data AttributeSet = AttributeSet (Map String Attribute) deriving (Eq, Ord, Show)

instance Show ([AttributeValue] -> AttributeValue) where
  show _ = "[AttributeValue] -> AttributeValue"

instance Show (AttributeValue -> AttributeValue) where
  show _ = "AttributeValue -> AttributeValue"


instance Ord ([AttributeValue] -> AttributeValue) where
  compare _ _ = EQ

instance Ord (AttributeValue -> AttributeValue) where
  compare _ _ = EQ

instance Eq ([AttributeValue] -> AttributeValue) where
  (==) _ _ = True

instance Eq (AttributeValue -> AttributeValue) where
  (==) _ _ = True
  

data AttributeGrammarMap = AttributeGrammarMap (Map NonTerminalOrTerminal AttributeSet) deriving (Eq, Ord, Show)

data TreeNodeIndex = TreeNodeIndex [Int] deriving (Eq, Ord, Show)

data ParseTreeIndexMap = ParseTreeIndexMap (Map TreeNodeIndex NonTerminalOrTerminal) deriving (Eq, Ord, Show)

data EnqueuedItem = EnqueuedItem TreeNodeIndex Attribute deriving (Eq, Ord, Show)

data DependencyChain = DependencyChain [EnqueuedItem] | Incomplete | Empty deriving (Eq, Ord, Show)

data PriorityQueue = PriorityQueue [DependencyChain] deriving (Eq, Ord, Show)

data VisitedNodeSet = VisitedNodeSet (Set TreeNodeIndex)

data ProcessedIndexMap = ProcessedIndexMap (Map (TreeNodeIndex, String) AttributeValue)

data ProcessedNodes = ProcessedNodes (Set TreeNodeIndex)

---------------------------------------------------------------------------------------

emptyProcessedNodes :: ProcessedNodes
emptyProcessedNodes = (ProcessedNodes Data.Set.empty)

emptyProcessedMap :: ProcessedIndexMap
emptyProcessedMap = (ProcessedIndexMap Data.Map.empty)

getChains :: PriorityQueue -> [DependencyChain]
getChains (PriorityQueue s) = s

---------------------------------------------------------------------------------------
isProcessed :: TreeNodeIndex -> ProcessedNodes -> Bool
isProcessed index (ProcessedNodes s) = Data.Set.member index s

processed :: TreeNodeIndex -> ProcessedNodes -> ProcessedNodes
processed index (ProcessedNodes s) = (ProcessedNodes (Data.Set.insert index s))

---------------------------------------------------------------------------------------
addValue :: AttributeValue  -> String -> TreeNodeIndex -> ProcessedIndexMap -> ProcessedIndexMap
addValue v key index (ProcessedIndexMap m) = (ProcessedIndexMap (Data.Map.insert (index, key) v m))

processedAt :: ProcessedIndexMap -> (TreeNodeIndex, String) -> Bool
processedAt (ProcessedIndexMap m) key = Data.Map.member key m

getValue :: (TreeNodeIndex, String) -> ProcessedIndexMap -> Maybe AttributeValue
getValue k (ProcessedIndexMap m) = Data.Map.lookup k m

getValues :: [(TreeNodeIndex, String)] -> ProcessedIndexMap -> [Maybe AttributeValue]
getValues keys pim = Data.List.map (\key' -> getValue key' pim) keys
                         
visited :: VisitedNodeSet -> TreeNodeIndex -> Bool
visited (VisitedNodeSet s) v = Data.Set.member v s

markAsVisited :: VisitedNodeSet -> TreeNodeIndex -> VisitedNodeSet
markAsVisited (VisitedNodeSet s) d = (VisitedNodeSet (Data.Set.insert d s))

---------------------------------------------------------------------------------------

getItemsInOrder :: DependencyChain -> [EnqueuedItem]
getItemsInOrder (DependencyChain l) = reverse l
getItemsInOrder Incomplete = []
getItemsInOrder Empty = []

getDependentKeys :: AttributeSet -> [String]
getDependentKeys (AttributeSet m) =
  let (attList :: [(String, Attribute)]) = Data.Map.toList m
  in  Data.List.intercalate [] (Data.List.intercalate [] (Data.List.map (\(k, att) -> gkey att) attList))
  where
    gkey (SynthesizedAttribute _ _) = []
    gkey (SynthesizedAttributeInteralEval k _) = [k]
    gkey (SynthesizedAttributeEval k _) = [[k]]
    gkey (Inherited k _) = [[k]]
    gkey NullAttribute = []

getKeys :: AttributeSet -> [String]
getKeys (AttributeSet m) = keys m

getAttributes :: AttributeSet -> [(String, Attribute)]
getAttributes (AttributeSet m) = Data.Map.toList m

hasKey :: String -> AttributeSet -> Bool
hasKey key (AttributeSet m) = case (Data.Map.lookup key m) of
  Nothing -> False
  Just _ -> True

getAttribute :: String -> AttributeSet -> Maybe Attribute
getAttribute key (AttributeSet m) = Data.Map.lookup key m

doesIndexHaveKey :: String -> TreeNodeIndex -> AttributeGrammarMap -> ParseTreeIndexMap -> Bool
doesIndexHaveKey key index agm ptm = case (searchIndexForAttributeSet agm ptm index) of
  Nothing -> False
  Just attSet -> hasKey key attSet

emptyQueue :: PriorityQueue
emptyQueue = (PriorityQueue [])
                                    
---------------------------------------------------------------------------------------

searchAttributeGrammarMap :: AttributeGrammarMap -> NonTerminalOrTerminal -> Maybe AttributeSet
searchAttributeGrammarMap (AttributeGrammarMap m) g = Data.Map.lookup g m

searchParseTreeIndexMap :: ParseTreeIndexMap -> TreeNodeIndex -> Maybe NonTerminalOrTerminal
searchParseTreeIndexMap (ParseTreeIndexMap m) index = Data.Map.lookup index m

getAllIndicies :: ParseTreeIndexMap -> [TreeNodeIndex]
getAllIndicies (ParseTreeIndexMap m) = Data.List.map (\(k, _) -> k) (Data.Map.toList m)

---------------------------------------------------------------------------------------

getStartOfChain :: DependencyChain -> EnqueuedItem
getStartOfChain (DependencyChain (x:xs)) = x

getLastItemOfChain :: DependencyChain -> EnqueuedItem
getLastItemOfChain (DependencyChain xs) = last xs

addItemToQueue :: DependencyChain -> EnqueuedItem -> DependencyChain
addItemToQueue (DependencyChain b) item = (DependencyChain (b ++ [item]))

addIndexToQueue :: DependencyChain -> TreeNodeIndex -> Attribute -> DependencyChain
addIndexToQueue c index att = addItemToQueue c (EnqueuedItem index att)

emptyChain :: DependencyChain
emptyChain = (DependencyChain [])

combineChains :: DependencyChain -> DependencyChain -> DependencyChain
combineChains (DependencyChain a) (DependencyChain b) = (DependencyChain (a ++ b))

combineDependencyChains :: [DependencyChain] -> DependencyChain
combineDependencyChains chains = Data.List.foldl' combineDependencyChainsFold emptyChain chains
combineDependencyChainsFold :: DependencyChain -> DependencyChain -> DependencyChain
combineDependencyChainsFold accChain chain = combineChains accChain chain

---------------------------------------------------------------------------------------

getIndex :: EnqueuedItem -> TreeNodeIndex
getIndex (EnqueuedItem i _) = i

getAttributeFrom :: EnqueuedItem -> Attribute
getAttributeFrom (EnqueuedItem _ a) = a

---------------------------------------------------------------------------------------

searchForChildrenIndicies :: TreeNodeIndex -> ParseTreeIndexMap -> [TreeNodeIndex]
searchForChildrenIndicies parentIndex map = searchForChildrenIndicies' (incraTreeNodeIndex parentIndex 0) map []

searchForChildrenIndicies' :: TreeNodeIndex -> ParseTreeIndexMap -> [TreeNodeIndex] -> [TreeNodeIndex]
searchForChildrenIndicies' tni m acc =
  case (searchParseTreeIndexMap m tni) of
    Nothing -> acc
    Just _ -> searchForChildrenIndicies' (incraLastItemTreeNodeIndex tni) m (acc ++ [tni])

searchForParentOfIndex :: TreeNodeIndex -> ParseTreeIndexMap -> Maybe TreeNodeIndex
searchForParentOfIndex (TreeNodeIndex l) m =
  let (potentialParentIndex :: TreeNodeIndex) = (TreeNodeIndex (Data.List.drop ((length l) - 1) l))
      (mntot :: Maybe NonTerminalOrTerminal) = searchParseTreeIndexMap m potentialParentIndex
  in  case mntot of
        Nothing -> Nothing
        Just _ -> Just potentialParentIndex

---------------------------------------------------------------------------------------

findChildWithIndex :: Int -> [ParserTree] -> Maybe ParserTree
findChildWithIndex 0 (x:_) = Just x
findChildWithIndex _ [] = Nothing
findChildWithIndex index (_:xs) = findChildWithIndex (index - 1) xs

searchTree :: TreeNodeIndex -> ParserTree -> Maybe ParserTree
searchTree (TreeNodeIndex []) pt = Just pt
searchTree (TreeNodeIndex (_:_)) (Leafe _) = Nothing
searchTree (TreeNodeIndex (x:xs)) (ParseTreeNode _ children) = case (findChildWithIndex x children) of
  Nothing -> Nothing
  Just children' -> searchTree (TreeNodeIndex xs) children'

incraTreeNodeIndex :: TreeNodeIndex -> Int -> TreeNodeIndex
incraTreeNodeIndex (TreeNodeIndex n) index = (TreeNodeIndex (n ++ [index]))

incraLastItemTreeNodeIndex :: TreeNodeIndex -> TreeNodeIndex
incraLastItemTreeNodeIndex (TreeNodeIndex n) =
  let (newLastNumber :: Int) = (last n) + 1
      (n' :: [Int]) = Data.List.drop ((length n) - 1) n
  in  incraTreeNodeIndex (TreeNodeIndex n') newLastNumber

decrementTreeNodeIndex :: TreeNodeIndex -> TreeNodeIndex
decrementTreeNodeIndex (TreeNodeIndex n) =
  let (newLastNumber :: Int) = minimum [(last n) - 1, 0]
      (n' :: [Int]) = Data.List.drop ((length n) - 1) n
  in  incraTreeNodeIndex (TreeNodeIndex n') newLastNumber  
      
insertTreeIndexMap :: ParseTreeIndexMap -> TreeNodeIndex -> NonTerminalOrTerminal -> ParseTreeIndexMap
insertTreeIndexMap (ParseTreeIndexMap m) k d = (ParseTreeIndexMap (Data.Map.insert k d m))
        
---------------------------------------------------------------------------------------

createParseTreeIndexMap :: ParserTree -> ParseTreeIndexMap
createParseTreeIndexMap pt =
  let (newTreeNodeIndex :: TreeNodeIndex) = (TreeNodeIndex [0])
      (grammarOfRoot :: NonTerminalOrTerminal) = getGrammarFromParseTree pt
      (pim :: ParseTreeIndexMap) = (ParseTreeIndexMap (Data.Map.fromList [(newTreeNodeIndex, grammarOfRoot)]))
  in  createParseTreeIndexMap' pt newTreeNodeIndex pim 
      

createParseTreeIndexMap' :: ParserTree -> TreeNodeIndex -> ParseTreeIndexMap -> ParseTreeIndexMap
createParseTreeIndexMap' pt tni ptm =
  let (children :: [ParserTree]) = getChildrenInTree pt
      (foldingAlgo :: ((ParseTreeIndexMap, Int) -> ParserTree -> (ParseTreeIndexMap, Int))) = foldOverParseTree tni
      (foldingOverChildrenAlgo :: (ParseTreeIndexMap, Int) -> ParserTree -> (ParseTreeIndexMap, Int)) = foldOverParseTreeChildren tni
      (ptm' :: ParseTreeIndexMap, _) = Data.List.foldl' foldingAlgo (ptm, 0) children
      (ptm'' :: ParseTreeIndexMap, _) = Data.List.foldl' foldingOverChildrenAlgo (ptm', 0) children      
  in  ptm''

foldOverParseTree :: TreeNodeIndex -> (ParseTreeIndexMap, Int) -> ParserTree -> (ParseTreeIndexMap, Int)
foldOverParseTree parentIndex (ptimacc, count) pt = (insertTreeIndexMap ptimacc (incraTreeNodeIndex parentIndex count) (getGrammarFromParseTree pt), count + 1)

foldOverParseTreeChildren :: TreeNodeIndex -> (ParseTreeIndexMap, Int) -> ParserTree -> (ParseTreeIndexMap, Int)
foldOverParseTreeChildren tni (ptms, n) pt = ((createParseTreeIndexMap' pt (incraTreeNodeIndex tni n) ptms), n + 1)

searchIndexForAttributeSet :: AttributeGrammarMap -> ParseTreeIndexMap -> TreeNodeIndex -> Maybe AttributeSet
searchIndexForAttributeSet agm ptm index = case (searchParseTreeIndexMap ptm index) of
  Nothing -> Nothing
  Just g -> searchAttributeGrammarMap agm g

isDependent :: TreeNodeIndex -> TreeNodeIndex -> AttributeGrammarMap -> ParseTreeIndexMap -> Bool
isDependent from to agm ptm =
  let (maybeAttributesFrom :: Maybe AttributeSet) = searchIndexForAttributeSet agm ptm from
      (maybeAttributesTo :: Maybe AttributeSet) = searchIndexForAttributeSet agm ptm to
  in  case (maybeAttributesFrom, maybeAttributesTo) of
        (Nothing, _) -> False
        (_, Nothing) -> False
        (Just attributeSetFrom, Just attributeSetTo) ->
          let (keysItDependsOn :: [String]) = getDependentKeys attributeSetFrom
              (keysInToNode :: [String]) = getKeys attributeSetTo
          in  hasSharedValue keysItDependsOn keysInToNode

dependsOn :: Attribute -> TreeNodeIndex -> AttributeGrammarMap -> ParseTreeIndexMap -> Bool
dependsOn (SynthesizedAttribute _ _ ) _ _ _ = False
dependsOn (SynthesizedAttributeInteralEval _ _) _ _ _ = False
dependsOn (SynthesizedAttributeEval key _) index agm ptm = case (searchIndexForAttributeSet agm ptm index) of
  Nothing -> False
  Just attSet -> hasKey key attSet
dependsOn (Inherited key _) index agm ptm = case (searchIndexForAttributeSet agm ptm index) of
  Nothing -> False
  Just attSet -> hasKey key attSet
dependsOn NullAttribute _ _ _ = False

getDependsOn :: String -> TreeNodeIndex -> AttributeGrammarMap -> ParseTreeIndexMap -> Maybe Attribute
getDependsOn key index agm ptm = case (searchIndexForAttributeSet agm ptm index) of
  Nothing -> Nothing
  Just attSet -> getAttribute key attSet
              
---------------------------------------------------------------------------------------

hasAMemeber :: [TreeNodeIndex] -> Set TreeNodeIndex -> Bool
hasAMemeber indicies sti = hasAMemeber' indicies sti False

hasAMemeber' :: [TreeNodeIndex] -> Set TreeNodeIndex -> Bool -> Bool
hasAMemeber' _ _ True = True
hasAMemeber' (x:xs) s False = hasAMemeber' xs s (Data.Set.member x s)

addVisited :: [TreeNodeIndex] -> Set TreeNodeIndex -> Set TreeNodeIndex
addVisited indicies s = Data.List.foldl' (\set index -> Data.Set.insert index set) s indicies
  
---------------------------------------------------------------------------------------

getDependencyChain :: ParseTreeIndexMap -> AttributeGrammarMap -> TreeNodeIndex -> String -> DependencyChain
getDependencyChain ptim agm index key =
  case (searchIndexForAttributeSet agm ptim index) of
    Nothing -> (DependencyChain [])
    Just attSet ->
      case (getAttribute key attSet) of
        Nothing -> (DependencyChain [])
        Just att -> getDependencyChain' ptim agm Data.Set.empty (DependencyChain [(EnqueuedItem index att)])      
           
getDependencyChain' :: ParseTreeIndexMap -> AttributeGrammarMap -> Set TreeNodeIndex -> DependencyChain -> DependencyChain
getDependencyChain' ptim agm visitedNodes dcacc =
  let (lastItemInChain :: EnqueuedItem) = getLastItemOfChain dcacc
      (att :: Attribute) = getAttributeFrom lastItemInChain 
      (currentNode :: TreeNodeIndex) = getIndex lastItemInChain      
  in  case att of
        (SynthesizedAttribute _ _) -> dcacc
        (SynthesizedAttributeInteralEval _ _) -> dcacc
        (SynthesizedAttributeEval key _) -> 
          let (children :: [TreeNodeIndex]) = searchForChildrenIndicies currentNode ptim
              (children' :: [TreeNodeIndex]) = Data.List.filter (\index -> dependsOn att index agm ptim) children
              (children'' :: [(Maybe Attribute, TreeNodeIndex)]) = Data.List.map (\index -> (getDependsOn key index agm ptim, index)) children'              
              (visitedNodes' :: Set TreeNodeIndex) = addVisited (Data.List.map (\(_, b) -> b) children'') visitedNodes
              (childRoots :: [DependencyChain]) = Data.List.map (\(Just attr, index) -> getDependencyChain' ptim agm visitedNodes' (addIndexToQueue emptyChain index attr)) children''
              (childRoot :: DependencyChain) = combineDependencyChains childRoots
              (newChain :: DependencyChain) = combineChains dcacc childRoot
          in  newChain                    
        (Inherited key _) ->
          let (maybeParentindex :: Maybe TreeNodeIndex) = searchForParentOfIndex currentNode ptim
          in  case maybeParentindex of
            Nothing -> Incomplete
            Just parentIndex -> case ((hasAMemeber [parentIndex] visitedNodes), doesIndexHaveKey key parentIndex agm ptim) of
              (True, _) -> Incomplete
              (_, False) -> Incomplete              
              (False, True) ->
                case (getDependsOn key parentIndex agm ptim) of
                  Nothing -> Incomplete
                  Just att -> getDependencyChain' ptim agm (addVisited [parentIndex] visitedNodes) (addItemToQueue dcacc (EnqueuedItem parentIndex att))                
        NullAttribute -> dcacc
              

---------------------------------------------------------------------------------------      

createPriorityQueue :: ParseTreeIndexMap -> AttributeGrammarMap -> PriorityQueue
createPriorityQueue ptim agm =
  let (allTreeIndicies :: [TreeNodeIndex]) = getAllIndicies ptim
      (allTreeIndiciesAndAttributes :: [(TreeNodeIndex, Maybe AttributeSet)]) = Data.List.map (\index -> (index, searchIndexForAttributeSet agm ptim index)) allTreeIndicies
      (allTreeIndiciesAndAttributesAndkeys :: [(TreeNodeIndex, Maybe AttributeSet, Maybe [String])]) = Data.List.map (\(index, mba) -> (index, mba, fmap (\at -> getKeys at) mba)) allTreeIndiciesAndAttributes
      (chains :: [Maybe [DependencyChain]]) = Data.List.map (\(index, maybeatt, maybeKeys) -> fmap (\keys -> (Data.List.map (\key -> getDependencyChain ptim agm index key) keys)) maybeKeys) allTreeIndiciesAndAttributesAndkeys
      (validChains :: [Maybe [DependencyChain]]) = Data.List.filter (\maybeChain -> isJust maybeChain) chains
      (chainsOfChains :: [[DependencyChain]]) = Data.List.map (\(Just c) -> c) validChains
      (chain :: [DependencyChain]) = intercalate [] chainsOfChains
  in  (PriorityQueue chain)



-- need to eliminate internal(to the nodes) cycles before this is called
processAttribute :: String -> Attribute -> TreeNodeIndex -> AttributeGrammarMap -> ParseTreeIndexMap -> ProcessedIndexMap -> AttributeSet -> ProcessedIndexMap
processAttribute key (SynthesizedAttribute _ value) index agm ptm pm _ = addValue value key index pm 
processAttribute keyOfAtt (SynthesizedAttributeInteralEval keys eval) index agm ptm pm as =
  let (attValues :: [(Maybe AttributeValue, String)]) = Data.List.map (\keyForValue -> ((getValue (index, keyForValue) pm), keyForValue)) keys
      (attsNeedTobeEvaluated :: [String]) = Data.List.map (\(Nothing, d) -> d) (Data.List.filter (\(maybeAtt, key) -> isNothing maybeAtt) attValues)
      (attsNeedTobeEval :: [(String, Attribute)]) = Data.List.map (\(k', Just a') -> (k', a')) (Data.List.filter (\(k, a) -> isJust a) (Data.List.map (\key' -> (key' ,getAttribute key' as)) attsNeedTobeEvaluated)) 
      (pm' :: ProcessedIndexMap) = Data.List.foldl' (\pm'' (key', att) -> processAttribute key' att index agm ptm pm'' as) pm attsNeedTobeEval
      (attsAlreadyEvaluated :: [AttributeValue]) = Data.List.map (\(Just a, _) -> a) (Data.List.filter (\(evaluatedAtt, _) -> isJust evaluatedAtt) attValues)
      (maybeValues :: [Maybe AttributeValue]) = getValues (Data.List.map (\keyOfAtt -> (index, keyOfAtt)) attsNeedTobeEvaluated) pm'
      (values :: [AttributeValue]) = Data.List.map (\(Just a) -> a) (Data.List.filter (\matv -> isJust matv) maybeValues)
      (allValues :: [AttributeValue]) = values ++ attsAlreadyEvaluated
      (evaluatedVal :: AttributeValue) = eval allValues      
  in  addValue evaluatedVal keyOfAtt index pm'
processAttribute keyOfAtt (SynthesizedAttributeEval key eval) index agm ptm pm as =
  let (childIndicies :: [(TreeNodeIndex, String)]) = Data.List.map (\index' -> (index, key)) (searchForChildrenIndicies index ptm)
      (childValues :: [AttributeValue]) = Data.List.map (\(Just m) -> m) (Data.List.filter (\f -> isJust f) (getValues childIndicies pm))
      (newValue :: AttributeValue) = eval childValues
      (pm' :: ProcessedIndexMap) = addValue newValue keyOfAtt index pm
  in  pm'
processAttribute keyOfAtt (Inherited key eval) index agm ptm pm as =
  case (searchForParentOfIndex index ptm) of
    Nothing -> pm
    Just parentIndex ->
      case (getValue (parentIndex, key) pm) of
        Nothing -> pm
        Just parentValue ->
          let (newValue :: AttributeValue) = eval parentValue
              (pm' :: ProcessedIndexMap) = addValue newValue keyOfAtt index pm
          in  pm'
processAttribute _ NullAttribute _ _ _ pm _ = pm
      
      

processAttributeSet :: AttributeSet -> TreeNodeIndex -> AttributeGrammarMap -> ParseTreeIndexMap -> ProcessedIndexMap -> ProcessedNodes -> (ProcessedIndexMap, ProcessedNodes)
processAttributeSet attSet index agm ptm pm pn =
  let (attsAndKeys :: [(String, Attribute)]) = getAttributes attSet
      (pm' :: ProcessedIndexMap) = Data.List.foldl' (\pm'' (k, att) -> processAttribute k att index agm ptm pm attSet) pm attsAndKeys
      (pn' :: ProcessedNodes) = processed index pn
  in  (pm', pn')
      
      
processNode :: TreeNodeIndex -> ProcessedNodes -> ProcessedIndexMap -> AttributeGrammarMap -> ParseTreeIndexMap -> (ProcessedIndexMap, ProcessedNodes)
processNode index pn pim agm ptm
  | not (isProcessed index pn) =
    case (searchIndexForAttributeSet agm ptm index) of
      Nothing -> (pim, pn)
      Just attSet -> processAttributeSet attSet index agm ptm pim pn        
  | otherwise                  = (pim, pn)

processChain :: DependencyChain -> AttributeGrammarMap -> ParseTreeIndexMap -> ProcessedIndexMap -> ProcessedNodes -> (ProcessedIndexMap, ProcessedNodes)
processChain Incomplete _ _ processed pset = (processed, pset)
processChain Empty _ _ processed pset = (processed, pset)
processChain chain agm ptim processed pset = 
  let (itemsToProcess :: [EnqueuedItem]) = getItemsInOrder chain
      (processedChainResults :: (ProcessedIndexMap, ProcessedNodes)) = Data.List.foldl' (\(pimacc, pnacc) enqItem -> processNode (getIndex enqItem) pnacc pimacc agm ptim) (processed, pset) itemsToProcess
  in  processedChainResults

processQueue :: PriorityQueue -> AttributeGrammarMap -> ParseTreeIndexMap -> (ProcessedIndexMap, ProcessedNodes)
processQueue queue agm ptm =
  let (processedValues :: ProcessedIndexMap) = emptyProcessedMap
      (processedSet :: ProcessedNodes) = emptyProcessedNodes
      (chains :: [DependencyChain]) = getChains queue
  in  Data.List.foldl' (\(pim, pn) chain -> processChain chain agm ptm pim pn) (processedValues, processedSet) chains
 
      
      
      

  
