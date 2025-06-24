-- |
-- Module      :  Coconut.Graph.Dot
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- FIXME add module description for Coconut.Graph.Dot

{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, RecordWildCards #-}
module Coconut.Graph.Dot ( DotCompilationOpts(..)
                         , DFGOpts(..), GraphCompileOpts(..), ShowConstantOpts(..)
                         , fglToDot, fglToDotString
                         , fglToDotGeneric, showDot, dotCompilation
                         , dotCompilationWithOpts
                         , dotStringScheduleFormatted
                         , dotStringScheduleWithRegMapFormatted
                         , dotScheduleCompilation
                         , dotScheduleCompilationWithRegMap
                         , dotCompilationLinkedDFGs
                         , dotCompilationInterference
                         , dotCompileIIGraph
                         , prettyCodeGraph
                         , prettyCFGraph
                         , prettyDFGraph ) where

import Control.Monad
import qualified Data.Graph.Inductive as FGL
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Short as BS
import Data.Bifunctor
import Data.Either (fromRight)
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import System.Process
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IM
import qualified Data.Graph.Inductive.PatriciaTree as FGLT

import Data.Fixed (mod')
import Coconut.BaseTypes
import Coconut.CodeGen (codeGenCG)
import Coconut.Utils.CGWrappers
import Coconut.Utils.ArbFloat
import Coconut.Graph.CodeGraph
import Coconut.Core.CoreISA
import Coconut.Core.ControlISA (compose,branch,doWhile)
import Coconut.Core.CoreHardware (CORE)
import Coconut.Core.MetaData
import Coconut.Core.Printer
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Graph.Utils.Dot
import Coconut.Schedule
import Coconut.RegisterAllocator


data DFGOpts = NoTimeNodes | WithTimeNodes deriving (Show, Eq)

data GraphCompileOpts = NoCompile | CompileWithDot deriving (Show, Eq)

data ShowConstantOpts = NoShowConstants | ShowConstants deriving (Show, Eq)

data DotCompilationOpts = DotCompilationOpts
  { dfgOpts :: DFGOpts
  , graphCompileOpts :: GraphCompileOpts
  , showConstantOpts :: ShowConstantOpts
  }
  deriving (Show, Eq)

defaultDotCompilationOpts =
  DotCompilationOpts
    { dfgOpts = WithTimeNodes
    , graphCompileOpts = CompileWithDot
    , showConstantOpts = ShowConstants
    }

data IINode h = IINode
  { dfNode :: DFNode h
  , hVal :: Double -- since S, V are fixed, and 0.0 <= h <= 1.0
  , latencyDepthNum :: Int
  , dispatchTimeRounded :: Int
  }

data IIEdge = IIEdge
  { dfEdge :: DFEdge
  , hValEdge :: Double
  } deriving Show

type IIGraph h = FGL.Gr (IINode h) IIEdge

data DFNodeWithDispatch h = BaseNode (DFNode h) | DispatchNode DispatchTime

data DFEdgeWithDispatch = BaseEdge DFEdge | DispatchEdge
  deriving Show

type DataFlowGraphWithDispatch h = FGL.Gr (DFNodeWithDispatch h) DFEdgeWithDispatch

instance (Show (RegType h)) => Show (DFNodeWithDispatch h) where
  show (BaseNode dfn) = show dfn
  show (DispatchNode dt) = show dt

-- Linked DataFlow graphs
data LinkDFGEdges = LinkBaseEdge DFEdge | LinkEdge
  deriving Show

type LinkedDFGs h = FGL.Gr (DFNode h) LinkDFGEdges

-- |Generate a Dot graph using the show instances of the node and edge labels as displayed graph labels
fglToDot :: (Show a, Show b, FGL.Graph gr) => gr a b -> Dot ()
fglToDot gr = fglToDotGeneric gr show show (const $ const []) (const [])
-- fglToDot gr = fglToDotGeneric gr (const "") show (const $ const []) (const [])

-- |Generate a Dot graph using the Node and Edge strings as labels
fglToDotString :: FGL.Graph gr => gr String String -> Dot ()
fglToDotString gr = fglToDotGeneric gr (const "") id (const $ const []) (const [])

-- |Generate a Dot graph without any edge or node labels
--fglToDotUnlabeled :: FGL.Graph gr => gr a b -> Dot ()
--fglToDotUnlabeled gr = fglToDotGeneric gr undefined undefined (const $ const []) (const [])

-- |Generate a Dot graph using the provided functions to mutate the node labels, edge labels and list of attributes.
fglToDotGeneric ::
     (FGL.Graph gr, Show b)
  => gr a b
  -> (Int -> String)
  -> (b -> String)
  -> (Int -> a -> [(String,String)])
  -> (b -> [(String,String)])
  -> Dot ()
fglToDotGeneric gr nodeConv edgeConv attrNodesConv attrEdgesConv = do
  let es = FGL.labEdges gr -- :: [(Int, Int, b)]
      ns = FGL.labNodes gr -- :: [(Int, a)]
  mapM_ (\(n,p) -> userNode (userNodeId n) (attrNodesConv n p ++ [("label", nodeConv n)])) ns
  mapM_ (\(a,b,p) -> edge (userNodeId a) (userNodeId b) (attrEdgesConv p ++ [("label", edgeConv p)])) es

-- | Print a @CodeGraph h@ as a prettified @[String]@ 
prettyCodeGraph :: Show (RegType h) => CodeGraph h -> [String]
prettyCodeGraph cg@(CodeGraph cfGraph dfGraphs cgIn cgOut cgMR _ cgTags maxID dbgMap) =
  let
     pCFGraph = prettyCFGraph cfGraph
     pDFGraphs = concatMap prettyDFGraph dfGraphs
     pCoalesces = registerCoalesceMap cg

  in ["cgIn: " ++ show cgIn ++ "  cgOut: " ++ show cgOut]
     ++ pCFGraph
     ++ pDFGraphs
     ++ ["CGTags"]
     ++ ["  "++show cgTags]
     ++ ["Register Coalesce Map"]
     ++ ["  "++show pCoalesces]
     ++ ["MRTables"]
     ++ ["  "++show (map (\(lbl,n,_) -> (lbl,n)) cgMR)]

-- | Print a @ControlFlowGraph h@ as a prettified @[String]@
prettyCFGraph :: Show (RegType h) => ControlFlowGraph h -> [String]
prettyCFGraph (ControlFlowGraph cfGraph cfIn cfOut) =
  let
    indentShow ss = "  "++show ss
    cfNodes = map indentShow $ FGL.labNodes cfGraph
    cfEdges = map indentShow $ FGL.labEdges cfGraph
  in ["ControlFlowGraph"]
     ++ ["ControlFlow Nodes"]
     ++ cfNodes
     ++ ["ControlFlow Edges"]
     ++ cfEdges
     ++ ["ControlFlow In/Out"]
     ++ [indentShow cfIn]
     ++ [indentShow cfOut]

-- | Print a @DataFlowGraph h@ with its label as a prettified @[String]@
prettyDFGraph :: Show (RegType h) => (String,DataFlowGraph h) -> [String]
prettyDFGraph (dfLabel,DataFlowGraph dfGraph dfIns dfOuts dfOverwrites dfIncrement dfStage) =
  let
    indentShow ss = "  "++show ss
    dfNodes = map indentShow $ FGL.labNodes dfGraph
    dfEdges = map indentShow $ FGL.labEdges dfGraph
  in ["DataFlowGraph: "++dfLabel]
     ++ ["DataFlow Nodes: "++dfLabel]
     ++ dfNodes
     ++ ["DataFlow Edges: "++dfLabel]
     ++ dfEdges
     ++ ["DataFlow Inputs: "++dfLabel]
     ++ [indentShow dfIns]
     ++ ["DataFlow Outputs: "++dfLabel]
     ++ [indentShow dfOuts]
     ++ ["DataFlow Overwrites: "++dfLabel]
     ++ [indentShow dfOverwrites]
     ++ ["DataFlow Increment: "++dfLabel]
     ++ [indentShow dfIncrement]
     ++ ["DataFlow Stage: "++dfLabel]
     ++ [indentShow dfStage]

labelDFEdgeType :: DFEdge -> String
labelDFEdgeType (DFEdge n) = show n

-- | Compile a control flow graph to dot format
cfgToDot :: FGL.Graph gr => gr CFNode (CFEdge h) -> Dot ()
cfgToDot cfg = fglToDotGeneric cfg show show
  (\nodeID cfNode ->
      case cfNode of
        CFNode tags -> [("xlabel",show tags)]
        CFBranchNode _ tags -> [("xlabel",show tags)]
        CFJoinNode tags -> [("xlabel",show tags)]) (const [])

-- | Label @Node@ using the @DataFlowGraph h@ as context
labelFromNodeID :: Show (RegType h) => DotCompilationOpts -> DataFlowGraph h -> FGL.Node -> String
labelFromNodeID opts gr nd = case FGL.lab (dataFlowGraph gr) nd of
  Just nodeLabel -> labelDFNodeType opts nd nodeLabel
  Nothing        -> ""

-- | Label @DFNode h@ according to its type
labelDFNodeType :: Show (RegType h) => DotCompilationOpts -> Int -> DFNode h -> String
labelDFNodeType _ idx (ResourceNode (RegisterRes r)) = show r ++ " " ++ show idx
labelDFNodeType _ idx (ResourceNode (MemoryRes n)) = "MEM " ++ show n ++ " " ++ show idx
labelDFNodeType _ idx (ResourceNode EmptyRes) = "Empty " ++ show idx
labelDFNodeType _ idx (ResourceNode SpillRes) = "Spill " ++ show idx
 -- Catch memory region cases as we don't want to print the entire memory
labelDFNodeType _ idx (InstructionNode (InitMR _ _)) = "InitMR " ++ show idx
labelDFNodeType opts idx (InstructionNode (Instruction imm elName _)) =
  case showConstantOpts opts of
    ShowConstants ->
      case elName of
        "undoubles" -> elName ++ " " ++ show (map (word642Dbl . fromIntegral) (nub imm)) ++ " " ++ show idx
        "unfloats"  -> elName ++ " " ++ show (map (word322Dbl . fromIntegral) (nub imm)) ++ " " ++ show idx
        _           -> elName ++ " " ++ show imm ++ " " ++ show idx
    NoShowConstants ->
      elName ++ " " ++ show idx
labelDFNodeType _ idx (InstructionNode (Spill el)) = el ++ " " ++ show idx
labelDFNodeType _ idx (InstructionNode (Despill el)) = el ++ " " ++ show idx
labelDFNodeType _ idx (InstructionNode (Move el _)) = el ++ " " ++ show idx
labelDFNodeType _ idx (InstructionNode el) = show el ++ " " ++ show idx
labelDFNodeType _ idx (BranchNode name _ _) = show name ++ " " ++ show idx

-- | Label a @DFNode@ or a @DispatchNode@ according to its type
labelNodeFromIDWrapper :: Show (RegType h) => FGLT.Gr (DFNodeWithDispatch h) DFEdgeWithDispatch -> FGL.Node -> String
labelNodeFromIDWrapper gr nd = case FGL.lab gr nd of
  Just (DispatchNode t) -> "t=" ++ show t
  Just (BaseNode dfnd) -> labelDFNodeType defaultDotCompilationOpts nd dfnd
  Nothing -> ""

-- | Label a @DFEdgeWithDispatch@ 
labelEdgeTypeWrapper :: DFEdgeWithDispatch -> String
labelEdgeTypeWrapper (BaseEdge ed) = labelDFEdgeType ed
labelEdgeTypeWrapper _             = ""

-- | Attach attributes to a @DFNodeWithDispatch h@ 
nodeAttrFuncWrapper :: Show (RegType h) => DotCompilationOpts -> Int -> DFNodeWithDispatch h -> [(String,String)]
nodeAttrFuncWrapper opts idx (BaseNode dfnd) = nodeAttrFunc opts idx dfnd
nodeAttrFuncWrapper _ _ _                    = []

-- | Add an edge attribute to a @DFEdgeWithDispatch@
edgeAttrFuncWrapper :: DFEdgeWithDispatch -> [(String,String)]
edgeAttrFuncWrapper (BaseEdge ed) = edgeAttrFunc ed
edgeAttrFuncWrapper _             = [("color", "pink")]

-- | Default attributes added to a @DFNode h@
nodeAttrFunc :: Show (RegType h) => DotCompilationOpts -> Int -> DFNode h -> [(String,String)]
nodeAttrFunc opts idx p = concatMap ($ p) [addNodeColorAttr, fontSizingAttr, dynamicSizingAttr opts idx, shapeAttr]

-- | Default attributes added to a @DFEdge@
edgeAttrFunc :: DFEdge -> [(String,String)]
edgeAttrFunc p = concatMap ($ p) [fontEdgeAttr]

-- | Default font size added to a @DFEdge@
fontEdgeAttr :: DFEdge -> [(String,String)]
fontEdgeAttr edge = [("fontsize", "8")]

-- | Default font size attribute for @DFNode h@
fontSizingAttr :: Show (RegType h) => DFNode h -> [(String,String)]
fontSizingAttr node = case node of
  ResourceNode _    ->  [("fontsize", "6")]
  InstructionNode _ ->  [("fontsize", "8")]
  BranchNode _ _ _ ->  [("fontsize", "8")]

-- | Default shape attribute for @DFNode h@
shapeAttr :: Show (RegType h) => DFNode h -> [(String,String)]
shapeAttr node = case node of
  ResourceNode _    ->  [("shape", "plaintext")]
  InstructionNode _ ->  [("shape", "box")]
  BranchNode _ _ _ ->  [("shape", "box")]

-- | Filled attribute for @DFNode h@
filledAttr :: DFNode h -> [(String, String)]
filledAttr (ResourceNode _) = []
filledAttr _ = [("style", "filled")]


-- | Calculates the size of a @DFNode h@ label dynamically
dynamicSizingAttr :: Show (RegType h) => DotCompilationOpts -> Int -> DFNode h -> [(String,String)]
dynamicSizingAttr opts idx node = let
  labelWidth  = (fromIntegral $ length (labelDFNodeType opts idx node)) / 15.0 + 0.15
  labelHeight = 0.25
  in [("width", show labelWidth), ("height", show labelHeight), ("fixedsize", "true")]

-- | Default color attribute for @DFNode h@
addNodeColorAttr ::  DFNode h -> [(String,String)]
addNodeColorAttr node = case node of
  ResourceNode _    ->  [("color", "green")]
  InstructionNode _ ->  [("color", "red")]
  BranchNode _ _ _ ->  [("color", "red")]

-- | Take a @CodeGraph@ and its schedule to graphically show 
--   dispatch times on the @CodeGraph@
codeGraphToDot :: Show (RegType h) => DotCompilationOpts -> Schedule -> CodeGraph h -> [(String, Dot ())]
codeGraphToDot opts sc cg  =
  map (second dfgFunc) $ cgDataFlows cg
  where
    dfgFunc = case dfgOpts opts of
      NoTimeNodes -> dataFlowGraphToDot opts
      WithTimeNodes -> dataFlowGraphDispatchToDot opts sc

-- | Take an interference graph and register map, returning the render of the interference graph
interferenceGraphToDot :: (Show (RegType h), Hardware h) => InterferenceGraph h -> RegMap h -> Dot ()
interferenceGraphToDot ig rmap = let
    ndConv nd = case rmap Map.!? nd of
                  Just (regtype, lab) -> (show nd ++ " , " ++ unpack (regPrefix regtype <> lab) )
                  Nothing        -> ""
  in fglToDotGeneric ig ndConv (const "") (const $ const []) (const [])

-- | Make a DataFlowGraph (on a loop body) consisting of scheduled and latency information,
-- which then produces hued nodes modulo the ii to indicate software pipelining
-- for i in 10
--   i1, i2, i3
--   i4
--   ii 3.5
dfgToIIGraph :: (Hardware h, Show (RegType h)) => Schedule -> DataFlowGraph h -> MDMap h -> Double -> IIGraph h
dfgToIIGraph sc dfg md ii =
  let
    removeResourceNode (n, ResourceNode _) = False
    removeResourceNode _ = True
    latencyMapFunc =
      latencyDepth dfg
      (map fst $ filter removeResourceNode $ FGL.labNodes $ dataFlowGraph dfg)
      (instructionLatency dfg md)
    nodes = map (createIINode sc ii latencyMapFunc) (FGL.labNodes $ dataFlowGraph dfg)
    edges = map (createIIEdge dfg sc ii) (FGL.labEdges $ dataFlowGraph dfg)
  in
    FGL.mkGraph nodes edges

-- | Takes a @IIGraph h@ and its associated information and renders to dot
iiGraphToDot :: (Hardware h, Show (RegType h)) => Schedule -> DataFlowGraph h -> MDMap h -> Double -> Dot ()
iiGraphToDot sc dfg md ii =
  let
    iiGraph = dfgToIIGraph sc dfg md ii
  in
    fglToDotGeneric iiGraph (labelIINode iiGraph) labelIIEdge iiAttrNodeFunc iiAttrEdgeFunc
    -- fglToDotGeneric dispatchGraph (labelNodeFromIDWrapper dispatchGraph) labelEdgeTypeWrapper nodeAttrFuncWrapper edgeAttrFuncWrapper

-- | Label nodes in the @IIGraph h@
labelIINode :: Show (RegType h) => IIGraph h -> FGL.Node -> String
labelIINode iiGraph n =
  case FGL.lab iiGraph n of
    Just IINode{..} -> labelDFNodeType defaultDotCompilationOpts n dfNode
      ++ "(" ++ show latencyDepthNum
      ++ "," ++ show dispatchTimeRounded ++ ")"
    Nothing -> error "unreachable case - labelIINode"

-- | Label an @IIEdge@
labelIIEdge :: IIEdge -> String
labelIIEdge IIEdge{..} = labelDFEdgeType dfEdge

iiAttrNodeFunc :: Show (RegType h) => Int -> IINode h -> [(String,String)]
iiAttrNodeFunc idx node@IINode{..} = (:) ("color", show hVal ++ " 1.000 1.000") $
  concatMap ($ dfNode) [fontSizingAttr, dynamicSizingAttr defaultDotCompilationOpts idx, shapeAttr, filledAttr]

iiAttrEdgeFunc :: IIEdge -> [(String, String)]
iiAttrEdgeFunc IIEdge{..} =
  ("color", show hValEdge ++ " 1.000 1.000") : edgeAttrFunc dfEdge

createIINode :: Schedule -> Double -> Map.Map FGL.Node Int -> FGL.LNode (DFNode h) -> FGL.LNode (IINode h)
createIINode sc ii latencyMap (n, dfnode) =
  let dispatchTime = fromMaybe 0 $ sc Map.!? n
      h = (dispatchTime `mod'` ii) / ii
      depth = fromMaybe 0 $ latencyMap Map.!? n
  in (n, IINode dfnode h depth $ round dispatchTime)

createIIEdge :: DataFlowGraph h -> Schedule -> Double -> FGL.LEdge DFEdge -> FGL.LEdge IIEdge
createIIEdge dfg sc ii (from, to, e) = (from, to, IIEdge e h)
  where
    nodeContext = FGL.context (dataFlowGraph dfg) from
    dispatchNode =
      case FGL.labNode' nodeContext of
        (_, ResourceNode _) -> case FGL.lpre' nodeContext of
          [] -> from -- For starting resource nodes
          [(n,_)] -> n -- Use the color of the previous InstructionNode
          ns ->
            error $ "Nodes linked to ResourceNode greater than 1: " ++
              show ns
        _ -> from
    dispatchTime = fromMaybe 0 (sc Map.!? dispatchNode)
    h = (dispatchTime `mod'` ii) / ii

linkDispatchNodes :: [FGL.Node] -> [FGL.LEdge DFEdgeWithDispatch]
linkDispatchNodes (x:y:xs) = (x, y, DispatchEdge) : linkDispatchNodes (y:xs)
linkDispatchNodes [x] = []
linkDispatchNodes [] = []

--TODO: This is just an ad-hoc fix to increment the maxVal past the
-- codegraph's maximum node id; a proper fix would be to extract
-- that maximum number and increment it here so that time nodes do not
-- collide with actual CG nodes.
createDispatchTimeNodes :: Int -> Schedule -> [FGL.LNode (DFNodeWithDispatch h)]
createDispatchTimeNodes maxVal sc = zip [maxVal+10000..] $ map DispatchNode $ nub $ Map.elems sc

createDispatchTimeEdges :: Int -> Schedule -> [FGL.LEdge DFEdgeWithDispatch]
createDispatchTimeEdges maxVal sc = let
  nodeMap = Map.fromList $ zip (nub $ Map.elems sc) [maxVal+10000..]
  dispatchNodes = Map.elems nodeMap
  in linkDispatchNodes dispatchNodes
  ++ (map (\(node, dispatchTime) -> (nodeMap Map.! dispatchTime, node, DispatchEdge)) $ Map.toList sc)
  ++ mapMaybe (\(a, b, c) -> case b of
                                Just x -> Just (a,x,c)
                                Nothing -> Nothing)
            (map (\(node, dispatchTime) -> (node, nodeMap Map.!? (dispatchTime+1), DispatchEdge)) $ Map.toList sc)

filterSchedule :: FGLT.Gr (DFNode h) DFEdge -> Schedule -> Schedule
filterSchedule dfg sc = let
  nds = FGL.nodes dfg
  ndSet = Set.fromList nds
  in Map.filterWithKey (\k _ -> k `Set.member` ndSet) sc

createGraphWithDispatch :: Show (RegType h) => DataFlowGraph h -> Schedule -> FGLT.Gr (DFNodeWithDispatch h) DFEdgeWithDispatch
createGraphWithDispatch gr sc = let
  dfg    = dataFlowGraph gr
  maxVal = maximum $ map snd $ dataFlowOutputs gr
  newSc  = filterSchedule dfg sc
  baseNodes = map (second BaseNode) (FGL.labNodes dfg)
  baseEdges = map (\(a,b,c) -> (a,b,BaseEdge c)) (FGL.labEdges dfg)
  in FGL.mkGraph
       (baseNodes ++ createDispatchTimeNodes maxVal newSc)
       (baseEdges ++ createDispatchTimeEdges maxVal newSc)

dataFlowGraphDispatchToDot :: Show (RegType h) => DotCompilationOpts -> Schedule -> DataFlowGraph h -> Dot ()
dataFlowGraphDispatchToDot opts sc gr = let
  dispatchGraph = createGraphWithDispatch gr sc
  in  fglToDotGeneric dispatchGraph (labelNodeFromIDWrapper dispatchGraph) labelEdgeTypeWrapper (nodeAttrFuncWrapper opts) edgeAttrFuncWrapper

dataFlowGraphToDot :: Show (RegType h) => DotCompilationOpts -> DataFlowGraph h -> Dot ()
dataFlowGraphToDot opts gr = 
  fglToDotGeneric fglGraph indexDFNode labelDFEdgeType (nodeAttrFunc opts) edgeAttrFunc
  where
    fglGraph = dataFlowGraph gr
    indexDFNode nd = case FGL.lab fglGraph nd of
      Just dfNode -> labelDFNodeType opts nd dfNode
      Nothing -> ""

createLinkedDFGs :: (Hardware h, Show (RegType h)) => CodeGraph h -> LinkedDFGs h
createLinkedDFGs cg =
  let dfgs = map (dataFlowGraph . snd) $ cgDataFlows cg
      (dfgAllNodes, dfgAllEdges) =
        foldMap (\dfg -> (FGL.labNodes dfg, FGL.labEdges dfg)) dfgs
      linkDfgAllEdges = map (\(a,b,l) -> (a,b,LinkBaseEdge l)) dfgAllEdges
      thd (_,_,x) = x
      linkDFGsEdges =
        concat $ mapMaybe
        (produceEdge . thd)
        (FGL.labEdges ((controlFlowGraph . cgControlFlow) cg))
 in FGL.mkGraph dfgAllNodes (linkDfgAllEdges ++ linkDFGsEdges)

produceEdge :: Hardware h => CFEdge h -> Maybe [FGL.LEdge LinkDFGEdges]
produceEdge (CFCompose _ nodeCons) = Just $ produceEdge' nodeCons
produceEdge (CFBranchEQ _ nodeCons) = Just $ produceEdge' nodeCons
produceEdge (CFBranchNE _ nodeCons) = Just $ produceEdge' nodeCons
produceEdge (CFJump _ nodeCons) = Just $ produceEdge' nodeCons
produceEdge _ = Nothing

produceEdge' :: [(FGL.Node, FGL.Node)] -> [FGL.LEdge LinkDFGEdges]
produceEdge' = foldr (\(n1, n2) -> (:) (n1, n2, LinkEdge)) []

linkedDFGsToDot :: Show (RegType h) => LinkedDFGs h -> Dot ()
linkedDFGsToDot linkedDFGs =
  fglToDotGeneric linkedDFGs (labelNodeWrapper linkedDFGs) labelEdgeWrapper
    (nodeAttrFunc defaultDotCompilationOpts) edgeAttrWrapper
  where
    labelNodeWrapper gr nd = case FGL.lab gr nd of
      Just dfnd -> labelDFNodeType defaultDotCompilationOpts nd dfnd
      Nothing -> ""

    labelEdgeWrapper LinkEdge          = ""
    labelEdgeWrapper (LinkBaseEdge ed) = labelDFEdgeType ed

    edgeAttrWrapper (LinkBaseEdge ed) = edgeAttrFunc ed
    edgeAttrWrapper LinkEdge          = [("color", "blue")]

dotDFGFormatted :: Show (RegType h) => Schedule -> DataFlowGraph h -> String -> String -> IO ()
dotDFGFormatted sc dfg lab filePath = do
  writeFile (filePath ++ "/" ++ lab ++ ".dot") $ showDot $ dataFlowGraphDispatchToDot defaultDotCompilationOpts sc dfg

dotStringFormatted :: (Hardware h, Show (RegType h)) => DotCompilationOpts -> CodeGraph h -> String -> IO [String]
dotStringFormatted opts cg filePath = let
    sch = defaultSchedule cg
    cgs = codeGraphToDot opts sch cg
    cfg = cfgToDot (controlFlowGraph $ cgControlFlow cg)
  in do
    mapM_ (uncurry writeFile . bimap (\fN -> filePath ++ "/" ++ fN ++ ".dot") showDot) cgs
    writeFile  (filePath ++ "/cfg.dot") $ showDot cfg
    return $ "cfg" : map fst cgs

dotStringFormattedInterference :: (Hardware h, Show (RegType h)) => InterferenceGraph h -> RegMap h -> String -> IO String
dotStringFormattedInterference ig rmap filePath = let
    igDot = interferenceGraphToDot ig rmap
  in do
    writeFile (filePath++"/ig.dot") $ showDotUndirected igDot
    return $ "ig"

dotLinkedDFGFormatted :: (Hardware h, Show (RegType h)) => CodeGraph h -> String -> IO ()
dotLinkedDFGFormatted cg filePath =
  let linkedDFGsDot = linkedDFGsToDot $ createLinkedDFGs cg
      fileNameExt ext = filePath ++ "/" ++ "linkedDFGs" ++ ext
  in do
    writeFile (fileNameExt ".dot") $ showDot linkedDFGsDot
    callProcess "dot" ["-Tpng", fileNameExt ".dot", "-o", fileNameExt ".png"]

-- This is a convenience function that calls system processes
-- to invoke dot file compilation. This could also be in a Makefile,
-- if we get separate-module compilation working with `stack ghc`
-- (Currently it doesn't because of nonlocal GHC extensions and
-- the lack of a main function)
dotCompilationWithFilePath :: (Hardware h, Show (RegType h)) => DotCompilationOpts -> CodeGraph h -> String -> IO ()
dotCompilationWithFilePath opts cg filePath = do
  dotFiles <- dotStringFormatted opts cg filePath
  when (graphCompileOpts opts == CompileWithDot) $
    forM_ dotFiles $ \file ->
                       callProcess "dot" ["-Tpng", filePath ++ "/" ++ file ++ ".dot"
                                         , "-o", filePath ++ "/" ++ file ++ ".png"]

dotCompilation :: (Hardware h, Show (RegType h)) => CodeGraph h -> IO ()
dotCompilation cg = dotCompilationWithFilePath defaultDotCompilationOpts cg "dot"

dotCompilationWithOpts :: (Hardware h, Show (RegType h)) => DotCompilationOpts -> CodeGraph h -> IO ()
dotCompilationWithOpts opts cg = dotCompilationWithFilePath opts cg "dot"

dotCompilationInterference :: (Hardware h, Show (RegType h)) => InterferenceGraph h -> RegMap h -> String -> IO ()
dotCompilationInterference ig rmap filePath = do
  file <- dotStringFormattedInterference ig rmap filePath
  callProcess "dot" ["-Tpng", filePath ++ "/" ++ file ++ ".dot"
                    , "-o", filePath ++ "/" ++ file ++ ".png"]

dotCompilationLinkedDFGs :: (Hardware h, Show (RegType h)) => CodeGraph h -> IO ()
dotCompilationLinkedDFGs cg = dotLinkedDFGFormatted cg "dot"

-- dotCompileTestIG filePath =
--   case testRegAllocateCG of
--     Left _ -> putStrLn "Expected RegMap but got 'Left [Node]'"
--     Right regmap -> dotCompilationInterference (snd testInterferenceGraph) regmap filePath

dotCompileIIGraph :: (Hardware h, Show (RegType h)) => FilePath -> DataFlowGraph h -> Schedule -> MDMap h -> Double -> IO ()
dotCompileIIGraph filePath dfg sc md ii =
  let dotGraph = iiGraphToDot sc dfg md ii
  in writeFile filePath $ showDot dotGraph

-----------------------------------------------------------------------------------------------------
-- Generating scheduled graphs
-----------------------------------------------------------------------------------------------------

scheduledGraphToDot :: Show (RegType h) => CodeGraph h -> ScheduledGraph h -> Schedule -> Dot ()
scheduledGraphToDot cg sg sched = fglToDotGeneric sg (labelScheduledNode cg sg sched) (const "") (const $ const []) (const [])

scheduledGraphWithRegMapToDot :: Show (RegType h) => CodeGraph h -> RegMap h -> ScheduledGraph h -> Dot ()
scheduledGraphWithRegMapToDot cg regMap sg = fglToDotGeneric sg (labelScheduledNodeWithRegMap cg regMap sg) (const "") setNodeAttr (const [])
  where
    setNodeAttr _ _ = [("shape","rect")]

labelScheduledNode :: Show (RegType h) => CodeGraph h -> ScheduledGraph h -> Schedule -> FGL.Node -> String
labelScheduledNode cg sg sched nd = case matchNodeInCG cg nd of
  CGControlFlowNode cfnd -> show nd ++ ": " ++ show cfnd ++ " " ++ scheduleLabel
  CGDataFlowNode dfnd -> show nd ++ ": " ++ show dfnd ++ " " ++ scheduleLabel
                         ++ ": " ++ show dispatchTime
  CGNotFound -> ""
  where
    dispatchTime = case Map.lookup nd sched of
                     Just n -> n
                     Nothing -> -1.0
    coalesceMap = registerCoalesceMap cg
    toCoalesceMap (n,rs) = case IM.lookup n coalesceMap of
                        Just n' -> (n',rs)
                        Nothing -> (n,rs)
    scheduleLabel = case FGL.lab sg nd of
      Nothing -> ""
      Just (ScheduledNode ndIdx uses dfns unmappedNd gConst regPres) -> show (map toCoalesceMap uses)
                                                 ++ " "
                                                 ++ show (map toCoalesceMap dfns)
                                                 ++ " "
                                                 ++ show unmappedNd
                                                 ++ " "
                                                 ++ show gConst
                                                 ++ " "
                                                 ++ show regPres

labelScheduledNodeWithRegMap :: Show (RegType h) => CodeGraph h -> RegMap h -> ScheduledGraph h -> FGL.Node -> String
labelScheduledNodeWithRegMap cg regMap sg nd = case matchNodeInCG cg nd of
  CGControlFlowNode cfnd -> show nd ++ ": " ++ show cfnd ++ " " ++ scheduleLabel
  CGDataFlowNode dfnd -> show nd ++ ": " ++ fst dfnd ++ "," ++ showDataFlowNode (snd dfnd) ++ " " ++ scheduleLabel
  CGNotFound -> ""
  where
    scheduleLabel = case FGL.lab sg nd of
      Nothing -> ""
      Just (ScheduledNode ndIdx uses dfns unmappedNd gConst regPres) ->
           printList (map (showTuple . bimap nodeToReg show) uses)
        ++ " "
        ++ printList (map (showTuple . bimap nodeToReg show) dfns) -- ("13", "GPR")
        ++ " "
        ++ show unmappedNd
        ++ " "
        ++ show gConst
        ++ " "
        ++ show regPres


    nodeToReg nd =
      case Map.lookup nd regMap of
        Nothing -> error $ "Dot.hs labelScheduledNodeWithRegMap did not find node " ++ show nd
        Just (_, lab) -> unpack lab
    showDataFlowNode (ResourceNode res) = show res
    showDataFlowNode (InstructionNode instr) = showEL instr
    showDataFlowNode (BranchNode instr imm _) = instr ++ " " ++ show imm

    showEL Meet = "Meet"
    showEL Join = "Join"
    showEL (InitMR label wrds) = "InitMR " ++ label -- ++ " " ++ show wrds
    showEL (Instruction _ name _) = name
    showEL (Spill name) = name
    showEL (Despill name) = name
    showEL (Move name _) = name

    showTuple (a,b) = "(" ++ a ++ "," ++ b ++ ")"
    printList [] = "[]"
    printList xs = "[" ++ foldl1 (\s a -> s ++ "," ++ a) xs ++ "]"

dotStringScheduleFormatted :: (Hardware h, Show (RegType h)) => CodeGraph h -> ScheduledGraph h -> Schedule -> String -> IO [String]
dotStringScheduleFormatted cg sg sched filePath = let
    sgs = scheduledGraphToDot cg sg sched
  in do
    writeFile (filePath++"/sfg.dot") $ showDot sgs
    return ["sfg"]

dotStringScheduleWithRegMapFormatted :: (Hardware h, Show (RegType h)) => CodeGraph h -> RegMap h -> ScheduledGraph h -> String -> IO [String]
dotStringScheduleWithRegMapFormatted cg regMap sg filePath = let
    sgs = scheduledGraphWithRegMapToDot cg regMap sg
  in do
    writeFile (filePath++"/sfg.dot") $ showDot sgs
    return ["sfg"]

dotScheduleCompilation :: (Hardware h, Show (RegType h)) => CodeGraph h -> ScheduledGraph h -> Schedule -> String -> IO ()
dotScheduleCompilation cg sg sched filePath = do
  dotFiles <- dotStringScheduleFormatted cg sg sched filePath
  forM_ dotFiles $ \file ->
    callProcess "dot" ["-Tpng", filePath ++ "/" ++ file ++ ".dot", "-o"
                      ,filePath ++ "/" ++ file ++ ".png"]

dotScheduleCompilationWithRegMap :: (Hardware h, Show (RegType h)) => CodeGraph h -> RegMap h -> ScheduledGraph h -> String -> IO ()
dotScheduleCompilationWithRegMap cg regMap sg filePath = do
  dotFiles <- dotStringScheduleWithRegMapFormatted cg regMap sg filePath
  forM_ dotFiles $ \file ->
    callProcess "dot" ["-Tpng", filePath ++ "/" ++ file ++ ".dot", "-o"
                      ,filePath ++ "/" ++ file ++ ".png"]
