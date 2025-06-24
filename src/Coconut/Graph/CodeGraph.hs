-- |
-- Module      :  CodeGraph
-- Copyright   :  (c) OCA 2021
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module provides a DataFlow and ControlFlow graph's built on top of @HyperGraph@
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}


module Coconut.Graph.CodeGraph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.List as List

import Data.Graph.Inductive hiding (Graph)
import Control.Monad (when)
import Control.Monad.State.Strict (StateT,MonadState,MonadIO,get,gets,put,modify',evalStateT)
import Control.Monad.Identity (runIdentity,Identity)


import Data.Graph.Inductive.Graph (LNode,LEdge,empty,mkGraph)
import qualified Data.Graph.Inductive.Graph as FGL

import Coconut.BaseTypes
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Graph.ByteStringAST

import Coconut.Utils.Hash

import Coconut.Graph.StrictIdentity

import Debug.Trace (traceM, traceShowId)
import Data.Word (Word64)

import Control.DeepSeq (force,NFData(..))

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS

-- import Data.Trie (Trie)
-- import qualified Data.Trie as Trie
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
-- import Coconut.Graph.RadixTree (RadixTree)
-- import qualified Coconut.Graph.RadixTree as RTree

-- | The building unit of a @DataFlowGraph@, essentially a wrapper of @CGBState h Node@
--   Data flow DSL functions (i.e. such as the methods in @CoreISA@) are essentially
--   @Graph@ combinators
data Graph h a = Graph { genGraph :: CGBState h Node
                       , graphAST :: !ShortByteString
                       , graphCache :: Maybe ShortByteString }

instance NFData (Graph h a) where
  rnf (Graph _ gAST gCACHE) = rnf gAST `seq` rnf gCACHE

-- TODO
--      and a function shared :: String -> Graph h a -> Graph h a
--      that sets a graph to have a isShared :: Just lbl
--      ex,
--      add (x,y) =
--        let
--           x' = shared "x'" $ va 2 x y
--           ...
--      Then add to CGBuilder a parameter shareMap :: Map String Node
--      Whenever a graph with isShared = Just lbl appears in any op_*
--      first attempt to lookup in shareMap, and only evaluate state
--      then insert if not

instance Hardware h => Cacheable (Graph h) where
  cache subst (Graph sT gAST _) =
    let
      !subst' = BS.toShort subst
    in Graph sT subst' (Just gAST)

  fCache fName args (Graph sT gAST _) =
    let
      !argLbls = map (\(Graph _ ast _) -> ast) args
      !subst' = BS.toShort fName <> (mconcat $ map (\arg -> lBracketW8 <> arg <> rBracketW8) argLbls)
    in Graph sT subst' (Just gAST)

  -- clearCache (Graph sT _) = let
  --   sT' = do n <- sT
  --            modify' (\cgBuilder -> cgBuilder { cgbCachedNodes = RTree.empty })
  --            return n
  --   in Graph sT' ""

-- | The building unit of a @ControlFlowGraph@, essentially a wrapper of @CGBState h BlockParams@
--   Functions built using @Graph@ are transformed into @Block@ via @basicBlock@
--   Control flow DSL functions are essentially @Block@ combinators
newtype Block h a = Block { genBlock :: CGBState h BlockParams }

-- | The parameters for a @Block@ that are "carried" in the underlying @CGBState@
data BlockParams = BlockParams {
  -- | The label of the first @DataFlowGraph@
    bpLabel :: String
  -- | The current @ControlFlowGraph@ input node
  , bpCFInput :: Node
  -- | The current @DataFlowGraph@ input nodes
  , bpDFInputs :: [(String,Node)]
  -- | The current @ControlFlowGraph@ output node
  , bpCFOutput :: Node
  -- | The current @DataFlowGraph@ output nodes
  , bpDFOutputs :: [(String,Node)]
  }
  deriving (Show,Eq)

-- | Same as @Block@ (i.e. a building unit of a @ControlFlowGraph@) however contains
--   a reference to a special node in the @DataFlowGraph@ that serves as the "conditional parameter",
--  i.e., a @CR@ resource
newtype BranchBlock h a = BranchBlock { genBranchBlock :: CGBState h BranchBlockParams }

-- | The parameters for a @BranchBlock@ that are "carried" in the underlying @CGBState@
data BranchBlockParams = BranchBlockParams {
  -- | The label of the first @DataFlowGraph@
    bbpLabel :: String
  -- | The branch node in the @DataFlowGraph@
  , bbpBranchNode :: (String,Node)
  -- | The current @ControlFlowGraph@ input node
  , bbpCFInput :: Node
  -- | The current @DataFlowGraph@ input nodes
  , bbpDFInputs :: [(String,Node)]
  -- | The current @ControlFlowGraph@ output node
  , bbpCFOutput :: Node
  -- | The current @DataFlowGraph@ output nodes
  , bbpDFOutputs :: [(String,Node)]
  }
  deriving (Show,Eq)

-- | Simple class used to overload functions using @BlockParams@,
-- @BranchBlockParams@
class BParams b where
  bParamLabel :: b -> String
  bParamCFInput :: b -> Node
  bParamCFOutput :: b -> Node

instance BParams BlockParams where
  bParamLabel = bpLabel
  bParamCFInput = bpCFInput
  bParamCFOutput = bpCFOutput

instance BParams BranchBlockParams where
  bParamLabel = bbpLabel
  bParamCFInput = bbpCFInput
  bParamCFOutput = bbpCFOutput

-- | Internal state thats wrapped in the @CGBStateT@ state monad transfomer that's
--   used to build a @CodeGraph@
data CGBuilder h
  = CGBuilder { -- | uniqueID used to create unique identifiers
                cgbUniqueID :: Int
                -- | the "in-construction" @DataFlow@ graph
              , cgbDFBuilder :: DFBuilder h
                -- | the "in-construction" @ControlFlowGraph@
              , cgbCFBuilder :: CFBuilder h
                -- | the most recently constructed @DataFlowGraph@
              , cgbDataFlows :: [(String,DataFlowGraph h)]
                -- | the most recently constructed @ControlFlow@ graph
              , cgbControlFlow :: ControlFlowGraph h
                -- | tables of memory regions with their initial values
              , cgbMRTables :: [(String,Node,[Interp VR])]
                -- | table reserved for constant loads
              , cgbConstTable :: [(Word64,Word64)]
                -- | map of tagged nodes
              , cgbTags :: Map String [Node]
                -- | map of cached nodes with labels for debugging
              , cgbDebugLabels :: Map Node String
              }

-- | Internal state for the "in-construction" @DataFlow@ Graph
data DFBuilder h = DFBuilder { dfbNodes :: ![LNode (DFNode h)]  -- Hash maps to node and output nodes
                             , dfbHashes :: CGNodeMap h
                             , dfbEdges :: ![LEdge DFEdge]
                             , dfbInputs :: [(String,Node)]
                             , dfbOutputs :: [(String,Node)]
                             , dfbOverwrites :: [(Node,(Node,Node))]
                             , dfbSubstMap :: !(HashMap ShortByteString ShortByteString)
                             , dfbCachedNodes :: !(HashMap ShortByteString Node)
                             , dfbIncrement :: Int
                             , dfbStage :: Int
                             }

-- | Internal state for the "in-construction" @DataFlow@ Graph
data CFBuilder h = CFBuilder { cfbNodes :: [LNode CFNode]
                             , cfbEdges :: [LEdge (CFEdge h)]
                             , cfbInput :: Maybe Node
                             , cfbOutput :: Maybe Node
                             }

-- | Monad state transformer for constructing @CGBuilder@
newtype CGBStateT h m a = CGBStateT { unCGBState :: StateT (CGBuilder h) m a }
  deriving (Functor, Applicative, Monad, MonadState (CGBuilder h), MonadIO, MonadFail)

-- | Default monad state transformer (i.e., using @Identity@ monad) for @CGBuilder@
type CGBState h a = CGBStateT h StrictIdentity a

-- | Initial state used for @CGBuilder@
initCGBState :: CGBuilder h
initCGBState = CGBuilder { cgbUniqueID = 0
                         , cgbDataFlows = []
                         , cgbDFBuilder = initDFBuilder
                         , cgbControlFlow = initControlFlowGraph
                         , cgbCFBuilder = initCFBuilder
                         , cgbMRTables = []
                         , cgbConstTable = []
                         , cgbTags = Map.empty
                         , cgbDebugLabels = Map.empty
                         }

-- | Initial state used for @DFBuilder@
initDFBuilder = DFBuilder { dfbNodes = []
                          , dfbHashes = IM.empty
                          , dfbEdges = []
                          , dfbInputs = []
                          , dfbOutputs = []
                          , dfbOverwrites = []
                          , dfbIncrement = 0
                          , dfbSubstMap = HashMap.empty
                          , dfbCachedNodes = HashMap.empty
                          , dfbStage = 0
                          }

-- | Initial state used for @DFBuilder@
initCFBuilder = CFBuilder { cfbNodes = []
                          , cfbEdges = []
                          , cfbInput = Nothing
                          , cfbOutput = Nothing
                          }

-- | Initial state for @DataFlowGraph@
initDataFlowGraph = DataFlowGraph { dataFlowGraph =  empty
                                  , dataFlowInputs = []
                                  , dataFlowOutputs = []
                                  , dataFlowOverwrites  = []
                                  , dataFlowIncrement  = 0
                                  , dataFlowStage = 0}

-- | Initial state for @DataFlowGraph@
initControlFlowGraph = ControlFlowGraph { controlFlowGraph =  empty
                                        , controlFlowInput = Nothing
                                        , controlFlowOutput = Nothing
                                        }

-- | Deconstructs a @CodeGraph@ back to a @CGBuilder@. Leaves all
-- @DataFlowGraph@'s constructed and the current "in-constuction"
-- @DataFlowGraph@ empty, however deconstructs the @ControlFlowGraph@ back to
-- "in-construction" format
codeGraphToCGBuilder :: CodeGraph h -> CGBuilder h
codeGraphToCGBuilder cg =
  let
    cfGraph = cgControlFlow cg
    dfGraphs = cgDataFlows cg
    maxID = cgMaxID cg
    mrTables = cgMRTables cg
    cTable = cgConstTable cg
    tags = cgTags cg
    inp = cgInput cg
    out = cgOutput cg
    debugLabs = cgDebugLabels cg
    -- maxID :: Node
    -- maxID = maximum $ snd (nodeRange $ controlFlowGraph cfGraph)
    --                 : map (\(_,dfGraph) -> snd $ nodeRange $ dataFlowGraph dfGraph ) dfGraphs

    cfBuilder = CFBuilder { cfbNodes = labNodes $ controlFlowGraph cfGraph
                          , cfbEdges = labEdges $ controlFlowGraph cfGraph
                          , cfbInput = controlFlowInput cfGraph
                          , cfbOutput = controlFlowOutput cfGraph
                          }

  in CGBuilder { cgbUniqueID = maxID + 1
               , cgbDataFlows = dfGraphs
               , cgbDFBuilder = initDFBuilder
               , cgbControlFlow = initControlFlowGraph
               , cgbCFBuilder = cfBuilder
               , cgbMRTables = mrTables
               , cgbConstTable = cTable
               , cgbTags = tags
               , cgbDebugLabels = debugLabs
               }

-- | Wrapper type used to return the match of a CodeGraph node lookup
data CodeGraphNode h = CGControlFlowNode CFNode
                     | CGDataFlowNode (String,DFNode h)
                     | CGNotFound

-- | Looks up a node in the entire CodeGraph
-- Checks the ControlFlowGraph first and returns the CFNode corresponding to the node if found
-- (without checking DataFlowGraphs). Throws an error if the same node is found in multiple DataFlowGraphs
matchNodeInCG :: CodeGraph h -> Node -> CodeGraphNode h
matchNodeInCG cg node =
  let
    cfGraph = cgControlFlow cg
    dfGraphs = cgDataFlows cg
    inp = cgInput cg
    out = cgOutput cg
    mCFNode = case match node (controlFlowGraph cfGraph) of
               (Just (adjPrevs,_,cfNode,adjSuccs),cfGraph') -> Just (CGControlFlowNode cfNode)
               (Nothing,_) -> Nothing
    mDFNode = case filter (\mCGNode -> isJust mCGNode) (map matchDFNode dfGraphs) of
                [Just cgDataFlowNode] -> Just cgDataFlowNode
                [] -> Nothing
                xs -> error $ "matchNodeInCG found multiple dfGraphs containing node: " ++ show node
    matchDFNode (lbl,dfGraph) = case match node (dataFlowGraph dfGraph) of
               (Just (adjPrevs,_,dfNode,adjSuccs),cfGraph') -> Just (CGDataFlowNode (lbl,dfNode))
               (Nothing,_) -> Nothing
  in case mCFNode of
       Just cfNode -> cfNode
       Nothing -> case mDFNode of
                    Just dfNode -> dfNode
                    Nothing -> CGNotFound

-- | If a node belongs to a @DataFlowGraph@ with a @dataFlowIncrement@, return
-- the original node before incrementation. If the node doesn't belong do a
-- @DataFlowGraph@ or has a @dataFlowIncrement@ of zero, returns Nothing
unIncrementNode :: CodeGraph h -> Node -> Maybe Node
unIncrementNode cg node =
  let
    cgNode = matchNodeInCG cg node

    (dfLabel,dfNode) = case cgNode of
                         CGDataFlowNode x -> x
                         _ -> error "unIncrementNode unreachable point"
    dfGraph = fromJust $ List.lookup dfLabel $ cgDataFlows cg
    dfIncr = dataFlowIncrement dfGraph
  in case cgNode of
       CGDataFlowNode _ -> if dfIncr == 0
                              then Nothing
                              else Just $ node - dfIncr
--                                   `debug` ("unIncrementNode dfIncr: " ++ show dfIncr)
       _ -> Nothing

-- | Evaluate @CGBState@ and build/return the resulting @CodeGraph@
createCG :: CGBState h a -> CodeGraph h
createCG = runStrictIdentity . createCGM

-- | Evaluate @CGBStateT@ under a given monad and build/return the resulting @CodeGraph@
createCGM :: Monad m => CGBStateT h m a -> m (CodeGraph h)
createCGM cgmState =
  let
    runCGMState cgbState = evalStateT (unCGBState cgbState) initCGBState
  in runCGMState $! cgmState >> buildCG

-- | Evaluate @CGBState@ and build/return the resulting @CodeGraph@ using a
-- given @CGBuilder@ as initial state
createCGFromCGB :: CGBuilder h -> CGBState h a -> CodeGraph h
createCGFromCGB cgBuilder = runStrictIdentity . (createCGMFromCGB cgBuilder)

-- | Evaluate @CGBStateT@ under a given monad and build/return the resulting
-- @CodeGraph@ using a given @CGBuilder@ as initial state
createCGMFromCGB :: Monad m => CGBuilder h -> CGBStateT h m a -> m (CodeGraph h)
createCGMFromCGB cgBuilder cgmState =
  let
    runCGMState cgbState = evalStateT (unCGBState cgbState) cgBuilder
  in runCGMState $ cgmState >> buildCG

-- | Build a @CodeGraph@ inside the @CGBStateT@ monad
buildCG :: forall h m . Monad m => CGBStateT h m (CodeGraph h)
buildCG = do
  cgbState <- get
  let cfBuilder = cgbCFBuilder cgbState
  let cfInp = fromJust $ cfbInput cfBuilder
  let cfOut = fromJust $ cfbOutput cfBuilder
  let controlFlow = mkGraph (cfbNodes cfBuilder) (cfbEdges cfBuilder)
  let dfInOutTags = concatMap (\(_,dfGraph) -> dataFlowInputs dfGraph ++ dataFlowOutputs dfGraph)
                    $ cgbDataFlows cgbState
  let tagMap = foldr insertNodeTag (cgbTags cgbState) dfInOutTags
  let maxID = cgbUniqueID cgbState
  return $!
    CodeGraph { cgControlFlow = ControlFlowGraph controlFlow (Just cfInp) (Just cfOut)
              , cgDataFlows = cgbDataFlows cgbState
              , cgInput = cfInp
              , cgOutput = cfOut
              , cgMRTables = cgbMRTables cgbState
              , cgConstTable = cgbConstTable cgbState
              , cgTags = tagMap
              , cgMaxID = maxID
              , cgDebugLabels = cgbDebugLabels cgbState
              }

-- | Insert a @DFNode@ into the current "in-construction" @DataFlowGraph@
cgbAddDFNode :: Hardware h => [Node] -> [Node] -> DFNode h -> CGBState h Node
cgbAddDFNode !inps !outs !dfNode = {-# SCC "cgbAddDFNode" #-} do
  cgbState <- get
  let uniqueID = cgbUniqueID cgbState
  let dfBuilder = cgbDFBuilder cgbState
  let dfNodes = dfbNodes dfBuilder
  let dfHashes = dfbHashes dfBuilder
  let hashKey = hash inps dfNode
  let newDfbHashes = case dfNode of
        InstructionNode _ -> insertWithHash hashKey ((dfNode,inps),uniqueID,outs) dfHashes
        _ -> dfHashes
  let dfBuilder' = dfBuilder { dfbNodes = (uniqueID,dfNode):dfNodes
                             , dfbHashes = newDfbHashes }
  modify' (\cgbState -> cgbState { cgbDFBuilder = dfBuilder'
                                 , cgbUniqueID = uniqueID+1})
  return $! uniqueID

-- | Insert a @DFNode@ with inputs into the current "in-construction" @DataFlowGraph@
--   Adds the node and edges from inputs to that node, then returns the Node
cgbAddDFNodeWithInputs :: Hardware h => [Node] -> DFNode h -> CGBState h (Node,[Node])
cgbAddDFNodeWithInputs !inputs !dfNode = {-# SCC "cgbAddDFNodeWithInputs" #-} do
  -- Create DF Node
  node <- cgbAddDFNode inputs [] dfNode -- FIXME: Do we need outputs here
  -- Add edges from inputs to DFNode
  sequence $! map (\(n,idx)-> cgbAddDFEdge (n,node) (DFEdge idx)) (zip inputs [0..])
  -- return newly added node output
  return $! (node,[node])

-- FIXME very innefficent, find a better way to do common-subexpression elimination
-- |  Insert a @DFNode@ with inputs into the current "in-construction" @DataFlowGraph@
--  IFF it doesn't already exist (i.e. a node with the same name and same inputs already exists)
cgbAddDFNodeWithInputsIfNew :: Hardware h => [Node] -> DFNode h -> CGBState h (Node,[Node])
cgbAddDFNodeWithInputsIfNew !inputs !dfNode =
  let
    instructionsWithSameName :: [LNode (DFNode h)] -> [Node]
    instructionsWithSameName nodes = map fst
      $ filter (\(n, dfNode') ->
                  case (dfNode',dfNode) of
                   (InstructionNode el',InstructionNode el) ->
                     case (el',el) of
                       (Instruction imm0 name0 _,Instruction imm1 name1 _) -> name0 == name1
                                                                              && imm0 == imm1
                       (Move name0 _,Move name1 _) -> name0 == name1
                       (Spill name0,Spill name1) -> name0 == name1
                       (Despill name0,Despill name1) -> name0 == name1
                       (InitMR lbl0 imm0,InitMR lbl1 imm1) -> lbl0 == lbl1
                       _ -> False
                   _ -> False) nodes

    instructionInputs :: [LEdge DFEdge] -> Node -> (Node,[Node])
    instructionInputs lEdges node  = (node,map (\(n0,_,_) -> n0)
                                          $ filter (\(n0,n1,_) -> n1 == node) lEdges)

    instructionOutputs :: [LEdge DFEdge] -> Node -> [Node]
    instructionOutputs lEdges node  = map snd
      $ List.sortOn fst
      $ map (\(_,n1,DFEdge idx) -> (idx,n1))
      $ filter (\(n0,n1,_) -> n0 == node) lEdges

    findSameInstr :: [(Node,[Node])] -> Maybe Node
    findSameInstr ((n,inputs'):ns) = if sameInputs inputs inputs'
                                        then Just n
                                        else findSameInstr ns
    findSameInstr [] = Nothing

    sameInputs :: [Node] -> [Node] -> Bool
    sameInputs inps0 inps1 = (length inps0 == length inps1) && (and [ n `elem` inps1 | n <- inps0 ] )

  in {-# SCC "cgbAddDFNodeWithInputsIfNew" #-} do
    dfBuilder <- gets cgbDFBuilder
    let hashes = dfbHashes dfBuilder
    case hashExists inputs dfNode hashes of
      Nothing -> cgbAddDFNodeWithInputs inputs dfNode
      Just (n, outputs) -> return $! (n, outputs)
    {-
    dfBuilder <- gets cgbDFBuilder
    let sameInstructions = instructionsWithSameName $ extractLNodes $ dfbNodes dfBuilder
    let sameInstructionsInputs = map (instructionInputs $ dfbEdges dfBuilder) sameInstructions
    case findSameInstr sameInstructionsInputs of
      Just n -> return $ (n,instructionOutputs (dfbEdges dfBuilder) n)
      Nothing -> cgbAddDFNodeWithInputs inputs dfNode
    -}

-- | Insert a @DFEdge@ into the current "in-construction" @DataFlow@ graph
cgbAddDFEdge :: (Node,Node) -> DFEdge -> CGBState h ()
cgbAddDFEdge (!n0,!n1) !dfEdge = {-# SCC "cgbAddDFEdge" #-} do
  cgbState <- get
  let dfBuilder = cgbDFBuilder cgbState
  let dfEdges = dfbEdges dfBuilder
  let dfBuilder' = dfBuilder { dfbEdges = (n0,n1,dfEdge):dfEdges }
  modify' (\cgbState -> cgbState { cgbDFBuilder =  dfBuilder' })

-- | Insert a @Resource@ (i.e. particular @DFNode@) into the "in-construction" @DataFlow@ graph
--   Returns the newly created @Node@
cgbAddResource :: Hardware h => ResType h -> CGBState h Node
cgbAddResource !resType = {-# SCC "cgbAddResource" #-} cgbAddDFNode [] [] $! ResourceNode resType

-- | Insert a @Instruction@ (i.e. a particular @DFNode@) into the "in-construction" @DataFlow@ graph.
--   Also inserts edges from inputs @Node@'s to the new @DFNode@. Each edge is is labelled to
--   specify argument order (left to right, 0 to infinity)
--   Returns the newly created Node
cgbAddInstruction :: Hardware h => [Node] -> EL h -> [ResType h] -> CGBState h (Node,[Node])
cgbAddInstruction !inputs !instr !resTypes = {-# SCC "cgbAddInstruction" #-} do
  -- Create output nodes
  outputs <- sequence $! map cgbAddResource resTypes
  -- Create instruction node, which stores its output nodes
  node <- cgbAddDFNode inputs outputs $ InstructionNode instr
  -- Add edges from inputs to instruction node
  sequence $! map (\(n,idx)-> cgbAddDFEdge (n,node) (DFEdge idx)) (zip inputs [0..])
  -- Add edges from instruction node to outputs
  sequence $! map (\(n,idx)-> cgbAddDFEdge (node,n) (DFEdge idx)) (zip outputs [0..])
  -- return output nodes
  return $! (node,outputs)

-- FIXME very innefficent, find a better way to do common-subexpression elimination
-- | Insert a @Instruction@ (i.e. a particular @DFNode@) into the "in-construction" @DataFlow@ graph
--   IFF it doesn't already exist (i.e. an instruciton with the same name and same inputs already exists)
--   Also inserts edges from inputs @Node@'s to the new @DFNode@. Each edge is is labelled to
--   specify argument order (left to right, 0 to infinity)
--   Returns the newly created Node
cgbAddInstructionIfNew :: Hardware h => [Node] -> EL h -> [ResType h] -> CGBState h (Node,[Node])
cgbAddInstructionIfNew !inputs !instr !resTypes = {-# SCC "cgbAddInstructionIfNew" #-} do
  {- traceM $ case instr of
    Instruction imms name _ -> show (imms,name)
    _ -> "Other" -}
  dfBuilder <- gets cgbDFBuilder
  let !hashes = dfbHashes dfBuilder
  case hashExists inputs (InstructionNode instr) hashes of
    Nothing -> cgbAddInstruction inputs instr resTypes
    Just (!n, !outputs) -> return $! (n, outputs)
          --  let m' = hashNode (checkCollisionMap (dfbNodes dfBuilder)) (inputs,instr)
          --  in modify (const m' . dfbNodes . cgbDFBuilder) >>
        {-
        let sameInstructions = instructionsWithSameName $ dfbNodes dfBuilder
        let sameInstructionsInputs = map (instructionInputs $ dfbEdges dfBuilder) sameInstructions
        case findSameInstr sameInstructionsInputs of
          Just n -> return $ (n,instructionOutputs (dfbEdges dfBuilder) n)
          Nothing -> cgbAddInstruction inputs instr resTypes
        -}

-- | Check if an instruction that performs an overwrite has already been inserted
-- This is tricky because when an overwriting instruction is inserted that shares a register
-- with another overwriting instruction, a register copy instruction needs to be added. Thus
-- to see if its a duplicate you have to check if the inserted instruction has the same inputs
-- or the same inputs except on of them is a product of a move
cgbCheckIfOverwriteIsDuplicate !inputs !instr !resTypes =
  let
    instructionsWithSameName :: [LNode (DFNode h)] -> [Node]
    instructionsWithSameName nodes = map fst
      $ filter (\(n, dfNode) ->
                  case dfNode of
                   InstructionNode el ->
                     case (el,instr) of
                       (Instruction imm0 name0 _,Instruction imm1 name1 _) -> name0 == name1
                                                                              && imm0 == imm1
                       (Move name0 _,Move name1 _) -> name0 == name1
                       (Spill name0,Spill name1) -> name0 == name1
                       (Despill name0,Despill name1) -> name0 == name1
                       (InitMR lbl0 imm0,InitMR lbl1 imm1) -> lbl0 == lbl1
                       _ -> False
                   _ -> False) nodes

    instructionInputs :: [LEdge DFEdge] -> [LNode (DFNode h)] -> Node -> (Node,[Node])
    instructionInputs lEdges lNodes node  =
      (node, map fst $ List.sortBy (\(_,dfEdge0) (_,dfEdge1) ->
                                      compare (dfEdgeIndex dfEdge0) (dfEdgeIndex dfEdge1))
            $ map (\(nRes,_,dfEdge) -> (nRes,dfEdge))
            -- if resource node is produced by a vlr, replace with the original node (i.e source of vlr)
            $ map (\(nRes,nInstr,lbl) -> (moveSource lEdges lNodes nRes,nInstr,lbl))
            -- filter for inputs produced by instruction
            $ filter (\(nRes,nInstr,_) -> nInstr == node) lEdges)

    moveSource lEdges lNodes nRes =
      -- filter for instructions that produce the currrent resource node we're inspecting
       case filter (\(nInstr,nRes',_) -> nRes == nRes') lEdges of
            -- if resource node is vlr, it should only be produced by one instruction
            ((nInstr,_,_):_) -> if isMoveOW lNodes nInstr
                                   then lookupMoveInput lEdges nInstr
                                   else nRes
            -- safe to assume it isn't produced by vlr, just return original resource node
            _ -> nRes

    lookupMoveInput lEdges nInstr = case concatMap (\(nInp,nInstr',_) ->
                                                     if nInstr == nInstr'
                                                     then [nInp]
                                                     else []) lEdges of
                                     (nInp:_) -> nInp
                                     _ -> error $ "failed to lookup move input: " ++ (show nInstr)

    isMoveOW lNodes nInstr = case List.lookup nInstr lNodes of
                            Just (InstructionNode (Move _ True)) -> True
                            _ -> False

    instructionOutputs :: [LEdge DFEdge] -> Node -> [Node]
    instructionOutputs lEdges node  = map snd
      $ List.sortOn fst
      $ map (\(_,n1,DFEdge idx) -> (idx,n1))
      $ filter (\(n0,n1,_) -> n0 == node) lEdges

    findSameInstr :: [LNode (DFNode h)] -> [LEdge DFEdge] -> [(Node,[Node])] -> Maybe Node
    findSameInstr dfNodes dfEdges ((n,inputs'):ns) = if sameInputs dfNodes dfEdges inputs inputs'
                                        then Just n
                                        else findSameInstr dfNodes dfEdges ns
    findSameInstr dfNodes dfEdges [] = Nothing

    -- instruction has the same inputs if the Node's are the same, EXCEPT in the case of a constant
    -- load (in which case we compare the immediates)
    -- this allows us to rematerialize overwritten constants instead of using a vlr
    sameInputs :: [LNode (DFNode h)] -> [LEdge DFEdge] -> [Node] -> [Node] -> Bool
    sameInputs dfNodes dfEdges (i0:inps0) (i1:inps1) = -- inps0 == inps1 -- (length inps0 == length inps1) && (and [ n `elem` inps1 | n <- inps0 ] )
      case (isConstant i0 dfNodes dfEdges,isConstant i1 dfNodes dfEdges) of
        (Just (Instruction imm0 name0 _),Just (Instruction imm1 name1 _))
          -> imm0 == imm1 && sameInputs dfNodes dfEdges inps0 inps1
        _ -> i0 == i1 && sameInputs dfNodes dfEdges inps0 inps1
    sameInputs dfNodes dfEdges [] [] = True
    sameInputs dfNodes dfEdges _ _ = False

    isConstant resNode dfNodes dfEdges =
      -- find the instruction nodes that produce the resource node
      case filter (\(_,resNode',_) -> resNode==resNode') dfEdges of
        -- there should only be one instruction node
        ((instrNode,_,_):_) ->
          case List.lookup instrNode dfNodes of
            -- if the node is a constant load, it's name should begin with 'u' 'n'
            Just (InstructionNode el@(Instruction _ ('u':'n':_) _)) -> Just el
            _ -> Nothing
        _ -> Nothing
  in {-# SCC "cgbCheckIfOverwriteIsDuplicate" #-} do
        dfBuilder <- gets cgbDFBuilder
        let sameInstructions = instructionsWithSameName $ dfbNodes dfBuilder
        let sameInstructionsInputs = map (instructionInputs (dfbEdges dfBuilder) (dfbNodes dfBuilder)) sameInstructions
        case findSameInstr (dfbNodes dfBuilder) (dfbEdges dfBuilder) sameInstructionsInputs of
          Just n -> return $! Just (instructionOutputs (dfbEdges dfBuilder) n)
          Nothing -> return $! Nothing

-- | Checks if a given node is a ResourceNode generated by a constant load, and
-- if so returns the @EL@ corresponding to that constant load
cgbInputIsConstantLoad :: Hardware h => Node -> CGBState h (Maybe (EL h))
cgbInputIsConstantLoad !inp = let
     inpProducers dfEdges = concatMap (\(n0,n1,_) -> if n1 == inp then [n0] else []) dfEdges
  in {-# SCC "cgbInputIsConstantLoad" #-}
    do  dfBuilder <- gets cgbDFBuilder
        let dfEdges = dfbEdges dfBuilder
        let dfNodes = dfbNodes dfBuilder
        case inpProducers dfEdges of
          (!n:_) -> case List.lookup n dfNodes of
                     Just (InstructionNode !el) ->
                       case el of
                         !(Instruction _ ('u':'n':_) _) -> return $! Just el
                         _ -> return $! Nothing
                     _ -> return $! Nothing
          _ -> return $! Nothing

-- | Add a new node to the current "in-construction" @DataFlow@ graph using @cgbAddResource@
--   and sets it as an input by adding it to the head of @cgbDFInputs@
cgbAddInput :: Hardware h => (String,ResType h ) -> CGBState h Node
cgbAddInput (!inputTag, !resType) =
  let
    cgbAddNewInput (inputID, resType) = do
      node <- cgbAddResource resType
      modify' (addDFBuilderInput (inputTag, node))
      -- TODO need to alter inputTag to make it unique as input?
      cgbCacheNode node (s2b inputTag)
      return $! node
  in {-# SCC "cgbAddInput" #-}
    do  dfBuilder <- gets cgbDFBuilder
        case List.lookup inputTag (dfbInputs dfBuilder) of
          Just !node -> return $! node
          Nothing -> do
            newNode <- cgbAddNewInput (inputTag,resType)
            traceM ("New node: " <> show newNode <> " For: " <> inputTag)
            return $! newNode


-- | Pure function for adding a node to @CGBuilder->cgbDFBuilder->dfbInputs@
--   Meant for use via the @modify@ function under the @CGBStateT@ monad
addDFBuilderInput :: (String, Node ) -> CGBuilder h -> CGBuilder h
addDFBuilderInput (!inputTag,!node) !cgBuilder =
  let
    dfBuilder = cgbDFBuilder cgBuilder
    dfInputs = dfbInputs dfBuilder
  in cgBuilder { cgbDFBuilder = dfBuilder { dfbInputs = (inputTag,node):dfInputs } }

-- | Print out the current @dfbInputs@ using @traceM@
---  NOTE for debuggin purposes only
printDFInputs :: CGBState h ()
printDFInputs = do
  dfBuilder <- gets cgbDFBuilder
  traceM $ "dfbInputs: " ++ show (dfbInputs dfBuilder)

-- | Sets the current @cgbDFOutputs@ list (that keeps track of the output nodes of
--   the current "in-construction" @DataFlowGraph@)
cgbSetOutputs :: [(String,Node)] -> [ShortByteString] -> CGBState h ()
cgbSetOutputs !nodes !asts = do
  cgbState <- get
  let dfBuilder' = (cgbDFBuilder cgbState) { dfbOutputs = nodes }
  modify' (\cgBuilder -> cgBuilder { cgbDFBuilder = dfBuilder' })

-- | Construct a @DataFlow@ graph using the current @cgbDFBuilder@, store it in @cgbDataFlows@,
--   and then reset @cgbDFBuilder@ to @initDFBuilder@
cgbCreateDFGraph :: Hardware h => String -> CGBState h (DataFlowGraph h)
cgbCreateDFGraph !label = do
  cgbState <- get
  let dfBuilder = cgbDFBuilder cgbState
  let dfNodes = dfbNodes dfBuilder
  let dfEdges = dfbEdges dfBuilder
  let dfGraphs = cgbDataFlows cgbState
  let dfGraph' = DataFlowGraph { dataFlowGraph = mkGraph dfNodes dfEdges
                               , dataFlowInputs = dfbInputs dfBuilder
                               , dataFlowOutputs = dfbOutputs dfBuilder
                               , dataFlowOverwrites  = List.nub $dfbOverwrites dfBuilder
                               , dataFlowIncrement = dfbIncrement dfBuilder
                               , dataFlowStage = dfbStage dfBuilder
                               }
  let dataFlows' = (label,dfGraph') : dfGraphs
  modify' (\cgbState -> cgbState { cgbDataFlows = dataFlows'
                                 , cgbDFBuilder = initDFBuilder
                                 })
  return $! dfGraph'

-- | Set the input/output (repectively) of the "in-constrution" @ControlFlowGraph@, i.e.,
--   @cfbInput@ and @cfbOutput@
cgbSetCFInsOuts :: (Node,Node) -> CGBState h ()
cgbSetCFInsOuts (!n0,!n1) = do
  cgbState <- get
  let cfBuilder' = (cgbCFBuilder cgbState) { cfbInput = Just n0
                                          , cfbOutput = Just n1 }
  modify' (\cgbState -> cgbState { cgbCFBuilder = cfBuilder' } )

-- | Insert a @CFNode@ into the current "in-construction" @cgbCFNodes@ list
--   Returns the current @Node@ id in the CGBState monad
cgbAddCFNode :: CFNode -> CGBState h Node
cgbAddCFNode !cfNode = {-# SCC "cgbAddCFNode" #-} do
  cgbState <- get
  let uniqueID = cgbUniqueID cgbState
  let cfBuilder = cgbCFBuilder cgbState
  let cfNodes = cfbNodes cfBuilder
  let cfBuilder' = cfBuilder { cfbNodes = (uniqueID,cfNode):cfNodes }
  modify' (\cgbState -> cgbState { cgbCFBuilder =  cfBuilder'
                                 , cgbUniqueID = uniqueID+1})
  return $! uniqueID

-- | Insert a @DFEdge@ into the current "in-construction" @cgbCFEdges@ list
--   FIXME remove me in favor of ad-hoc functions like cgAddDataFlow, cgAddBranch, etc?
cgbAddCFEdge :: (Node,Node) -> CFEdge h -> CGBState h ()
cgbAddCFEdge (!n0,!n1) !cfEdge = {-# SCC "cgbAddCFEdge" #-} do
  cgbState <- get
  let cfBuilder = cgbCFBuilder cgbState
  let cfEdges = cfbEdges cfBuilder
  let cfBuilder' = cfBuilder { cfbEdges = (n0,n1,cfEdge):cfEdges }
  modify' (\cgbState -> cgbState { cgbCFBuilder = cfBuilder' })

-- | Deletes the first @Node@ and substitutes all occurences of it with the
-- second @Node@ in the current "in-construction" @ControlFlowGraph@. NOTE
-- assumes both nodes already exist and will build a bad graph if the second
-- node does not
cgbSubCFNode :: Node -> Node -> CGBState h ()
cgbSubCFNode !n0 !n1 = {-# SCC "cgbSubCFNode" #-} do
  cgbState <- get
  let cfBuilder = cgbCFBuilder cgbState
  -- remove n0 from current list of LNode's
  let cfNodes' = filter (\(n,_) -> n /= n0) $ cfbNodes cfBuilder
  -- substitute all occurances of n0 with n1 in edges
  let cfEdges' = map subEdge $ cfbEdges cfBuilder
  let cfBuilder' = cfBuilder { cfbNodes = cfNodes'
                             , cfbEdges = cfEdges' }
  modify' (\cgbState -> cgbState { cgbCFBuilder = cfBuilder' })
  where
    subEdge (nA,nB,e)
      | n0 == nA && n0 == nB = (n1,n1,e)
      | n0 == nA = (n1,nB,e)
      | n0 == nB = (nA,n1,e)
      | otherwise = (nA,nB,e)

-- | Insert an already constructed @DataFlowGraph@ into the current
-- @cgbDataFlows@ list. Remaps nodes to ensure uniqueness, then returns the new
-- @DataFlowGraph@ with these remapped nodes
cgbInsertDataFlow :: Hardware h => (String,DataFlowGraph h) -> CGBState h (DataFlowGraph h)
cgbInsertDataFlow (!label,!dfGraph) =
  let
    instructionInputs :: [LEdge DFEdge] -> Node -> [Node]
    instructionInputs lEdges node  = map fst
                                          $ List.sortBy (\(_,dfEdge0) (_,dfEdge1) ->
                                                           compare (dfEdgeIndex dfEdge0) (dfEdgeIndex dfEdge1))
                                          $ map (\(n0,_,dfEdge) -> (n0,dfEdge))
                                          $ filter (\(n0,n1,_) -> n1 == node) lEdges

    instructionOutputs :: [LEdge DFEdge] -> Node -> [Node]
    instructionOutputs lEdges node  = map snd
      $ List.sortOn fst
      $ map (\(_,n1,DFEdge idx) -> (idx,n1))
      $ filter (\(n0,n1,_) -> n0 == node) lEdges

    rehashFunc :: Hardware h => Int -> CGNodeMap h -> CGNodeMap h
    rehashFunc uID =
        -- foldr (\((dfNode,inps),n,outs) mp ->
      --              insertWithHash (hash inps dfNode)
      --                        ((dfNode,inps),n,outs) mp) IM.empty -- Rehash
      -- . map (\(inps,n,outs) -> (inps,n+uID,outs)) -- Increment the IDs
      -- -- Extract keys and sort in descending order on the ID
      -- . reverse . List.sortOn (\(_,n,_) -> n) . concat . IM.elems

      IM.map (map (\(inps,n,outs) -> (inps,n+uID,outs)))
  in {-# SCC "cgbInsertDataFlow" #-} do
    cgbState <- get
    let dfHashes = dfbHashes . cgbDFBuilder $ cgbState
        uID = (cgbUniqueID cgbState) + 1
        dfNodes = labNodes $ dataFlowGraph dfGraph
        dfNodes' = map (\(n,lN) -> (n+uID,lN)) dfNodes
        dfEdges = labEdges $ dataFlowGraph dfGraph
        dfEdges' = map (\(n0,n1,lE) -> (n0+uID,n1+uID,lE)) dfEdges
        dfIns = map (\(tag,inp) -> (tag,inp+uID)) $ dataFlowInputs dfGraph
        dfOuts = map (\(tag,out) -> (tag,out+uID)) $ dataFlowOutputs dfGraph
        dfOverwrites = map (\(n,(n0,n1)) -> (n+uID,(n0+uID,n1+uID))) $ dataFlowOverwrites dfGraph
        uID' = maximum $ (map (\(n,_) -> n) dfNodes') ++ (concatMap (\(n0,n1,_) -> [n0,n1]) dfEdges')
        dfBuilder' = DFBuilder { dfbNodes = dfNodes' -- IM.fromList $ zip dfbNodeKeys $ zip dfNodes' dfNodeOutputs
                               , dfbHashes = rehashFunc uID dfHashes -- rehash
                               , dfbEdges = dfEdges'
                               , dfbInputs = dfIns
                               , dfbOutputs = dfOuts
                               , dfbOverwrites = dfOverwrites
                               , dfbIncrement = uID
                               , dfbSubstMap = HashMap.empty
                               , dfbCachedNodes = HashMap.empty
                               , dfbStage = dataFlowStage dfGraph
                               }
    modify' (\cgbState ->  cgbState { cgbUniqueID = uID'+1
                                    , cgbDFBuilder = dfBuilder' } )
    cgbCreateDFGraph label

-- | Insert an already constructed @DataFlowGraph@ into the current
-- @cgbDataFlows@ list AND add it as an in the current "in-construction"
-- @ControlFlowGraph@ (creating two @CFNode@'s in the process)
cgbInsertDataFlowEdge :: Hardware h => (String,DataFlowGraph h) -> CGBState h (Node,Node,DataFlowGraph h)
cgbInsertDataFlowEdge (!label,!dfGraph) = {-# SCC "cgbInsertDataFlowEdge" #-} do
  dfGraph' <- cgbInsertDataFlow (label,dfGraph)
  c0 <- cgbAddCFNode $ CFNode $ dataFlowInputs dfGraph'
  c1 <- cgbAddCFNode $ CFNode $ dataFlowOutputs dfGraph'
  cgbAddCFEdge (c0,c1) (CFDataFlow label)
  return $! (c0,c1,dfGraph')

-- | Looks up a @CFNode@ corresponding to @Node@ in the current
-- "in-construction" @ControlFlowGraph@ NOTE uses fromJust to perform the list
-- lookup
cgbLookupCFNode :: Node -> CGBState h (CFNode)
cgbLookupCFNode !node = {-# SCC "cgbLookupCFNode" #-} do
  cgbState <- get
  let cfBuilder = cgbCFBuilder cgbState
  let cfNode = fromJust $ List.lookup node (cfbNodes cfBuilder)
  return $! cfNode

-- | Overwrite an existing @CFNode@ in the current "in-construction"
-- @ControlFlowGraph@
cgbUpdateCFNode :: Node -> CFNode -> CGBState h ()
cgbUpdateCFNode node cfNode = {-# SCC "cgbUpdateCFNode" #-} do
  cgbState <- get
  let cfBuilder = cgbCFBuilder cgbState
  let cfNodes = cfbNodes cfBuilder
  let cfBuilder' = cfBuilder { cfbNodes = updateCFNode cfNodes }
  modify' $(\cgbState ->  cgbState { cgbCFBuilder = cfBuilder' } )
  where
    updateCFNode [] = []
    updateCFNode ((!n,!cf):ns)
      | n == node = (n,cfNode):ns
      | otherwise = (n,cf) : updateCFNode ns

-- | Add a node to an existing DataFlowGraph (NOTE be careful adding
-- instructions that have a register overwrite, this doesn't modify
-- @dataFlowOverwrites@)
cgbInsertDFNode :: String -> DFNode h -> CGBState h Node
cgbInsertDFNode !label !dfNode = {-# SCC "cgbInsertDFNode" #-} do
  cgbState <- get
  let dfGraph = case List.lookup label $ cgbDataFlows cgbState of
                  Just df -> df
                  Nothing -> error $ "Missing DataFlowGraph: " ++ label
  let uID = cgbUniqueID cgbState
  let dfGraph' = DataFlowGraph (insNode (uID,dfNode) $ dataFlowGraph dfGraph)
                               (dataFlowInputs dfGraph)
                               (dataFlowOutputs dfGraph)
                               (dataFlowOverwrites dfGraph)
                               (dataFlowIncrement dfGraph)
                               (dataFlowStage dfGraph)
  let dataFlows' = (label,dfGraph') : (filter (\(lbl,_) -> lbl /= label) (cgbDataFlows cgbState))
  modify' (\cgbState -> cgbState { cgbDataFlows = dataFlows'
                                 , cgbUniqueID = uID+1})
  return $! uID

-- | Add an overwrite (n,(n0,n1)) to the current "in-construction" list of
-- overwrites (i.e. dfbOverwrites) where n is the instruction @Node@, n0 is the
-- input @Node@ being overwritten and n1 is the output @Node@
cgbAddOverwrite :: (Node,(Node,Node)) -> CGBState h ()
cgbAddOverwrite !ow = {-# SCC "cgbAddOverwrite" #-} do
  cgbState <- get
  let dfBuilder = cgbDFBuilder cgbState
  let dfOverwrites = dfbOverwrites dfBuilder
  let dfBuilder' = dfBuilder { dfbOverwrites = ow:dfOverwrites }
  modify' (\cgbState -> cgbState { cgbDFBuilder = dfBuilder' })

-- | Add an entry to the current @cgbMRTables@
cgbAddMRTable :: String -> Node -> [Interp VR] -> CGBState h ()
cgbAddMRTable !label !n !vs = {-# SCC "cgbAddMRTable" #-} do
  cgbState <- get
  let mrTables = cgbMRTables cgbState
  if label `elem` (map (\(label,_,_)->label) mrTables) -- only add the table if the label doesn't already exist
    then return $! ()
    else modify' (\cgbState -> cgbState { cgbMRTables = (label,n,vs) : mrTables })

-- | Add an entry to the current @cgbConstTable@
cgbAddConstant :: (Word64,Word64) -> CGBState h ()
cgbAddConstant !c = {-# SCC "cgbAddConstant" #-} do
  cgbState <- get
  let cTable = cgbConstTable cgbState
  if c `elem` cTable -- only add the table if the label doesn't already exist
    then return ()
    else modify' (\cgbState -> cgbState { cgbConstTable = c : cTable })


-- | Add a node with a given tag (i.e. String) to the cgTag map
cgbAddTag :: String -> Node -> CGBState h Node
cgbAddTag !tag !node = {-# SCC "cgbAddTag" #-} do
  cgbState <- get
  let tagMap = cgbTags cgbState
  let tagMap' = insertNodeTag (tag,node) tagMap
  modify' (\cgbState -> cgbState { cgbTags = tagMap' })
  return $! node

-- | Insert a tag into a tag map
insertNodeTag :: (String,Node) -> Map String [Node] -> Map String [Node]
insertNodeTag (!tag,!node) !tagMap =
  let
    insertIfNew new old = if node `elem` old then old else new++old
  in Map.insertWith insertIfNew tag [node] tagMap

-- | Remove @CFEdge@'s (any edges from (n0,n1)) from the current
-- "in-construction" @ControlFlowGraph@
cgbRemoveCFEdge :: (Node,Node) -> CGBState h ()
cgbRemoveCFEdge (!n0,!n1) = {-# SCC "cgbRemoveCFEdge" #-} do
  cgbState <- get
  let cfBuilder = cgbCFBuilder cgbState
  let cfEdges = cfbEdges cfBuilder
  let cfEdges' = [ (n0',n1',e) | (n0',n1',e) <- cfEdges, not (n0==n0' && n1==n1') ]
  modify' (\cgbState -> cgbState { cgbCFBuilder = cfBuilder { cfbEdges = cfEdges' }})

-- | Removes a @DataFlowGraph@ with the given label from the list cgbDatFlows
cgbRemoveDFGraph :: String -> CGBState h ()
cgbRemoveDFGraph !label = {-# SCC "cgbRemoveDFGraph" #-} do
  cgbState <- get
  let dfGraphs = cgbDataFlows cgbState
  let dfGraphs' = filter (\(l,_) -> l /= label) dfGraphs
  modify' (\cgbState -> cgbState { cgbDataFlows = dfGraphs' })

-- | Sequence a list of CGBState while checking for cached nodes
cgbSequence :: [(CGBState h Node,ShortByteString,Maybe ShortByteString)]
            -> CGBState h [Node]
cgbSequence !ns = sequence $! map cgbSeqNode ns

-- | Process a single node, checking for substitutions and caching
cgbSeqNode :: (CGBState h Node, ShortByteString,Maybe ShortByteString)
           -> CGBState h Node
cgbSeqNode (sT,ast,mSubst) = do
  cacheMap <- gets (dfbCachedNodes . cgbDFBuilder)
  dbgMap <- gets cgbDebugLabels
  ast' <- cgbSubstitute ast mSubst
  case HashMap.lookup ast' cacheMap of
    Nothing -> do n <- sT
                  cgbCacheNode n ast'
                  when (ast /= ast') $ cgbCacheNode n ast
                  case mSubst of
                    Just subst -> do
                      traceM $ "Placing debug node: " <> show n
                      modify' $ \cgbState -> cgbState
                          { cgbDebugLabels = force $ Map.insert n (show subst) dbgMap }
                      traceM $ show $ debugPrintCgbLabels dbgMap
                    Nothing -> return ()
                  return $! n
    Just !n -> return $! n


-- | Modify CGBuilder.DFBuilder.dfbSubstMap
modifySubstMap substMap cgBuilder =
  let
    dfBuilder = cgbDFBuilder cgBuilder
  in cgBuilder { cgbDFBuilder = dfBuilder { dfbSubstMap = substMap } }

-- | Perform a substitution on the ByteString AST if necessary
cgbSubstitute ast mSubst = do
  case mSubst of
    Nothing -> return ast -- no subst to perform
    Just s ->
      do substMap <- gets (dfbSubstMap . cgbDFBuilder)
         -- check if subst already exists
         case HashMap.lookup ast substMap of
           -- if subst already exists, it must map to the same ast
           Just s' -> if s /= s'
                      then error $ "attempted to re-use subst label: "
                           ++ (BC.unpack $ BS.fromShort ast)
                      else return s
           -- if subst doesn't exist, add it to cgbSubstMap
           Nothing -> do modify' (modifySubstMap (HashMap.insert ast s substMap))
                         return s

modifyCacheMap cacheMap cgBuilder =
  let
    dfBuilder = cgbDFBuilder cgBuilder
  in cgBuilder { cgbDFBuilder = dfBuilder { dfbCachedNodes = cacheMap } }

-- | Add a list of outNodes to cgbCachedNodes
cgbCacheOutputs :: [Node] -> ShortByteString -> CGBState h ()
cgbCacheOutputs !outNodes !ast = do
  cachedNodes <- gets (dfbCachedNodes . cgbDFBuilder)
  let !cachedNodes' = force
                    $ List.foldl' (\trie (n,outNode) ->
                                     HashMap.insert (buildASTParams ast n) outNode trie)
                     cachedNodes
                     (zip [0..] outNodes)
  modify' (modifyCacheMap cachedNodes')

-- | Add a node to cgbCachedNodes
cgbCacheNode :: Node -> ShortByteString -> CGBState h ()
cgbCacheNode !node !ast = do
  cachedNodes <- gets (dfbCachedNodes . cgbDFBuilder)
  let !cachedNodes' = force $ HashMap.insert ast node cachedNodes
  modify' (modifyCacheMap cachedNodes')

debugPrintCgbLabels :: Map Node String -> Map Node String
debugPrintCgbLabels =
  Map.map (\s -> take 20 s ++ "TRUNCATED") 
