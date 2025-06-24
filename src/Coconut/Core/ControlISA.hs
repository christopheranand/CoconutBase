-- |
-- Module      :  Coconut.Core.ControlISA
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- FIXME add module description for ControlISA

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Coconut.Core.ControlISA where

import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Control.Monad.State.Strict (get,put,gets)

import Data.Graph.Inductive.Graph

import Coconut.BaseTypes
import Coconut.Graph.DataFlow
import Coconut.Graph.ControlFlow
import Coconut.Graph.CodeGraph
import Coconut.Utils.CGWrappers
import Coconut.Core.CoreISA
import Coconut.Core.CoreHardware
import Coconut.Core.CodeGraph

-- * By Block (safe)

-- | Type-safe composition of @Block@'s
-- Adds a new @ControlFlowGraph@ edge @CFCompose@ with implicitly tied dataflow input/outputs in-order
-- Returns a new @Block@ with blockA inputs and blockB outputs in @BlockParam@, and sets new
-- input/output nodes for the "in-construction" @ControlFlowGraph@
compose :: Block h (a,b) -> Block h (b,c) -> Block h (a,c)
compose blockA blockB = Block $ do
  bpA <- genBlock blockA
  bpB <- genBlock blockB
  let labelA = bpLabel bpA
  let labelB = bpLabel bpB
  let (cfInA,cfOutA) = (bpCFInput bpA,bpCFOutput bpA)
  let (cfInB,cfOutB) = (bpCFInput bpB,bpCFOutput bpB)
  let (dfInA,dfOutA) = (bpDFInputs bpA,bpDFOutputs bpA)
  let (dfInB,dfOutB) = (bpDFInputs bpB,bpDFOutputs bpB)
  cgbAddCFEdge (cfOutA, cfInB) (CFCompose (labelA,labelB) $ zip (map snd dfOutA) (map snd dfOutB))
  cgbSetCFInsOuts (cfInA,cfOutB)
  return $ BlockParams labelA cfInA dfInA cfOutB dfOutB


-- | Type-safe branching of @Block@'s (i.e. if-then-else)
-- Adds two new edges to @ControlFlowGraph@ (i.e. @CFBranchEQ@ to blockA and @CFBranchNE@ to blockB),
-- a new @CFJoinNode@ node, and finally two edges from blockA,blockB to the join node
-- NOTE assumes dataflow nodes from blockB are consolidated and thus BlockParams only contains outputs
-- nodes from blockA in bpDFOutputs
branch :: BranchBlock h (a,b) -> Block h (b,c) -> Block h (b,c) -> Block h (c,d) -> Block h (a,d)
branch cond blockA blockB post = Block $ do
  bpCond <- genBranchBlock cond
  bpA <- genBranchBlock $ blockAddJump blockA
  bpB <- genBranchBlock $ blockAddJump blockB
  bpP <- genBlock post
  let (labelA,labelB,labelC,labelP) = (bbpLabel bpA,bbpLabel bpA,bbpLabel bpCond,bpLabel bpP)
  let (cfInC,cfOutC) = (bbpCFInput bpCond,bbpCFOutput bpCond)
  let (dfInC,dfOutC) = (bbpDFInputs bpCond,bbpDFOutputs bpCond)
  let (cfInA,cfOutA) = (bbpCFInput bpA,bbpCFOutput bpA)
  let (cfInB,cfOutB) = (bbpCFInput bpB,bbpCFOutput bpB)
  let (dfInA,dfOutA) = (bbpDFInputs bpA,bbpDFOutputs bpA)
  let (dfInB,dfOutB) = (bbpDFInputs bpB,bbpDFOutputs bpB)
  let (cfInP,cfOutP) = (bpCFInput bpP,bpCFOutput bpP)
  let (dfInP,dfOutP) = (bpDFInputs bpP,bpDFOutputs bpP)
  -- add edge from condition block to blockA
  cgbAddCFEdge (cfOutC,cfInA) (CFBranchEQ labelA $ zip (map snd dfOutC) (map snd dfInA))
  -- add edge from condition block to blockB
  cgbAddCFEdge (cfOutC,cfInB) (CFBranchNE labelB $ zip (map snd dfOutC) (map snd dfInB))
  -- create join node and add edges from blockA,blockB outputs
  cfJoin <- cgbAddCFNode $ CFJoinNode $ zip (map snd dfOutA) (map snd dfOutB)
  cgbAddCFEdge (cfOutA,cfJoin) (CFJump (labelA,labelP) $ zip (map snd dfOutA) (map snd dfInP))
  cgbAddCFEdge (cfOutB,cfJoin) (CFJump (labelB,labelP) $ zip (map snd dfOutB) (map snd dfInP))
  -- replace starting cfnode in post block with join node
  cgbSubCFNode cfInP cfJoin
  -- set CG in and out nodes
  cgbSetCFInsOuts (cfInC, cfOutP)
  -- new Block is from start of cond BranchBlock to post block at the end
  return $ BlockParams labelC cfInC dfInC cfOutP dfOutP

-- | Type-safe looping
-- Adds a @CFLoopEQ@ edge from the end to the start of a BranchBlock and a @CFLoopNE@ edge
-- from the end of the @BranchBlock@ to a post @Block@
doWhile :: BranchBlock h (a,a) -> Block h (a,c) -> Block h (a,c)
doWhile loop post = Block $ do
  bpLoop <- genBranchBlock loop
  bpPost <- genBlock post
  let label = bbpLabel bpLoop
  let labelP = bpLabel bpPost
  let (cfInL,cfOutL) = (bbpCFInput bpLoop,bbpCFOutput bpLoop)
  let (cfInP,cfOutP) = (bpCFInput bpPost,bpCFOutput bpPost)
  let (dfInL,dfOutL) = (bbpDFInputs bpLoop,bbpDFOutputs bpLoop)
  let (dfInP,dfOutP) = (bpDFInputs bpPost,bpDFOutputs bpPost)
  -- Add Loop edge from end loop BranchBlock to beginning of loop BranchBlock
  cgbAddCFEdge (cfOutL,cfInL) (CFBranchEQ label $ zip (map snd dfInL) (map snd dfOutL))
  -- Add Compose edge from end of loop BranchBlock to beginnning of post Block
  cgbAddCFEdge (cfOutL,cfInP) (CFBranchNE labelP $ zip (map snd dfOutL) (map snd dfInP))
  -- new Block is from start of loop BranchBlock to end of post Block
  return $ BlockParams label cfInL dfInL cfOutP dfOutP

-- * By BlockParams (unsafe)

-- | Unsafe branching of @Block@'s (i.e. if-then-else)
-- Adds two new edges to @ControlFlowGraph@ (i.e. @CFBranchEQ@ to blockA and
-- @CFBranchNE@ to blockB).
-- Forward projects nodes (see @projectCFNodeTags@) that are outputs of the condition block
branchBP :: (BParams b0,BParams b1)
         => (String -> String -> Bool)
         -> BranchBlockParams
         -> b0
         -> b1
         -> CGBState h ()
branchBP tagCompare cBlock aBlock bBlock =
    do cgbState <- get
       let (labelC,cNode) = (bbpLabel cBlock,bbpCFOutput cBlock)
           (labelA,aNode) = (bParamLabel aBlock,bParamCFInput aBlock)
           (labelB,bNode) = (bParamLabel bBlock,bParamCFInput bBlock)
           cfBuilder = cgbCFBuilder cgbState
           cfNodes = cfbNodes cfBuilder
           cfEdges = cfbEdges cfBuilder
           cTags = case List.lookup cNode cfNodes of
                      Just (CFBranchNode _ tags)-> tags
                      Just _ -> error $ "branchCFN given cNode that isn't CFBranchNode: " ++ show cNode
                      Nothing -> error $ "branchCFN given missing node for cNode: " ++ show cNode
           aTags = case List.lookup aNode cfNodes of
                      Just (CFNode tags) -> tags
                      Just (CFBranchNode _ tags) -> tags
                      Just _ -> error $ "branchCFN given bad node type for aNode: " ++ show aNode
                      Nothing -> error $ "branchCFN given missing node for aNode: " ++ show aNode
           bTags = case List.lookup bNode cfNodes of
                      Just (CFNode tags) -> tags
                      Just (CFBranchNode _ tags) -> tags
                      Just _ -> error $ "branchCFN given bad node type for bNode: " ++ show bNode
                      Nothing -> error $ "branchCFN given missing node for bNode: " ++ show bNode
       cgbAddCFEdge ((cNode,aNode)) $ CFBranchEQ labelA
         $ tieNodesByTagModulo (labelC,cTags) (labelA,aTags)
       cgbAddCFEdge ((cNode,bNode)) $ CFBranchNE labelB
         $ tieNodesByTagModulo (labelC,cTags) (labelB,bTags)
       projectCFNodeTags aNode cTags
       projectCFNodeTags bNode cTags

-- | Unsafe composition of @Block@'s
-- Adds a new edge to the @ControlFlowGraph@ (i.e. @CFCompose@ betwen blocks A and B)
-- Forward projects nodes (see @projectCFNodeTags@) that are outputs of the block A
composeBP :: (BParams b0,BParams b1)
         => (String -> String -> Bool)
         -> b0
         -> b1
         -> CGBState h ()
composeBP tagCompare aBlock bBlock =
  do cgbState <- get
     let (labelA,aNode) = (bParamLabel aBlock,bParamCFOutput aBlock)
         (labelB,bNode) = (bParamLabel bBlock,bParamCFInput bBlock)
         cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         aTags = case List.lookup aNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "composeCFN given bad node type for aNode: " ++ show (labelA,aNode)
                   Nothing -> error $ "composeCFN given missing node for aNode: " ++ show (labelA,aNode)
         bTags = case List.lookup bNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "composeCFN given bad node type for bNode: " ++ show (labelB,bNode)
                   Nothing -> error $ "composeCFN given missing node for bNode: " ++ show (labelB,bNode)
         missingTags = filter (\bTag -> not $ bTag `elem` map fst aTags) $ map fst bTags
         bTags' = case missingTags of
                    [] -> bTags ++ filter (\(aTag,_) -> not $ aTag `elem` (map fst bTags)) aTags
                    _ -> error $ "Missing tags "++show missingTags++" from nodes "++show (aNode,bNode)
     cgbAddCFEdge (aNode,bNode) $ CFCompose (labelA,labelB)
                                $ (tieNodesBy tagCompare) (labelA,aTags) (labelB,bTags)
     projectCFNodeTags bNode aTags

-- | Unsafe composition of @Block@'s
-- Adds a new edge to the @ControlFlowGraph@ (i.e. @CFCompose@ betwen blocks A and B)
-- Backword projects nodes (see @projectCFNodeTags@) that are inputs of the block B
composeBP_B :: (BParams b0,BParams b1)
         => (String -> String -> Bool)
         -> b0
         -> b1
         -> CGBState h ()
composeBP_B tagCompare aBlock bBlock =
  do cgbState <- get
     let (labelA,aNode) = (bParamLabel aBlock,bParamCFOutput aBlock)
         (labelB,bNode) = (bParamLabel bBlock,bParamCFInput bBlock)
         cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         aTags = case List.lookup aNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "composeCFN given bad node type for aNode: " ++ show (labelA,aNode)
                   Nothing -> error $ "composeCFN given missing node for aNode: " ++ show (labelA,aNode)
         bTags = case List.lookup bNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "composeCFN given bad node type for bNode: " ++ show (labelB,bNode)
                   Nothing -> error $ "composeCFN given missing node for bNode: " ++ show (labelB,bNode)
     cgbAddCFEdge (aNode,bNode) $ CFCompose (labelA,labelB)
                                $ (tieNodesBy tagCompare) (labelA,aTags) (labelB,bTags)
     backProjectCFNodeTags aNode bTags

-- | Unsafe unconditional branch of @Block@'s
-- Adds a new edge to the @ControlFlowGraph@ (i.e. @CFJump@ betwen blocks A and B)
-- Forward projects nodes (see @projectCFNodeTags@) that are outputs of the block A
jumpBP :: (BParams b0,BParams b1)
         => (String -> String -> Bool)
         -> b0
         -> b1
         -> CGBState h ()
jumpBP tagCompare aBlock bBlock =
  do cgbState <- get
     let (labelA,aNode) = (bParamLabel aBlock,bParamCFOutput aBlock)
         (labelB,bNode) = (bParamLabel bBlock,bParamCFInput bBlock)
         cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         aTags = case List.lookup aNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "jumpCFN given bad node type for aNode: " ++ show (labelA,aNode)
                   Nothing -> error $ "jumpCFN given missing node for aNode: " ++ show (labelA,aNode)
         bTags = case List.lookup bNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "jumpCFN given bad node type for bNode: " ++ show (labelB,bNode)
                   Nothing -> error $ "jumpCFN given missing node for bNode: " ++ show (labelB,bNode)
     cgbAddCFEdge (aNode,bNode) $ CFJump (labelA,labelB)
       $ (tieNodesBy tagCompare) (labelA,aTags) (labelB,bTags)


-- | Project tags onto a given @CFNode@. So given a node matching @CFNode tags@,
-- add any pair (tag',n) in tags' to tags when tag' is not alredy in tags. Also,
-- do the same thing for any adjacent node (n,n1) connected by a @CFDataFlow@ edge
projectCFNodeTags :: Node -> [(String,Node)] -> CGBState h ()
projectCFNodeTags node tags' =
  do cgbState <- get
     let cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         isDataFlow cfEdge = case cfEdge of { CFDataFlow _ -> True ; _ -> False }
         adjNodes = node : concatMap (\(n0,n1,e) -> if n0 == node && isDataFlow e then [n1] else []) cfEdges
         adjCFNodes = concatMap (\n -> case List.lookup n cfNodes of
                                         Just cfNode -> [(n,cfNode)]
                                         Nothing -> error $ "updateTags missing node: " ++ show n
                                ) adjNodes
         projectTags tagsA tagsB = tagsB ++ filter (\(lblA,_) -> not $ lblA `elem` (map fst tagsB)) tagsA
     mapM_ (\(n,cfNode) ->
              case cfNode of
                CFNode tags -> cgbUpdateCFNode n $ CFNode $ projectTags tags' tags
                CFBranchNode b tags -> cgbUpdateCFNode n $ CFBranchNode b $ projectTags tags' tags
                _ -> error $ "projectCFNodeTags given non-CFNode: " ++ show (n,cfNode)
           ) adjCFNodes

-- | Back project tags onto a given @CFNode@. So given a node matching @CFNode tags@,
-- add any pair (tag',n) in tags' to tags when tag' is not alredy in tags. Also,
-- do the same thing for any adjacent node (n0,n) connected by a @CFDataFlow@ edge
backProjectCFNodeTags :: Node -> [(String,Node)] -> CGBState h ()
backProjectCFNodeTags node tags' =
  do cgbState <- get
     let cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         isDataFlow cfEdge = case cfEdge of { CFDataFlow _ -> True ; _ -> False }
         adjNodes = node : concatMap (\(n0,n1,e) -> if n1 == node && isDataFlow e then [n0] else []) cfEdges
         adjCFNodes = concatMap (\n -> case List.lookup n cfNodes of
                                         Just cfNode -> [(n,cfNode)]
                                         Nothing -> error $ "updateTags missing node: " ++ show n
                                ) adjNodes
         projectTags tagsA tagsB = tagsB ++ filter (\(lblA,_) -> not $ lblA `elem` (map fst tagsB)) tagsA
     mapM_ (\(n,cfNode) ->
              case cfNode of
                CFNode tags -> cgbUpdateCFNode n $ CFNode $ projectTags tags' tags
                CFBranchNode b tags -> cgbUpdateCFNode n $ CFBranchNode b $ projectTags tags' tags
                _ -> error $ "projectCFNodeTags given non-CFNode: " ++ show (n,cfNode)
           ) adjCFNodes

-- * By CFNode (unsafe)

-- TODO generalize branchCFN to branchCFG like composeCFG

-- | Given three @CFNode@'s @cNode@,@aNode@ and @bNode@, manually add @CFEdge@'s
-- @CFBranchEQ@ from @cNode@ to @aNode@ and @CFBranchNE@ from @cNode@ to
-- @bNode@.
-- Tag zipping is enforced "surjective", so @cNode@ projects its nodes onto
-- @aNode@ and @bNode@ and any nodes connected to them by a @CFDataFlow@ edge
-- (read description in @composeCFN@)
branchCFN :: (String, Node) -> (String,Node) -> (String,Node) -> CGBState h ()
branchCFN (labelC,cNode) (labelA,aNode) (labelB,bNode) =
  do cgbState <- get
     let cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         cTags = case List.lookup cNode cfNodes of
                   Just (CFBranchNode _ tags)-> tags
                   Just _ -> error $ "branchCFN given cNode that isn't CFBranchNode: " ++ show cNode
                   Nothing -> error $ "branchCFN given missing node for cNode: " ++ show cNode
         aTags = case List.lookup aNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "branchCFN given bad node type for aNode: " ++ show aNode
                   Nothing -> error $ "branchCFN given missing node for aNode: " ++ show aNode
         bTags = case List.lookup bNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "branchCFN given bad node type for bNode: " ++ show bNode
                   Nothing -> error $ "branchCFN given missing node for bNode: " ++ show bNode
     cgbAddCFEdge ((cNode,aNode)) $ CFBranchEQ labelA
           $ tieNodesByTagModulo (labelC,cTags) (labelA,aTags)
     cgbAddCFEdge ((cNode,bNode)) $ CFBranchNE labelB
           $ tieNodesByTagModulo (labelC,cTags) (labelB,bTags)
     projectCFNodeTags aNode cTags
     projectCFNodeTags bNode cTags

-- | Given two @CFNode@'s @aNode@ and @bNode@, manually add @CFEdge@'s
-- @CFCompose@ from @aNode@ to @cNode@ to.
-- Tag zipping is enforced "surjective", so all nodes in @aNode@ are zipped
-- together by tag to @aNode@ and @bNode@, if a tag in @aNode@ is missing from
-- @bNode@ it is added, and if a tag in @bNode@ is missing from @cNode@ an error
-- is thrown. When adding a missing node, it is added to both @bNode@ and any
-- adjacent @CFNode@'s to @bNode@ connected by a @CFDataFlow@ edge
composeCFG :: (String -> String -> Bool)
           -> (String,Node)
           -> (String,Node)
           -> CGBState h ()
composeCFG cmp (labelA,aNode) (labelB,bNode) =
  do cgbState <- get
     let cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         aTags = case List.lookup aNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "composeCFN given bad node type for aNode: " ++ show (labelA,aNode)
                   Nothing -> error $ "composeCFN given missing node for aNode: " ++ show (labelA,aNode)
         bTags = case List.lookup bNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "composeCFN given bad node type for bNode: " ++ show (labelB,bNode)
                   Nothing -> error $ "composeCFN given missing node for bNode: " ++ show (labelB,bNode)
         missingTags = filter (\bTag -> not $ bTag `elem` map fst aTags) $ map fst bTags
         bTags' = case missingTags of
                    [] -> bTags ++ filter (\(aTag,_) -> not $ aTag `elem` (map fst bTags)) aTags
                    _ -> error $ "Missing tags "++show missingTags++" from nodes "++show (aNode,bNode)
     cgbAddCFEdge (aNode,bNode) $ CFCompose (labelA,labelB)
                                $ (tieNodesBy cmp) (labelA,aTags) (labelB,bTags)
     projectCFNodeTags bNode aTags

-- | Compose two nodes via composeCFG with (==) supplied to tieNodesBy
composeCFN :: (String,Node)
           -> (String,Node)
           -> CGBState h ()
composeCFN = composeCFG (==)

-- | Compose two nodes via composeCFG with (==), except when tag is lbl:i then
-- tie to lbl:i+1 (suitable for tying between modulo staging)
composeCFM :: (String,Node)
           -> (String,Node)
           -> CGBState h ()
composeCFM =composeCFG cmpTagsByStage

-- | Given two @CFNode@'s manually add a @CFJump@ edge. Unlike @branchCFN@ this
-- does not "project" nodes
jumpCFG :: (String -> String -> Bool) -> (String,Node) -> (String,Node) -> CGBState h ()
jumpCFG cmp (labelA,aNode) (labelB,bNode) =
  do cgbState <- get
     let cfBuilder = cgbCFBuilder cgbState
         cfNodes = cfbNodes cfBuilder
         cfEdges = cfbEdges cfBuilder
         aTags = case List.lookup aNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "jumpCFN given bad node type for aNode: " ++ show (labelA,aNode)
                   Nothing -> error $ "jumpCFN given missing node for aNode: " ++ show (labelA,aNode)
         bTags = case List.lookup bNode cfNodes of
                   Just (CFNode tags) -> tags
                   Just (CFBranchNode _ tags) -> tags
                   Just _ -> error $ "jumpCFN given bad node type for bNode: " ++ show (labelB,bNode)
                   Nothing -> error $ "jumpCFN given missing node for bNode: " ++ show (labelB,bNode)
     cgbAddCFEdge (aNode,bNode) $ CFJump (labelA,labelB)
       $ (tieNodesBy cmp) (labelA,aTags) (labelB,bTags)

-- | Add a jump between two nodes via composeCFG with (==) supplied to tieNodesBy
jumpCFN :: (String,Node) -> (String,Node) -> CGBState h ()
jumpCFN = jumpCFG (==)

-- | Add a jump between two nodes via composeCFG with (==), except when tag is
-- lbl:i then tie to lbl:i+1 (suitable for tying between modulo staging)
jumpCFM :: (String,Node) -> (String,Node) -> CGBState h ()
jumpCFM = jumpCFG cmpTagsByStage

-----------------------------------------------------------------------------------------------------
-- Test functions
-----------------------------------------------------------------------------------------------------
testCond :: forall repr. CoreISA repr => (repr GPR,repr GPR) -> (repr BRANCH,(repr GPR,repr GPR))
testCond (g0,g1) =
  let
    g3 = unintegerG 1
    g4 = addG g0 g3
    g5 = branchNotLow g0 g1
  in (g5,(g4,g1))

testA :: forall repr . CoreISA repr => (repr GPR,repr GPR) -> repr GPR
testA (g0,g1) =
  let
    g2 = addG g0 g1
  in g2

testB :: forall repr . CoreISA repr => (repr GPR,repr GPR) -> repr GPR
testB (g0,g1) =
  let
    g2 = addG g0 g1
    g3 = addG g2 g1
  in g3

-- testPost :: forall repr . CoreISA repr => repr GPR -> repr GPR
-- testPost g0 =
--   let
--     g1 = negG g0
--   in g1

-- TODO update testBranch to work with tags
-- testBranch :: forall h . (Hardware h, CoreISA (Graph h)) => Block h ((GPR,GPR),GPR)
-- testBranch =
--   let
--     cBlock = branchingBlock "cond" testCond
--     aBlock = basicBlock "a" (testA @(Graph h))
--     bBlock = basicBlock "b" (testB @(Graph h))
--     pBlock = basicBlock "post" (testPost @(Graph h))
--   in branch cBlock aBlock bBlock pBlock

