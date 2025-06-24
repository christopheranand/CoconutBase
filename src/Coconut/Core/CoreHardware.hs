-- |
-- Module      :  Coconut.Core.CoreHardware
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports a functionality generating a implementing CORE hardware functionality

{-# LANGUAGE FlexibleInstances,RecordWildCards,ScopedTypeVariables,InstanceSigs,TypeApplications,AllowAmbiguousTypes,OverloadedStrings,ExistentialQuantification,
    RecordWildCards #-}
module Coconut.Core.CoreHardware where

import Data.Bifunctor (first, bimap)
import Data.Tuple (swap)
import Data.Typeable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, fromJust)
import Data.ByteString (ByteString)
import qualified Data.List as List
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Graph.Inductive.Graph

import Debug.Trace

import Control.Monad.State
import Control.Monad.Except

import System.IO

import Coconut.BaseTypes
import Coconut.Core.CoreISA
import Coconut.Graph.CodeGraph
import Coconut.Core.Interp
import Coconut.Simulator
import Coconut.Utils.ArbFloat (word642Dbl)
import Numeric (showHex)

-- | Datatype used to index @Hardware@ instances
data CORE

data instance SimError CORE = LookupError String
                            | FuncNotFound String
                            | FuncCastError String
                            | NodeError Node
                            | BadNodeLookupInCG Node
                            | BranchInstructionNameNotFound Node
                            | OtherError String
                            deriving Show

-- TODO: We also need to print information for dbx, which we can use this data type for
--  by deriving a custom show instance of this type
data instance SimOutput CORE =
      DataInstrOutput
      { instr :: String
      , imms :: [Int]
      , args :: [(RegType CORE,ByteString,Either (Interp GPR) (Interp VR))]
      , modifiedGPRs :: [(ByteString, Interp GPR)]
      , modifiedVRs :: [(ByteString, Interp VR)]
      }
    | BranchOutput
      { branchInstrName :: String
      , branchConditionOutput :: Bool
      , comparisonOps :: [(ByteString, Interp GPR)]
      }
    | MemoryOutput
      { instr :: String
      , imms :: [Int]
      , args :: [(RegType CORE,ByteString,Either (Interp GPR) (Interp VR))]
      , newMemRegion :: Interp MR
      , mLoadOutput :: Maybe (Either (Interp GPR) (Interp VR))
      }
    | SpillOutput
      { isDespill :: Bool
      , spillIndex :: Int
      , spillArgs :: [(RegType CORE,ByteString,Either (Interp GPR) (Interp VR))]
      , spillVal :: [Word64]
      }
    | NoOp
    | AddInitMR Node

instance Hardware CORE  where

  -- | @Hardware@ @CORE@ supports two register types, @GPR@ (general), @VR@ (vector)
  data RegType CORE = GPR | VR
    deriving (Typeable,Eq,Show)

  -- | @Hardware@ @CORE@ keeps track of a two register files @hardwareST_GPRs@ and @hardwareST_VRs@
  data HardwareST CORE = HardwareST { hardwareST_GPRs :: Map ByteString (Interp GPR)
                                    , hardwareST_VRs :: Map ByteString (Interp VR)
                                    , branchCondition :: Bool
                                    , hardwareST_MRTables ::  [(ByteString,Interp MR)]
                                    , hardwareST_SpillTable :: Map Int (Interp SPILL) -- index by byte address
                                    }
    deriving Show

  data MetaData CORE = MDCore { mdCorePName   :: ByteString
                              , mdCoreLatency :: Int
                              , mdFormatConv :: [Int] -> InstructionFormat CORE
                              }

  -- | @MetaData CORE@ has latencies accessed via @mdCoreLatency@
  hardwareLatency = mdCoreLatency

  -- | @initHardwareST@ @CORE@ has empty register files and a condition register of 0
  -- | initMem is the initial memory layout
  initHardwareST (regMap,spillMap) initMem (inputGPRs, inputVRs) =
    let
      pullGPRsFromRegMap = map snd $ filter ((== GPR) . fst) $ Map.elems regMap
      pullVRsFromRegMap  = map snd $ filter ((== VR) . fst) $ Map.elems regMap
    in HardwareST
       { hardwareST_GPRs = Map.fromList $ zip pullGPRsFromRegMap (repeat (InterpGPR 0)) ++ inputGPRs
       , hardwareST_VRs = Map.fromList $ zip pullVRsFromRegMap (repeat (InterpVR (0, 0))) ++ inputVRs
       , branchCondition = False
       , hardwareST_MRTables = map (\(label, node, vrs)
                                    -> (node, initMR (fromIntegral $ length vrs) label vrs)) initMem
       , hardwareST_SpillTable = Map.fromList
                                 -- NOTE 8 bytes per Word64, 2 Word64s per VR
                                 $ concatMap (\n -> [(n*8*2,InterpSPILL (0,0))
                                                    ,(n*8*2+spillSpaceSize*8,InterpSPILL (0,0))])
                                 $ [0..(spillSpaceSize `div` 2) -1]
       }

  memTables = hardwareST_MRTables

  getBranchCond = branchCondition

  -- | @CORE@ contains two register types @GPR@ and @VR@
  allRegTypes = [GPR, VR]

  -- | @CORE@ uses @GPR@'s for memory regions
  memRegType = GPR

  -- | @CORE@ uses a @GPR@ and the register 6 to store the constant table address
  constantReg = (GPR,"6")

  -- | @CORE@ has a single type identifie-able as a GPR
  isGPR reg = case reg of
                GPR -> True
                _ -> False

  isHStore (InstructionNode (Instruction _ name _)) =
    case name of
      "moduloVStore" -> True
      "stvxMR" -> True
      "stdMR" -> True
      "stwMR" -> True
      "stdxMR" -> True
      "stv0MR" -> True
      _ -> False
  isHStore _ = False

  isHLoad (InstructionNode (Instruction _ name _)) =
    case name of
      "moduloVLoad" -> True
      "ldMR" -> True
      "ldxMR" -> True
      "lwzMR" -> True
      "lgMR" -> True
      "vldxMR" -> True
      "vld0MR" -> True
      "vldInMR" -> True
      "vleb" -> True
      "vleh" -> True
      "vlef" -> True
      "vleg" -> True
      "vlegAll" -> True
      _ -> False
  isHLoad _ = False

  isModuloLoad (InstructionNode (Instruction _ name _)) =
    case name of
      "moduloVLoad" -> True
      "moduloVLoadG" -> True
      _ -> False
  isModuloLoad _ = False

  isModuloStore (InstructionNode (Instruction _ name _)) =
    case name of
      "moduloVStore" -> True
      "moduloVStoreG" -> True
      _ -> False
  isModuloStore _ = False

  isIncMR (InstructionNode (Instruction _ name _)) =
    case name of
      "incMR" -> True
      "incMR0" -> True
      _ -> False
  isIncMR _ = False

  -- | @CORE@ identifies constant loads by the prefix "un"
  isConstantLoad dfGraph node =
    let
      hasConstName ('u':'n':_) = True
      hasConstName _ = False
    in case match node (dataFlowGraph dfGraph ) of
                      (Just (_,_,InstructionNode el,_),_) -> case el of
                                                  Instruction _ name _ -> hasConstName name
                                                  _ -> False
                      _ -> False

  -- | @CORE@ contains registers for @GPR@ and @VR@ hardware
  regColors regType = case regType of
                        -- GPR -> map (BS.pack . show) $ [1..3] ++ [5] ++ [7..15] -- r6 points to constant table
                        --                                           -- also r4 is the stack pointer
                        GPR -> map (BS.pack . show) $ [1..3] ++ [5] ++ [7..15]
                        VR -> map (BS.pack . show) [0..31]

  -- | @CORE@ supports three register types: @GPR@, @VR@ and @CR@
  typeToReg ty
    | ty == result @GPR = GPR -- return the data level GPR from the type level GPR
    | ty == result @CR  = GPR -- use GPR's as CR's at the data level for CORE
    | ty == result @VR  = VR
    -- if there is a mismatch when generating a @CodeGraph@ it will throw a runtime error
    | otherwise         = error "Hardware CORE: encountered unsupported typeRep"
    where
      result :: forall x. Typeable x => TypeRep
      result = typeRep (Proxy @x)

  -- | @CORE@ has the following reg prefixes: (@GPR@,"r" ), (@VR@,"v")
  regPrefix reg = case reg of
                    GPR -> "r"
                    VR  -> "v"
    where
      result :: forall x. Typeable x => TypeRep
      result = typeRep (Proxy @x)

  -- | @CORE@ supports two spill names @GPR@ is "spillGPR" and @VR@ is "spillVR"
  spillName regType = case regType of
                        GPR -> "spillGPR"
                        VR -> "spillVR"

  -- | @CORE@ supports two despill names @GPR@ is "despillGPR" and @VR@ is "despillVR"
  despillName regType = case regType of
                          GPR -> "despillGPR"
                          VR -> "despillVR"

  -- | @CORE@ supports two moves for @GPR@ and @VR@
  moveName regType = case regType of
                       GPR -> "movGPR"
                       VR -> "movVR"

  -- TODO implement runInstruction in Hardware CORE instance
  runInstruction handle schedGraph cg (regMap,spillMap) node =
    let
      -- Custom functions that throw a SimError exception
      fromLookupToExcept :: (Show a, Show b, Eq a) => String -> a -> [(a, b)] -> SimState CORE b
      fromLookupToExcept label k xs  =
        case lookup k xs of
          Nothing -> throwError $ LookupError $ "Failed to find " ++ show k ++ " in structure "
                     ++ label ++ ": " ++ show xs
                     -- error $ "Failed to find " ++ show k ++ " in structure: " ++ show xs
          Just v -> pure v

      -- Throw an error if map lookup fails
      fromMapLookupToExcept :: (Show k, Show v, Ord k) => String -> k -> Map k v -> SimState CORE v
      fromMapLookupToExcept label k m =
        case Map.lookup k m of
          Nothing -> throwError $ LookupError $ "Failed to find " ++ show k ++ " in structure "
                     ++ label ++ ": "++ show m
                    -- error $ "Failed to find " ++ show k ++ " in structure: " ++ show m
          Just v -> pure v

      fetchImmediates :: SimState CORE [Int]
      fetchImmediates = case matchNodeInCG cg node of
        CGDataFlowNode (_, InstructionNode Instruction{..}) ->
          pure elImmediates
        CGDataFlowNode (_, InstructionNode _) ->
          pure []
        _ ->
          throwError $ BadNodeLookupInCG node

      fetchBranchInstrName :: SimState CORE String
      fetchBranchInstrName = case matchNodeInCG cg node of
        CGDataFlowNode (_, BranchNode{..}) ->
          pure instructionName
        _ ->
          throwError $ BranchInstructionNameNotFound node

      -- Handles updating the monadic state depending on the node, or branches first before updating
      callUpdate :: HardwareST CORE -> SimState CORE (SimOutput CORE)
      callUpdate hState = case matchNodeInCG cg node of
        CGDataFlowNode (_, InstructionNode el@(InitMR _ _)) ->
          initializeMR el hState
        CGDataFlowNode (_, InstructionNode el) -> do
          imms <- fetchImmediates
          updateState imms el hState
        CGDataFlowNode (_, BranchNode brName brImm fn) -> do
          name <- fetchBranchInstrName
          branchAndUpdateState name fn brImm hState
        -- Skip over control flow input and output nodes
        CGControlFlowNode _ ->
          pure NoOp
        _ ->
          throwError $ NodeError node

      inOuts = case lab schedGraph node of
                     Just (ScheduledNode instrNode uses defs _ _ _) -> (uses, defs)
                     Nothing -> error $ "Hardware.runInstruction bad node lookup: " ++ show node

      -- fetch the unmapped node for debug label lookup
      unmappedNode = case lab schedGraph node of
        Just (ScheduledNode _ _ _ unmappedNd _ _) -> unmappedNd
        Nothing -> error $ "Hardware.runInstruction bad node lookup: " ++ show node

      -- Look up a register in the memory region tables
      memLookup :: ByteString -> [(ByteString, Interp MR)] -> Interp MR
      memLookup reg mrTables =
        case List.lookup reg mrTables of
          Nothing -> error $ "Couldn't find reg in mrTables: " ++ show reg ++ "\n and hState: " ++ show mrTables
          Just vrs -> vrs

      initializeMR :: EL CORE -> HardwareST CORE -> SimState CORE (SimOutput CORE)
      initializeMR (InitMR _ _) hState =
        case inOuts of
          ([], [(n, _)]) -> do
            (label,mrTable) <- fromLookupToExcept "initializeMR" node $ map (\(lab,n,vrs) -> (n,(lab,vrs))) (cgMRTables cg)
            let
              mrTableReg = snd $ fromJust $ Map.lookup n regMap
              mrTableConversion = (mrTableReg, initMR (fromIntegral $ length mrTable) label mrTable)
              newState =
                hState { hardwareST_MRTables = hardwareST_MRTables hState ++ [mrTableConversion] }
            -- FIXME this never gets printed out?
            -- liftIO $ if label=="inMR" || label=="outMR"
            --             then do { putStrLn ("Initalized MR Region: "++show label)
            --                     ; mapM_ (putStrLn . show) mrTable
            --                     }
            --             else return ()
            put newState
            return $ AddInitMR n

      -- TODO: Call this to run branch nodes and update state
      branchAndUpdateState :: Typeable fn => String -> fn -> [Int] -> HardwareST CORE -> SimState CORE (SimOutput CORE)
      branchAndUpdateState name f bImm hState =
            case inOuts of
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR)],[]) ->
                do df <-
                     maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp BRANCH") pure $
                     (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp BRANCH)) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   r0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   r1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   let val = df r0 r1
                       newState = hState { branchCondition = runInterpBRANCH val }
                   put newState
                   return $ BranchOutput name (branchCondition newState) [(reg0, r0), (reg1, r1)]
              ([(n0,RegisterRes GPR)],[]) ->
                do df <-
                     maybe (throwError $ FuncCastError "Interp GPR -> Interp BRANCH") pure
                     (cast f :: Maybe (Interp GPR -> Interp BRANCH)) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   r0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   let val = df r0
                       newState = hState { branchCondition = runInterpBRANCH val }
                   put newState
                   return $ BranchOutput name (branchCondition newState) [(reg0, r0)]
              ([],[]) -> do
                   put (hState { branchCondition = True})
                   return $ BranchOutput name True []

      -- Update the monadic simulator state
      updateState :: [Int] -> EL CORE -> HardwareST CORE -> SimState CORE (SimOutput CORE)
      updateState _ (Move instrName _) hState =
        case inOuts of
          ([(n0,RegisterRes GPR)],[(n1,RegisterRes GPR)]) -> do
            (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
            (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
            let hstGPRs = hardwareST_GPRs hState
                regVal = runInterpGPR $ fromJust $ Map.lookup reg0 hstGPRs
            r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
            put $ hState { hardwareST_GPRs = Map.insert reg0 r0 $ hardwareST_GPRs hState }
            return $ DataInstrOutput instrName [] [(GPR,reg0,Left r0)] [(reg1,r0)] []
          ([(n0,RegisterRes VR)],[(n1,RegisterRes VR)]) -> do
            (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
            (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
            let hstGPRs = hardwareST_GPRs hState
                regVal = runInterpGPR $ fromJust $ Map.lookup reg0 hstGPRs
            v0 <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
            put $ hState { hardwareST_VRs = Map.insert reg0 v0 $ hardwareST_VRs hState }
            return $ DataInstrOutput instrName [] [(VR,reg0,Right v0)] [] [(reg1,v0)]
          _ -> throwError $ FuncNotFound $ "runInstruction failed on move " ++ show node
      updateState _ (Spill _name) hState =
        case inOuts of
          -- No FIFO
          ([(r0,RegisterRes GPR)],[(s0,SpillRes)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            let spillTable = hardwareST_SpillTable hState
                hstGPRs = hardwareST_GPRs hState
                spillVal = runInterpGPR $ fromJust $ Map.lookup reg0 hstGPRs
                spillByteIndex = spillIndex * 8 * 2
            put $ hState { hardwareST_SpillTable =
                           Map.insert spillByteIndex (InterpSPILL (spillVal,0)) spillTable }
            return $ SpillOutput False spillByteIndex [(GPR,reg0,Left $ InterpGPR spillVal)] [spillVal,0]

          ([(r0,RegisterRes VR)],[(s0,SpillRes)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            let spillTable = hardwareST_SpillTable hState
                hstVRs = hardwareST_VRs hState
                spillVal = runInterpVR $ fromJust $ Map.lookup reg0 hstVRs
                spillByteIndex = spillIndex * 8 * 2
            put $ hState { hardwareST_SpillTable =
                           Map.insert spillByteIndex (InterpSPILL spillVal) spillTable }
            return $ SpillOutput False spillByteIndex [(VR,reg0,Right $ InterpVR spillVal)] [fst spillVal
                                                                                            ,snd spillVal]
         -- FIFO
          ([(r0,RegisterRes GPR),(r1,RegisterRes GPR)],[(s0,SpillRes)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            (_,reg1) <- fromMapLookupToExcept "regMap" r1 regMap
            let spillTable = hardwareST_SpillTable hState
                hstGPRs = hardwareST_GPRs hState
                spillVal = runInterpGPR $ fromJust $ Map.lookup reg0 hstGPRs
                fifoPtr = fromIntegral $ runInterpGPR $ fromJust $ Map.lookup reg1 hstGPRs
                spillByteIndex = spillIndex * 8 * 2
            put $ hState { hardwareST_SpillTable =
                           Map.insert (spillByteIndex+fifoPtr) (InterpSPILL (spillVal,0)) spillTable }
            return $ SpillOutput False (spillByteIndex+fifoPtr) [(GPR,reg0,Left $ InterpGPR spillVal)] [spillVal,0]

          ([(r0,RegisterRes VR),(r1,RegisterRes GPR)],[(s0,SpillRes)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            (_,reg1) <- fromMapLookupToExcept "regMap" r1 regMap
            let spillTable = hardwareST_SpillTable hState
                hstVRs = hardwareST_VRs hState
                hstGPRs = hardwareST_GPRs hState
                spillVal = runInterpVR $ fromJust $ Map.lookup reg0 hstVRs
                fifoPtr = fromIntegral $ runInterpGPR $ fromJust $ Map.lookup reg1 hstGPRs
                spillByteIndex = spillIndex * 8 * 2
            put $ hState { hardwareST_SpillTable =
                           Map.insert (spillByteIndex+fifoPtr) (InterpSPILL spillVal) spillTable }
            return $ SpillOutput False (spillByteIndex+fifoPtr) [(VR,reg0,Right $ InterpVR spillVal)] [fst spillVal
                                                                                                      ,snd spillVal]
          _ -> throwError $ FuncNotFound $ "runInstruction failed on spill " ++ show node
      updateState _ (Despill _name) hState =
        case inOuts of
          -- No FIFO
          ([(s0,SpillRes)],[(r0,RegisterRes GPR)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            let spillTable = hardwareST_SpillTable hState
                spillByteIndex = spillIndex * 8 * 2
                (spillVal,_) = runInterpSPILL $ fromJust $ Map.lookup spillByteIndex spillTable
                hstGPRs = hardwareST_GPRs hState
            put $ hState { hardwareST_GPRs = Map.insert reg0 (InterpGPR spillVal) hstGPRs }
            return $ SpillOutput True spillByteIndex [(GPR,reg0,Left $ InterpGPR spillVal)] [spillVal,0]

          ([(s0,SpillRes)],[(r0,RegisterRes VR)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            let spillTable = hardwareST_SpillTable hState
                spillByteIndex = spillIndex * 8 * 2
                spillVal = runInterpSPILL $ fromJust $ Map.lookup spillByteIndex spillTable
                hstVRs = hardwareST_VRs hState
            put $ hState { hardwareST_VRs = Map.insert reg0 (InterpVR spillVal) hstVRs }
            return $ SpillOutput True spillByteIndex [(VR,reg0,Right $ InterpVR spillVal)] [fst spillVal
                                                                                           ,snd spillVal]
          -- FIFO
          ([(s0,SpillRes),(r1,RegisterRes GPR)],[(r0,RegisterRes GPR)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            (_,reg1) <- fromMapLookupToExcept "regMap" r1 regMap
            let spillTable = hardwareST_SpillTable hState
                spillByteIndex = spillIndex * 8 * 2
                fifoPtr = fromIntegral $ runInterpGPR $ fromJust $ Map.lookup reg1 hstGPRs
                (spillVal,_) = runInterpSPILL $ fromJust $ Map.lookup (spillByteIndex+fifoPtr) spillTable
                hstGPRs = hardwareST_GPRs hState
            put $ hState { hardwareST_GPRs = Map.insert reg0 (InterpGPR spillVal) hstGPRs }
            return $ SpillOutput True (spillByteIndex+fifoPtr) [(GPR,reg0,Left $ InterpGPR spillVal)] [spillVal,0]

          ([(s0,SpillRes),(r1,RegisterRes GPR)],[(r0,RegisterRes VR)]) -> do
            spillIndex <- fromMapLookupToExcept "spillMap" s0 spillMap
            (_,reg0) <- fromMapLookupToExcept "regMap" r0 regMap
            (_,reg1) <- fromMapLookupToExcept "regMap" r1 regMap
            let spillTable = hardwareST_SpillTable hState
                spillByteIndex = spillIndex * 8 * 2
                fifoPtr = fromIntegral $ runInterpGPR $ fromJust $ Map.lookup reg1 hstGPRs
                spillVal = runInterpSPILL $ fromJust $ Map.lookup (spillByteIndex+fifoPtr) spillTable
                hstVRs = hardwareST_VRs hState
                hstGPRs = hardwareST_GPRs hState
            put $ hState { hardwareST_VRs = Map.insert reg0 (InterpVR spillVal) hstVRs }
            return $ SpillOutput True (spillByteIndex+fifoPtr) [(VR,reg0,Right $ InterpVR spillVal)] [fst spillVal
                                                                                                     ,snd spillVal]
          _ -> throwError $ FuncNotFound $ "runInstruction failed on despill " ++ show node

      -- TODO: Apr 8: For now, check for InitMR nodes and explicitly skip
      updateState _ (InitMR _ _) hState = pure NoOp
      updateState imms (Instruction imm instrName f) hState =
            -- Format: ([(input, type)], [(output, type)])
            -- Cast type to a function df and apply df to relevant memory tables
            -- Set the new monadic state i.e. (prevState, df) -> newState
            -- Many of the inOuts cases correspond to specific instructions in CoreISA and implemented in Interp,
            -- Example instructions are commented by "e.g."
            case inOuts of
              ([(m0,MemoryRes _)],[(m1,MemoryRes _)]) -> do
                df <-
                        maybe (throwError $ FuncCastError "Interp MR -> Interp MR") pure
                        (cast f :: Maybe (Interp MR -> Interp MR))
                (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                let mrTables = hardwareST_MRTables hState
                    table0 = memLookup m0res mrTables
                    newTable = df table0
                    mrTables' = case List.lookup m1res mrTables of
                                        Just _ -> [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                let tVal = if n == m1res then (n, newTable) else (n, t) ]
                                        Nothing -> mrTables ++ [(m1res,newTable)]
                put $ hState { hardwareST_MRTables = mrTables'
                        }
                return $ MemoryOutput instrName imms [] newTable Nothing
              ([(m0,MemoryRes _)],[(n1,RegisterRes GPR),(m1,MemoryRes _)]) -> do
                df <-
                  maybe (throwError $ FuncCastError "Interp MR -> (Interp GPR, Interp MR)") pure
                  (cast f :: Maybe (Interp MR -> (Interp GPR, Interp MR)))
                (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                let table0 = memLookup m0res (hardwareST_MRTables hState)
                    table1 = memLookup m1res (hardwareST_MRTables hState)
                    (val, newTable) = df table0
                put $ hState { hardwareST_GPRs = Map.insert reg1 val $ hardwareST_GPRs hState
                            , hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                            }
                return $ MemoryOutput instrName imms [] newTable (Just (Left val))
              -- TODO: Arguments appear swapped in the SchedGraph when it comes to MR
              -- e.g. ldxMR
              ([(m0,MemoryRes _),(n0,RegisterRes GPR)],[(n1,RegisterRes GPR),(m1,MemoryRes _)]) -> do
                df <- maybe (throwError $ FuncCastError "Interp MR -> Interp GPR -> (Interp GPR, Interp MR)") pure $ (cast f :: Maybe (Interp MR -> Interp GPR -> (Interp GPR, Interp MR)))
                (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                let table0 = memLookup m0res (hardwareST_MRTables hState)
                    table1 = memLookup m1res (hardwareST_MRTables hState)
                    (val, newTable) = df table0 r0
                put $ hState { hardwareST_GPRs = Map.insert reg1 val $ hardwareST_GPRs hState
                                , hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                             }
                return $ MemoryOutput instrName imms [(GPR, reg1, Left r0)] newTable (Just (Left val))
              -- e.g. vldxMR
              ([(m0,MemoryRes _),(n0,RegisterRes GPR)],[(n1,RegisterRes VR),(m1,MemoryRes _)]) -> do
                df <- maybe (throwError $ FuncCastError "Interp MR -> Interp GPR -> (Interp VR, Interp MR)") pure $ (cast f :: Maybe (Interp MR -> Interp GPR -> (Interp VR, Interp MR)))
                (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                let table0 = memLookup m0res (hardwareST_MRTables hState)
                    table1 = memLookup m1res (hardwareST_MRTables hState)
                    (val, newTable) = df table0 r0
                put $ hState { hardwareST_VRs = Map.insert reg1 val $ hardwareST_VRs hState
                                , hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                               let tVal = if n == m0res then (n, newTable) else (n, t) ]
                             }
                return $ MemoryOutput instrName imms [(GPR, reg0, Left r0)] newTable (Just (Right val))
              -- e.g. vleb
              ([(m0,MemoryRes _),(n0,RegisterRes GPR),(n1,RegisterRes VR)],[(n2,RegisterRes VR),(m1,MemoryRes _)]) -> do
                df <- maybe (throwError $ FuncCastError "Interp MR -> Interp GPR -> Interp VR -> (Interp VR, Interp MR)") pure $ (cast f :: Maybe (Interp MR -> Interp GPR -> Interp VR -> (Interp VR, Interp MR)))
                (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                r1 <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                let table0 = memLookup m0res (hardwareST_MRTables hState)
                    table1 = memLookup m1res (hardwareST_MRTables hState)
                    (val, newTable) = df table0 r0 r1
                put $ hState { hardwareST_VRs = Map.insert reg1 val $ hardwareST_VRs hState
                                , hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                                }
                return $ MemoryOutput instrName imms [(GPR, reg0, Left r0), (VR, reg1, Right r1)] newTable (Just (Right val))
              ([(m0,MemoryRes _),(n0,RegisterRes GPR),(n1,RegisterRes GPR)],[(n2,RegisterRes VR),(m1,MemoryRes _)]) -> do
                df <- maybe (throwError $ FuncCastError "Interp MR -> Interp GPR -> Interp GPR -> (Interp VR, Interp MR)") pure $ (cast f :: Maybe (Interp MR -> Interp GPR -> Interp GPR -> (Interp VR, Interp MR)))
                (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                r1 <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                let table0 = memLookup m0res (hardwareST_MRTables hState)
                    table1 = memLookup m1res (hardwareST_MRTables hState)
                    (val, newTable) = df table0 r0 r1
                put $ hState { hardwareST_VRs = Map.insert reg1 val $ hardwareST_VRs hState
                                , hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                             }
                return $ MemoryOutput instrName imms [(GPR, reg0, Left r0), (GPR, reg1, Left r1)] newTable (Just (Right val))
              ([(m0,MemoryRes _),(n0,RegisterRes VR)],[(n1,RegisterRes VR),(m1,MemoryRes _)]) -> do
                df <- maybe (throwError $ FuncCastError "Interp MR -> Interp VR -> (Interp VR, Interp MR)") pure $ (cast f :: Maybe (Interp MR -> Interp VR -> (Interp VR, Interp MR)))
                (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                v0 <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                let table0 = memLookup m0res (hardwareST_MRTables hState)
                    table1 = memLookup m1res (hardwareST_MRTables hState)
                    (val, newTable) = df table0 v0
                put hState { hardwareST_VRs = Map.insert reg1 val $ hardwareST_VRs hState
                                , hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                           }
                return $ MemoryOutput instrName imms [(VR, reg0, Right v0)] newTable (Just (Right val))
                --e.g. stdMR  :: repr MR -> Int -> repr GPR   -> repr MR
              ([(m0,MemoryRes _),(n0,RegisterRes GPR)],[(m1,MemoryRes _)]) -> do
                        df <- maybe (throwError $ FuncCastError "Interp MR -> Interp GPR -> Interp MR") pure $ (cast f :: Maybe (Interp MR -> Interp GPR -> Interp MR))
                        (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                        (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                        (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                        r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                        let table0 = memLookup m0res (hardwareST_MRTables hState)
                            -- table1 = memLookup m1res (hardwareST_MRTables hState)
                            newTable = df table0 r0
                            mrTables' = case List.lookup m1res (hardwareST_MRTables hState) of
                                        Just _ -> [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                        let tVal = if n == m1res then (n, newTable) else (n, t) ]
                                        Nothing -> (hardwareST_MRTables hState) ++ [(m1res,newTable)]
                        put $ hState {  hardwareST_MRTables = mrTables'
                                        }
                              `debug` ("Generated new table: " ++ show newTable
                                      ++"\n\nOld table: " ++ show table0)
                        return $ MemoryOutput instrName imms [(GPR, reg0, Left r0)] newTable Nothing
              -- e.g. stv0MR
              ([(m0,MemoryRes _),(n0,RegisterRes VR)],[(m1,MemoryRes _)]) -> do
                  df <- maybe (throwError $ FuncCastError "Interp MR -> Interp VR -> Interp MR") pure $ (cast f :: Maybe (Interp MR -> Interp VR -> Interp MR))
                  (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                  (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                  (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                  r0 <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                  let table0 = memLookup m0res (hardwareST_MRTables hState)
                      table1 = memLookup m1res (hardwareST_MRTables hState)
                      newTable = df table0 r0
                  put $ hState {  hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                               }
                  return $ MemoryOutput instrName imms [(VR,reg0,Right r0)] newTable Nothing
              --e.g. stdxMR  :: repr MR -> Int -> repr GPR -> repr GPR   -> repr MR
              ([(m0,MemoryRes _),(n0,RegisterRes GPR),(n1,RegisterRes GPR)],[(m1,MemoryRes _)]) -> do
                  df <- maybe (throwError $ FuncCastError "Interp MR -> Interp GPR -> Interp GPR -> Interp MR") pure $ (cast f :: Maybe (Interp MR -> Interp GPR -> Interp GPR -> Interp MR))
                  (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                  (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                  (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                  (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                  r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                  r1 <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                  let table0 = memLookup m0res (hardwareST_MRTables hState)
                      table1 = memLookup m1res (hardwareST_MRTables hState)
                      newTable = df table0 r0 r1
                  put $ hState {  hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                                }
                  return $ MemoryOutput instrName imms [(GPR,reg0,Left r0), (GPR,reg1,Left r1)] newTable Nothing
              --e.g. stvxMR  :: repr MR -> Int -> repr GPR -> repr VR   -> repr MR
              ([(m0,MemoryRes _),(n0,RegisterRes GPR),(n1,RegisterRes VR)],[(m1,MemoryRes _)]) -> do
                  df <- maybe (throwError $ FuncCastError "Interp MR -> Interp GPR -> Interp VR -> Interp MR") pure $ (cast f :: Maybe (Interp MR -> Interp GPR -> Interp VR -> Interp MR))
                  (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                  (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap -- TODO: This would be the same memory region as m0, do we need this?
                  (_, reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                  (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                  r0 <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                  r1 <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                  let table0 = memLookup m0res (hardwareST_MRTables hState)
                      table1 = memLookup m1res (hardwareST_MRTables hState)
                      newTable = df table0 r0 r1
                  put $ hState {  hardwareST_MRTables = [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                                 let tVal = if n == m0res then (n, newTable) else (n, t) ]
                               }
                  return $ MemoryOutput instrName imms [(GPR,reg0,Left r0), (VR,reg1,Right r1)] newTable Nothing
              -- NOTE moduloVLoad / moduloVStore
              ([(m0,MemoryRes _),(m1,MemoryRes _)],[(n1,RegisterRes GPR),(m2,MemoryRes _)]) -> do
                        df <- maybe (throwError $ FuncCastError "Interp MR -> Interp MR -> (Interp GPR, Interp MR)") pure
                              (cast f :: Maybe (Interp MR -> Interp MR -> (Interp GPR, Interp MR)))
                        (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                        (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap
                        (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                        (_,m2res) <- fromMapLookupToExcept "regMap" m2 regMap -- TODO: This would be the same memory region as m0, do we need this?
                        let table0 = memLookup m0res (hardwareST_MRTables hState)
                            table1 = memLookup m1res (hardwareST_MRTables hState)
                            (val, newTable) = df table0 table1
                            mrTables' = case List.lookup m2res (hardwareST_MRTables hState) of
                                        Just _ -> [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                let tVal = if n == m2res then (n, newTable) else (n, t) ]
                                        Nothing -> (hardwareST_MRTables hState) ++ [(m2res,newTable)]
                        put $ hState { hardwareST_GPRs = Map.insert reg1 val $ hardwareST_GPRs hState
                                , hardwareST_MRTables = mrTables'
                                }
                        return $ MemoryOutput instrName imms [] newTable (Just (Left val))

              ([(m0,MemoryRes _),(m1,MemoryRes _)],[(n1,RegisterRes VR),(m2,MemoryRes _)]) -> do
                        df <-
                                maybe (throwError $ FuncCastError "Interp MR -> Interp MR -> (Interp VR, Interp MR)") pure
                                (cast f :: Maybe (Interp MR -> Interp MR -> (Interp VR, Interp MR)))
                        (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                        (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap
                        (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                        (_,m2res) <- fromMapLookupToExcept "regMap" m2 regMap -- TODO: This would be the same memory region as m0, do we need this?
                        let table0 = memLookup m0res (hardwareST_MRTables hState)
                            table1 = memLookup m1res (hardwareST_MRTables hState)
                            (val, newTable) = df table0 table1
                            mrTables' = case List.lookup m2res (hardwareST_MRTables hState) of
                                        Just _ -> [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                let tVal = if n == m2res then (n, newTable) else (n, t) ]
                                        Nothing -> (hardwareST_MRTables hState) ++ [(m2res,newTable)]
                        put $ hState { hardwareST_VRs = Map.insert reg1 val $ hardwareST_VRs hState
                                , hardwareST_MRTables = mrTables'
                                }
                        return $ MemoryOutput instrName imms [] newTable (Just (Right val))

              ([(m0,MemoryRes _),(m1,MemoryRes _),(n1,RegisterRes GPR)],[(m2,MemoryRes _)]) -> do
                        df <-
                                maybe (throwError $ FuncCastError "Interp MR -> Interp MR -> Interp GPR -> Interp MR") pure
                                (cast f :: Maybe (Interp MR -> Interp MR -> Interp GPR -> Interp MR))
                        (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                        (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap
                        (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                        r1 <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                        (_,m2res) <- fromMapLookupToExcept "regMap" m2 regMap -- TODO: This would be the same memory region as m0, do we need this?
                        let table0 = memLookup m0res (hardwareST_MRTables hState)
                            table1 = memLookup m1res (hardwareST_MRTables hState)
                            newTable = df table0 table1 r1
                            mrTables' = case List.lookup m2res (hardwareST_MRTables hState) of
                                        Just _ -> [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                let tVal = if n == m2res then (n, newTable) else (n, t) ]
                                        Nothing -> (hardwareST_MRTables hState) ++ [(m2res,newTable)]
                        put $ hState {hardwareST_MRTables = mrTables'
                                }
                        return $ MemoryOutput instrName imms [(GPR,reg1,Left r1)] newTable Nothing

              ([(m0,MemoryRes _),(m1,MemoryRes _),(n1,RegisterRes VR)],[(m2,MemoryRes _)]) -> do
                        df <-
                                maybe (throwError $ FuncCastError "Interp MR -> Interp MR -> Interp VR -> Interp MR") pure
                                (cast f :: Maybe (Interp MR -> Interp MR -> Interp VR -> Interp MR))
                        (_,m0res) <- fromMapLookupToExcept "regMap" m0 regMap
                        (_,m1res) <- fromMapLookupToExcept "regMap" m1 regMap
                        (_, reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                        r1 <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_VRs hState
                        (_,m2res) <- fromMapLookupToExcept "regMap" m2 regMap -- TODO: This would be the same memory region as m0, do we need this?
                        let table0 = memLookup m0res (hardwareST_MRTables hState)
                            table1 = memLookup m1res (hardwareST_MRTables hState)
                            newTable = df table0 table1 r1
                            mrTables' = case List.lookup m2res (hardwareST_MRTables hState) of
                                        Just _ -> [ tVal | (n, t) <- hardwareST_MRTables hState,
                                                let tVal = if n == m2res then (n, newTable) else (n, t) ]
                                        Nothing -> (hardwareST_MRTables hState) ++ [(m2res,newTable)]
                        put $ hState {hardwareST_MRTables = mrTables'
                                }
                        return $ MemoryOutput instrName imms [(VR,reg1,Right r1)] newTable Nothing
              -- NOTE end moduloVLoad/moduloVStore
              ([], [(n0,RegisterRes VR)]) -> do
                val <- maybe (throwError $ FuncCastError "Interp VR") pure $ (cast f :: Maybe (Interp VR))
                (rt,reg) <- fromMapLookupToExcept "regMap" n0 regMap
                put $ hState { hardwareST_VRs = Map.insert reg val $ hardwareST_VRs hState }
                return $ DataInstrOutput instrName imms [] [] [(reg, val)]
              ([], [(n0,RegisterRes GPR)]) -> do
                val <- maybe (throwError $ FuncCastError "Interp GPR") pure $ (cast f :: Maybe (Interp GPR))
                (rt,reg) <- fromMapLookupToExcept "regMap" n0 regMap
                put $ hState { hardwareST_GPRs = Map.insert reg val $ hardwareST_GPRs hState }
                return $ DataInstrOutput instrName imms [] [(reg, val)] []
              ([(n0,RegisterRes VR)],[(n1,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR))) -- e.g. vnot
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   let val = df v0
                   put $ hState { hardwareST_VRs = Map.insert reg1 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0)] [] [(reg1, val)]
              ([(n0,RegisterRes VR)],[(n1,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp GPR)") pure $ (cast f :: Maybe (Interp VR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   let val = df v0
                   put $ hState { hardwareST_GPRs = Map.insert reg1 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0)] [(reg1, val)] []
              ([(n0,RegisterRes GPR)],[(n1,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp VR)") pure $ (cast f :: Maybe (Interp GPR -> Interp VR))) -- e.g. vnot
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   let val = df v0
                   put $ hState { hardwareST_VRs = Map.insert reg1 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0)] [] [(reg1, val)]
              ([(n0,RegisterRes GPR)],[(n1,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   let val = df v0
                   put $ hState { hardwareST_GPRs = Map.insert reg1 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0)] [(reg1, val)] []
              ([(n0,RegisterRes VR),(n1,RegisterRes VR)],[(n2,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_VRs = Map.insert reg2 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (VR,reg1,Right v1)] [] [(reg2, val)]
              ([(n0,RegisterRes VR),(n1,RegisterRes VR)],[(n2,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR -> Interp GPR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_GPRs = Map.insert reg2 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (VR,reg1,Right v1)] [(reg2, val)] []
              ([(n0,RegisterRes VR),(n1,RegisterRes GPR)],[(n2,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp GPR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp GPR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_VRs = Map.insert reg2 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (GPR,reg1,Left v1)] [] [(reg2, val)]
              ([(n0,RegisterRes GPR),(n1,RegisterRes VR)],[(n2,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp GPR -> Interp VR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_VRs = Map.insert reg2 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (VR,reg1,Right v1)] [] [(reg2, val)]
              ([(n0,RegisterRes VR),(n1,RegisterRes GPR)],[(n2,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp VR -> Interp GPR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_GPRs = Map.insert reg2 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (GPR,reg1,Left v1)] [(reg2, val)] []
              ([(n0,RegisterRes GPR),(n1,RegisterRes VR)],[(n2,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp VR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp VR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_GPRs = Map.insert reg2 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (VR,reg1,Right v1)] [(reg2, val)] []
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR)],[(n2,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp GPR))) -- e.g. srwg
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_GPRs = Map.insert reg2 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (GPR,reg1,Left v1)] [(reg2, val)] []
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR)],[(n2,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp VR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp VR))) -- e.g. vlvgp
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   let val = df v0 v1
                   put $ hState { hardwareST_VRs = Map.insert reg2 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (GPR,reg1,Left v1)] [] [(reg2, val)]
              -- TODO enumerate all input/output type combinations
              ([(n0,RegisterRes VR),(n1,RegisterRes VR),(n2,RegisterRes VR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR -> Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR -> Interp VR -> Interp VR))) -- e.g. selb
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (VR,reg1,Right v1), (VR,reg2,Right v2)] [] [(reg3, val)]
              ([(n0,RegisterRes GPR),(n1,RegisterRes VR),(n2,RegisterRes VR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp VR -> Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp GPR -> Interp VR -> Interp VR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (VR,reg1,Right v1), (VR,reg2,Right v2)] [] [(reg3, val)]
              ([(n0,RegisterRes VR),(n1,RegisterRes GPR),(n2,RegisterRes VR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp GPR -> Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp GPR -> Interp VR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (GPR,reg1,Left v1), (VR,reg2,Right v2)] [] [(reg3, val)]
              ([(n0,RegisterRes VR),(n1,RegisterRes VR),(n2,RegisterRes GPR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR -> Interp GPR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR -> Interp GPR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg2 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (VR,reg1,Right v1), (GPR,reg2,Left v2)] [] [(reg3, val)]
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR),(n2,RegisterRes VR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp VR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (GPR,reg1,Left v1), (VR,reg2,Right v2)] [] [(reg3, val)]
              ([(n0,RegisterRes VR),(n1,RegisterRes GPR),(n2,RegisterRes GPR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp GPR -> Interp GPR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp GPR -> Interp GPR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg2 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (GPR,reg1,Left v1), (GPR,reg2,Left v2)] [] [(reg3, val)]
              ([(n0,RegisterRes GPR),(n1,RegisterRes VR),(n2,RegisterRes GPR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp VR -> Interp GPR -> Interp VR)") pure $ (cast f :: Maybe (Interp GPR -> Interp VR -> Interp GPR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg2 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (VR,reg1,Right v1), (GPR,reg2,Left v2)] [] [(reg3, val)]
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR),(n2,RegisterRes GPR)],[(n3,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp GPR -> Interp VR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp GPR -> Interp VR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg2 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_VRs = Map.insert reg3 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (GPR,reg1,Left v1), (GPR,reg2,Left v2)] [] [(reg3, val)]
              ([(n0,RegisterRes VR),(n1,RegisterRes VR),(n2,RegisterRes VR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR -> Interp VR -> Interp GPR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR -> Interp VR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (VR,reg1,Right v1), (VR,reg2,Right v2)] [(reg3, val)] []
              ([(n0,RegisterRes GPR),(n1,RegisterRes VR),(n2,RegisterRes VR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp VR -> Interp VR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp VR -> Interp VR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (VR,reg1,Right v1), (VR,reg2,Right v2)] [(reg3, val)] []
              ([(n0,RegisterRes VR),(n1,RegisterRes GPR),(n2,RegisterRes VR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp GPR -> Interp VR -> Interp GPR)") pure $ (cast f :: Maybe (Interp VR -> Interp GPR -> Interp VR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (GPR,reg1,Left v1), (VR,reg2,Right v2)] [(reg3, val)] []
              ([(n0,RegisterRes VR),(n1,RegisterRes VR),(n2,RegisterRes GPR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR -> Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR -> Interp GPR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg2 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (VR,reg1,Right v1), (GPR,reg2,Left v2)] [(reg3, val)] []
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR),(n2,RegisterRes VR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp VR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp VR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (GPR,reg1,Left v1), (VR,reg2,Right v2)] [(reg3, val)] []
              ([(n0,RegisterRes VR),(n1,RegisterRes GPR),(n2,RegisterRes GPR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp GPR -> Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp VR -> Interp GPR -> Interp GPR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg2 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (GPR,reg1,Left v1), (GPR,reg2,Left v2)] [(reg3, val)] []
              ([(n0,RegisterRes GPR),(n1,RegisterRes VR),(n2,RegisterRes GPR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp VR -> Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp VR -> Interp GPR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg2 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (VR,reg1,Right v1), (GPR,reg2,Left v2)] [(reg3, val)] []
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR),(n2,RegisterRes GPR)],[(n3,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp GPR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   let val = df v0 v1 v2
                   put $ hState { hardwareST_GPRs = Map.insert reg3 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (GPR,reg1,Left v1), (GPR,reg2,Left v2)] [(reg3, val)] []
              ([(n0,RegisterRes VR),(n1,RegisterRes VR),(n2,RegisterRes VR),(n3,RegisterRes VR)],[(n4,RegisterRes VR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp VR -> Interp VR -> Interp VR -> Interp VR -> Interp VR)") pure $ (cast f :: Maybe (Interp VR -> Interp VR -> Interp VR -> Interp VR -> Interp VR))) -- e.g. fauxDep
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_VRs hState" reg0 $ hardwareST_VRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_VRs hState" reg1 $ hardwareST_VRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_VRs hState" reg2 $ hardwareST_VRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   v3   <- fromMapLookupToExcept "_VRs hState" reg3 $ hardwareST_VRs hState
                   (_,reg4) <- fromMapLookupToExcept "regMap" n4 regMap
                   let val = df v0 v1 v2 v3
                   put $ hState { hardwareST_VRs = Map.insert reg4 val $ hardwareST_VRs hState }
                   return $ DataInstrOutput instrName imms [(VR,reg0,Right v0), (VR,reg1,Right v1), (VR,reg2,Right v2), (VR,reg3,Right v3)] [] [(reg4, val)]
              ([(n0,RegisterRes GPR),(n1,RegisterRes GPR),(n2,RegisterRes GPR),(n3,RegisterRes GPR)],[(n4,RegisterRes GPR)]) ->
                do df <- (maybe (throwError $ FuncCastError "Interp GPR -> Interp GPR -> Interp GPR -> Interp GPR -> Interp GPR)") pure $ (cast f :: Maybe (Interp GPR -> Interp GPR -> Interp GPR -> Interp GPR -> Interp GPR))) -- e.g.
                   (_,reg0) <- fromMapLookupToExcept "regMap" n0 regMap
                   v0   <- fromMapLookupToExcept "_GPRs hState" reg0 $ hardwareST_GPRs hState
                   (_,reg1) <- fromMapLookupToExcept "regMap" n1 regMap
                   v1   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg2) <- fromMapLookupToExcept "regMap" n2 regMap
                   v2   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg3) <- fromMapLookupToExcept "regMap" n3 regMap
                   v3   <- fromMapLookupToExcept "_GPRs hState" reg1 $ hardwareST_GPRs hState
                   (_,reg4) <- fromMapLookupToExcept "regMap" n4 regMap
                   let val = df v0 v1 v2 v3
                   put $ hState { hardwareST_GPRs = Map.insert reg4 val $ hardwareST_GPRs hState }
                   return $ DataInstrOutput instrName imms [(GPR,reg0,Left v0), (GPR,reg1,Left v1), (GPR,reg2,Left v2), (GPR,reg3,Left v3)] [(reg4, val)] []
              -- ([(n0,RegisterRes GPR),(n1,RegisterRes GPR)], [(c0,RegisterRes CR)]) ->
              --  undefined
              _ -> throwError $ FuncNotFound $ "runInstruction failed on node " ++ show node -- error $ "Hardware.runInstruction failed on node: " ++ show node ++ " with typeref: " ++ show (typeOf f)
      nextNode = suc schedGraph node
    in do
      hState0 <- get
      dbg <- callUpdate hState0
      -- Print debug label from caching
      case Map.lookup unmappedNode (cgDebugLabels cg) of
        Nothing -> return ()
        Just lab -> liftIO $ print $ "Cache label: " ++ lab
      liftIO $ print dbg
      liftIO $ hPutStrLn handle $ unlines $ regToDbx dbg
      return nextNode

-- TODO: Where do we print outputs for relative jumps?
regToDbx :: SimOutput CORE -> [String]
regToDbx BranchOutput{..} = ["stepi"]
regToDbx DataInstrOutput{..} =
  [ "stepi"
  , "p \"$" <> printOutputReg modifiedGPRs modifiedVRs instr <> " =\""
  , "p $" <> printOutputReg modifiedGPRs modifiedVRs instr
  ]
regToDbx MemoryOutput{..} = ["stepi"]
regToDbx _ = []

-- Print utilities
-- TODO: Make a new instance to match dbx script
instance Show (SimOutput CORE) where
  show :: SimOutput CORE -> String
  show DataInstrOutput{..} =
    instr ++ "\t"
      ++ printOutputReg modifiedGPRs modifiedVRs instr
      ++ " " ++ unwords (map (\(reg,n,_) -> showRegType reg ++ BS.unpack n) args)
      ++ " " ++ unwords (map show imms) ++ " "
      ++ "\nargs: " ++ unlines (map (('\t' :) . printArgsGPROrVR) args)
      ++ "\n\tresult: " ++ concatMap printGPR modifiedGPRs ++ concatMap printVR modifiedVRs
  show BranchOutput{..} =
    branchInstrName ++ ": \n"
      ++ " " ++ show branchConditionOutput ++ " " ++ concatMap printGPR comparisonOps
  show MemoryOutput{..} =
    instr ++ ": \n"
      ++ "args: \n" ++ unlines (map (('\t' :) . printArgsGPROrVR) args)
      ++ "imms: " ++ unwords (map show imms)
      ++ "\nresult: " ++ printLoadOrStore mLoadOutput ++ " " ++ show newMemRegion
  show SpillOutput{..} =
    (if isDespill then "spill: \n" else "despill: \n")
     ++ "args: " ++ unlines (map (('\t' :) . printArgsGPROrVR) spillArgs)
     ++ "\nspill index: " ++ show spillIndex
     ++ "\nspill val: " ++ show spillVal
  show NoOp = "At CGNode"
  show (AddInitMR nd) =
    "Adding initMR at node " ++ show nd

showRegType :: RegType CORE -> String
showRegType GPR = "r"
showRegType VR  = "v"

printGPR :: (ByteString, Interp GPR) -> String
printGPR (reg, gpr) = "r" ++ BS.unpack reg ++ " (decimal: " ++ show (runInterpGPR gpr) ++ ",hex: 0x"
                      ++ showHex (runInterpGPR gpr) ""
                      ++ "double: " ++ show (word642Dbl (runInterpGPR gpr)) ++ ")"

printVR :: (ByteString, Interp VR) -> String
printVR (reg, vr) = "v" ++ BS.unpack reg ++ " (decimal: " ++ show (doubles vr) ++ ",hex: "
                   ++ case runInterpVR vr of
                        (p0,p1) -> "(0x" ++ showHex p0 ",0x" ++ showHex p1 "))"

printLoadOrStore :: Maybe (Either (Interp GPR) (Interp VR)) -> String
printLoadOrStore Nothing = "Store"
printLoadOrStore (Just (Left gpr)) = "Load GPR Word64:" ++ show (runInterpGPR gpr)
                                   ++ "  double: " ++ show (word642Dbl (runInterpGPR gpr))
printLoadOrStore (Just (Right vr)) = "Load VR: " ++ show (doubles vr)

printOutputReg :: [(ByteString, Interp GPR)] -> [(ByteString, Interp VR)] -> String -> String
printOutputReg gprs vrs instrName =
  if isOverwrite
    then ""
    else unwords $ map printRegGPR gprs ++ map printRegVR vrs
  where
    isOverwrite = instrName `elem`
      []

    printRegGPR (reg, _) = "r" ++ BS.unpack reg
    printRegVR  (reg, _) = "vr" ++ BS.unpack reg

printArgsGPROrVR :: (RegType CORE, ByteString, Either (Interp GPR) (Interp VR)) -> String
printArgsGPROrVR (GPR, reg, Left val) = printGPR (reg, val)
printArgsGPROrVR (VR, reg, Right val) = printVR (reg, val)
