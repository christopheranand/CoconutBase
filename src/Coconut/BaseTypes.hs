-- |
-- Module      :  Coconut.BaseTypes
-- Copyright   :  (c) OCA 2021
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  experimental
--
-- This module exports types Coconut uses, including types for DSL instances like @Interp@, @Graph@
-- and default register types likes @GPR@, @VR, @MR@

{-# LANGUAGE FlexibleInstances,ExistentialQuantification,GeneralizedNewtypeDeriving,FlexibleContexts,UndecidableInstances #-}
module Coconut.BaseTypes where

import GHC.TypeLits (Nat,KnownNat)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import Data.Typeable
import Data.Word (Word64)
import Data.Graph.Inductive.Graph (Context,Node)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans (liftIO)

import Coconut.Utils.MeetJoin

import System.IO

import Debug.Trace (trace)

debug = (flip $ trace)

-- | A CodeGraph is a @ControlFlowGraph@ and a collection of @DataFlowGraph@'s
data CodeGraph h = CodeGraph { -- | A @ControlFlowGraph@
                               cgControlFlow :: ControlFlowGraph h
                               -- | A list of @DataFlowGraph@ contained in the @ControlFlowGraph@
                             , cgDataFlows :: [(String, DataFlowGraph h)]
                               -- | Input for the @ControlFlowGraph@
                             , cgInput :: Node
                               -- | Output for the @ControlFlowGraph@
                             , cgOutput :: Node
                               -- | A list of memory regions used with their inital values
                             , cgMRTables :: [(String,Node,[Interp VR])]
                               -- | A list of constants used to populate the constant table
                             , cgConstTable :: [(Word64,Word64)]
                               -- | A map of tagged nodes
                             , cgTags :: Map String [Node]
                               -- | The maximum id used for any Node/Edge
                             , cgMaxID :: Int
                               -- | A map ocntaining debug labels of cached nodes
                             , cgDebugLabels :: Map Node String
                             }

-- NOTE DataFlowGraph (and by extension ControlFlowGraph which contains DataFlowGraph in CFEdge)
-- wraps type families in DFNode and thus breaks derive-ability, hence the need to manually supply
-- show instances
instance (Show (ControlFlowGraph h),Show (DataFlowGraph h)) => Show (CodeGraph h) where
  show cg =
    let
      cf = cgControlFlow cg
      df = cgDataFlows cg
      inp = cgInput cg
      out = cgOutput cg
    in "CodeGraph { cgControlFlow = " ++ show cf
       ++ "\n , cgDataFlows = " ++ show df
       ++ "\n , cgInput = " ++ show inp
       ++ "\n , cgOutput = " ++ show out
       -- ++ "\n , cgMRTables = " ++ show tables
       -- ++ "\n , cgConstTable = " ++ show cTable
       -- ++ "\n , cgTags = " ++ show tags
       ++ "\n }"

-- | A ControlFlow arrangement is a graph with a list of resources as nodes and
--   @DataFlow@ graphs as edges (as well as some special edges for branches, loops, etc)
data ControlFlowGraph h = ControlFlowGraph { controlFlowGraph :: Gr CFNode (CFEdge h)
                                           , controlFlowInput :: Maybe Node
                                           , controlFlowOutput :: Maybe Node
                                           }

instance Show (ControlFlowGraph h) where
  show (ControlFlowGraph cfGraph inp out) =
    "ControlFlowGraph { controlFlowGraph = " ++ show cfGraph
    ++ "\n , controlFlowInput = " ++ show inp
    ++ "\n , controlFlowOutput = " ++ show out
    ++ "\n }"

data CFNode = CFNode [(String,Node)]
            | CFBranchNode (String,Node) [(String,Node)]
            | CFJoinNode [(Node,Node)]
            -- CFReturn
 deriving (Eq,Show)

-- FIXME should remove DataFlowGraph from CFDataFlow and CFModuloBody and force user to lookup
-- in cgDataFlows using label, because often cgDataFlows gets updated but the corresponding CFEdge does not
data CFEdge h = CFDataFlow String
              | CFModuloBody String
              | CFCompose (String,String) [(Node,Node)] -- ^ compose one block with another, contains ties
              | CFBranchNE String [(Node,Node)] -- ^ branch on NE, contains label and ties
              | CFBranchEQ String [(Node,Node)] -- ^ branch on EQ, contains label and ties
              | CFJump (String,String) [(Node,Node)] -- ^ unconditional jump FIXME is this just compose?


instance Show (CFEdge h) where
  show (CFDataFlow label) = "CFDataFlow " ++ label
  show (CFModuloBody label) = "CFModuloBody " ++ show label
  show (CFCompose label nodes) = "CFCompose " ++ show label ++ " " ++ show nodes
  show (CFBranchEQ label nodes) = "CFBranchEQ " ++ show label ++ " " ++ show nodes
  show (CFBranchNE label nodes) = "CFBranchNE" ++ show label ++ " " ++ show nodes
  show (CFJump label nodes) = "CFJump " ++ show label ++ " " ++ show nodes

-- | A DataFlow graph is a bipartite graph between data nodes (resources like registers,
--   memory regions, etc) and instructions that act on them
data DataFlowGraph h = DataFlowGraph { -- | the underlying FGL graph structure
                                       dataFlowGraph :: Gr (DFNode h) DFEdge
                                       -- | inputs nodes (all @ResourceNode@'s) with tags
                                     , dataFlowInputs :: [(String,Node)]
                                       -- | output nodes (all @ResourceNode@'s) with tags
                                     , dataFlowOutputs :: [(String,Node)]
                                       -- | instructions (first Node) with input/output pairs that are overwritten
                                     , dataFlowOverwrites :: [(Node,(Node,Node))]
                                       -- | increment used if node was inserted by cgbInsertDataFlow
                                     , dataFlowIncrement :: Int
                                       -- | the stage used for modulo scheduling
                                     , dataFlowStage :: Int
                                     }

instance (Show (DFNode h)) => Show (DataFlowGraph h) where
  show (DataFlowGraph dfGraph ins outs overwrites increment stage) =
    "DataFlowGraph { dataFlowGraph = " ++ show dfGraph
    ++ "\n , dataFlowInputs = "  ++ show ins
    ++ "\n , dataFlowOutputs = " ++ show outs
    ++ "\n , dataFlowOverwrites = " ++ show overwrites
    ++ "\n , dataFlowIncrement = " ++ show increment
    ++ "\n , dataFlowStage = " ++ show stage
    ++ "\n }"

-- | The @Context@ (i.e., of an fgl @Graph@) used in a @DataFlowGraph@
type DataFlowContext h = Context (DFNode h) DFEdge

-- | Used to label edges in a @DataFlow@ graph, contains a single element @dfEdgeIndex@
--   that specifies each edges index (this allows edges to have a specified "ordering")
data DFEdge = DFEdge { dfEdgeIndex :: Int }
  deriving (Show,Eq,Ord)

-- | The in-construction representation of nodes, which maps a hash containing
--   input nodes and instruction to its node label and inputs,
--   node identifier and output nodes
type CGNodeMap h = IntMap [((DFNode h, [Node]), Node, [Node])]

-- | Nodes for @DataFlow@ graphs are either a @Resource@ or @Instruction@ (since @DataFlow@ is a bipartite
--   graph).
--   * @Resource@ wraps @ResType@, which is indexed by a type variable h to support different
--     register types (i.e., via RegType) for different instances of @Hardware@
--   * @Instruction@ wraps @EL@ which is also indexed by a type variable h to support
--     interpretation of instructions for different instances of @Hardware@
data DFNode h = ResourceNode (ResType h)
              | InstructionNode (EL h) -- FIXME change name of EL?
              | forall fn . Typeable fn =>
                         BranchNode { instructionName :: String
                                    , bnImmediates :: [Int]
                                    , bnInterpFn :: fn }

instance (Show (ResType h),Show (EL h)) => Show (DFNode h) where
  show (ResourceNode res) = "ResourceNode " ++ show res
  show (InstructionNode instr) = "InstructionNode " ++ show instr
  show (BranchNode instr imm _) = "BranchNode " ++ show instr ++ " " ++ show imm

-- |Type used to index type wrappers (i.e., Graph, Interp) that are instances of
-- @CoreISA@ (and classes that inherit it)
-- Should be used to specify standard general purpose registers (GPR's)
data GPR

-- |Type used to index type wrappers (i.e., Graph, Interp) that are instances of
-- @CoreISA@ (and classes that inherit it)
-- Should be used to specify standard 128-bit vector registers (VR's)
data VR

-- |Type used to index type wrappers (i.e., Graph, Interp) that are instances of
-- @CoreISA@ (and classes that inherit it)
-- Should be used to specify standard Floating Point registers (FPR's)
data FPR

-- |Type used to index type wrappers (i.e., Graph, Interp) that are instances of
-- @CoreISA@ (and classes that inherit it)
-- Should be used to specify conditional registers (CR's)
data CR

-- |Type used to index type wrappers (i.e., Graph, Interp) that are instances of
-- @CoreISA@ (and classes that inherit it)
-- Should be used to specify (virtual) memory regions (where the size is constrained by Nat)
data MR

-- |Type used to index type wrappers (i.e., Graph, Interp) that are instances of
-- @CoreISA@ (and classes that inherit it)
-- Should be used to specify a specific memory region used for spilling
data SPILL

-- |Type used to index type wrappers (i.e., Graph, Interp) that are instances of
-- @CoreISA@ (and classes that inherit it)
-- Should be used to specify a branch instruction
data BRANCH

{- |Determines the partiation size of vector instructions, which can be one of the following values.
    Different instructions have different maximum sizes and different reserved numbers, as noted in
    the documentation.

     * 0 -> byte (8 bits)
     * 1 -> halfword (16 bits)
     * 2 -> word (32 bits)
     * 3 -> doubleword (64 bits)
     * 4 -> quadword (128 bits)
     * 5-15 -> reserved
-}
type Size = Integer -- TODO use an enum for this?

-- |Type family used to create an instance of @CoreISA@ (and classes that inherit it)
-- meant to provide Haskell based interpretation of the DSL. This type family must provide
-- instance for all types used in the @CoreISA@ typeclass (i.e., @GPR@, @VR@, etc)
data family Interp a

-- |Interpret a GPR as a Word64 (needed for @CoreISA@ Interp instance)
newtype instance Interp GPR = InterpGPR { runInterpGPR :: Word64 }
  deriving (Show,Eq,Typeable)

-- |Interpret a CR as a Word64 (needed for @CoreISA@ Interp instance)
newtype instance Interp CR = InterpCR { runInterpCR :: Word64 }
  deriving (Show,Eq,Typeable)

-- |Interpret a FPR as a Double (needed for @CoreISA@ Interp instance)
newtype instance Interp FPR = InterpFPR { runInterpFPR :: Double }
  deriving (Show,Eq,Typeable)

-- |Interpret a VR as a pair of Word64 (needed for @CoreISA@ Interp instance)
newtype instance Interp VR = InterpVR { runInterpVR :: (Word64,Word64) }
  deriving (Show,Eq,Typeable)

-- |Interpret an MR as a @ZInterpMemRegion@
-- TODO: Move the (s :: Nat) to the data level
newtype instance Interp MR = InterpMR { runInterpMR :: InterpMemRegion }
  deriving (Eq,Typeable)

-- |Interpret an MR as a @ZInterpMemRegion@
-- NOTE: we should possibly have two seperate instances for spilling VRs and GPRs
-- however, for now we store the GPR in the fst of the tuple of (Word64,Word64)
newtype instance Interp SPILL = InterpSPILL { runInterpSPILL :: (Word64,Word64) }
  deriving (Eq,Typeable,Show)

instance Show (Interp MR) where
  show (InterpMR (PIMR name current _ _ size)) = "Interp MR: " ++ name ++ " size: " ++ show size

-- |Used to provide a virtual memory region for interpretation
data InterpMemRegion = PIMR  {name :: String -- ^ provides a name
                              ,current :: BC.ByteString -- ^ TODO document
                              ,atLast :: BC.ByteString
                              ,opsSince :: [LS]
                              ,size :: Integer
                              }
    deriving (Eq)

-- |Used to keep Load / Store meta-data used by InterpMemRegion
data LS  = Load (Int,Int)
         | Store (Int,Int) BC.ByteString
   deriving (Eq,Show)

-- |TODO document RunTimeSized
data RunTimeSized = RTS  {rname :: String
                         ,rcurrent :: BC.ByteString
                         ,ratLast :: BC.ByteString
                         ,ropsSince :: [LS]
                         ,rsize :: Int
                         }
      deriving (Show)
-- |Interpret a BRANCH as a boolean condition on whether of not to branch
newtype instance Interp BRANCH = InterpBRANCH { runInterpBRANCH :: Bool }

-- |A map of meta data information indexed by instructions name (i.e., @elName@ in @EL@)
newtype MDMap h = MDMap { mdMap :: Map String (MetaData h) }

-- | A ScheduledGraph is a Control Flow Graph that only specifies control flow
-- not between asset dependency, but simply between scheduled instructions,
-- i.e., and edge between two instrcutions specifies ones instruction comes
-- after the other in a schedule, and nothing to do with which registers it
-- depends on
-- @ScheduledGraph@ is a type alias for @Gr (ScheduledNode h) ()@
type ScheduledGraph h = Gr (ScheduledNode h) ScheduledEdge

-- | A ScheduledNode is an instruction node (i.e. @snInstructionNode@) with the
-- registers it uses (@snUses@) and registers it defines (@snDefs@)
data ScheduledNode h = ScheduledNode { snInstructionNode :: Node
                                     , snUses :: [(Node,ResType h)]
                                     , snDefs :: [(Node,ResType h)]
                                     , snUnmapped :: Node
                                     , snGlobalConst :: Bool
                                     , snRegPressure :: Maybe [(String,Int)]
                                     }
-- | A @ScheduledEdge@ is the labelled edge (i.e, @LEdge@) of a @ScheduledGraph@ used to specify
-- control flow, it's an enum type with the following value constructors:
--         * a standard compose @SECompose@
--         * an "equal" branch @SEBranchEQ@
--         * a "not-equal" branch @SEBranchNE@
data ScheduledEdge = SECompose
                   | SEBranchEQ
                   | SEBranchNE
  deriving (Show,Eq)

-- TODO move Printer to a seperate module?
class Hardware h => Printer h where
  data InstructionFormat h
  printableInstrLabel  :: MetaData h -> ByteString
  printableInstrFormat :: MetaData h -> [Int] -> InstructionFormat h -- TODO: how do we differentiate between imm and mask values?
  --printableInstruction :: ByteString -> [(RegType h,ByteString)] -> ByteString
  printableInstruction :: MDMap h -> Maybe (InstructionFormat h) -> ByteString -> [(RegType h,ByteString)] -> [Int] -> Maybe String -> [(Word64,Word64)] -> ByteString
  -- TODO printableTable doesnt really need codegraph, but we need to make it type specific
  printableTable :: CodeGraph h -> String -> [(Word64, Word64)] -> [ByteString]
  printableSectionLabel :: CodeGraph h -> String -> ByteString

-- TODO move Hardware to seperate module?
-- |A typeclass that implements different hardware functionality, allowing for extensible register types
--  and simulation
class (Show (RegType h),Eq (RegType h), Show (HardwareST h)) => Hardware h where
  data RegType h      -- use type family to enumerate different register types at the data level
  data HardwareST h   -- use type family to encode architecture information, thats needed as state for simulation
  data MetaData h     -- use type family to encode different meta data about instructions
  hardwareLatency :: MetaData h -> Int
  memTables :: HardwareST h -> [(ByteString,Interp MR)]
  initHardwareST :: (RegMap h,SpillMap) -> [(String, ByteString, [Interp VR])] -> ([(ByteString, Interp GPR)], [(ByteString, Interp VR)]) -> HardwareST h
  getBranchCond  :: HardwareST h -> Bool
  allRegTypes    :: [RegType h]
  memRegType     :: RegType h
  constantReg    :: (RegType h,ByteString)
  isGPR          :: RegType h -> Bool
  isHStore       :: DFNode h -> Bool
  isHLoad        :: DFNode h -> Bool
  isIncMR        :: DFNode h -> Bool
  isModuloLoad   :: DFNode h -> Bool
  isModuloStore  :: DFNode h -> Bool
  isConstantLoad :: DataFlowGraph h -> Node -> Bool
  regColors      :: RegType h -> [ByteString]
  typeToReg      :: TypeRep -> RegType h -- take Type Representation (provided curtosy of Typeable library) and turn them into concrete type
  regPrefix      :: RegType h -> ByteString -- return the register prefix (like v for VR or r for GPR) used in the target assembly language
  spillName      :: RegType h -> String
  despillName    :: RegType h -> String
  moveName       :: RegType h -> String
  runInstruction :: Handle -> ScheduledGraph h -> CodeGraph h -> (RegMap h,SpillMap) -> Node -> SimState h [Node] -- perform simulation, knowing what register types are available
  -- FIXME changed from runInstruction :: Schedule h -> RegMap h -> Edge -> SimState h (Maybe Edge) -- perform simulation, knowing what register types are available
  -- Need a more typesafe way to represent Edge's? (Right now they are just fgl Node's which are just Int's)

-- |A schedule is a map from an @Instruction@ node in a @CodeGraph@ to their dispatch time
--  Type alias for @Map Node DispatchTime@ (i.e., @Map Int Int@)
type Schedule = Map Node DispatchTime
-- FIXME changed from type Schedule h = Map Edge DispatchTime
-- Need a more typesafe way to represent Edge's? (Right now they are just fgl Node's which are just Int's)

-- |A type synonym for Int used to represent dispatch times for instructions (i.e., @CodeGraph@ @Edge@)
type DispatchTime = Double

-- |A Map from nodes (indexed via @NodeName@) in a @CodeGraph@ to a register type and label for printing.
--  The label is a @ByteString@ without its prefix, i.e., instead of "r0", just "0", use @regPrefix@
--  to retrieve the prefix
type RegMap h = Map Node (RegType h,ByteString)
-- FIXME changed from type RegMap h = Map NodeName (RegType h,ByteString)
-- Need a more typesafe way to represent register Nodes? (Right now they are just fgl Node's which are just Int's)

-- | A Map from nodes (that are @RegisterRes@ of type @SpillRes@) to offsets
-- into spill memory
-- NOTE currently offset by the register type with the maximum number of bytes (i.e. VR),
-- so an offset of 2 is 2 * 16 bytes of offset (i.e., 16 is number of bytes per VR)
type SpillMap = Map Node Int

-- | We currently use a fixed size (in number of Word64's) for the spill space
-- NOTE the FIFO space will be double this size
spillSpaceSize :: Int
spillSpaceSize = 32 * 2 -- support 32 VRs

-- |Custom type for identifying exceptions that may occur during simulation. Used inside of the @ExceptT@
--  monad transformer in the @SimState@ monad transfomer stack
data family SimError h

-- |Custom type for supporting output information during simulation
data family SimOutput h

-- TODO add me to an appropriate module
-- data instance SimError CORE = LookupError String
--                             | OtherError String
--   deriving Show

-- TODO move me to Simulation module?
-- |Monad transformer stack used for simulation. The stack is: Hardware State -> Exceptions -> IO.
-- The Hardware State is handled by @StateT@ monad that keeps track of a @HardwareST@ data structure,
-- which is a type family associated with the @Hardware@ type class. The exceptions are handled by
-- an @ExceptT@ monad that uses another type family @SimError@. The implementation of both @HardwareST@
-- and @SimError@ are specified by the first type parameter @h@
newtype SimState h a = SimState { runSimState :: StateT (HardwareST h) (ExceptT (SimError h) IO) a }
  deriving (Monad,Functor,Applicative,MonadState (HardwareST h),MonadError (SimError h),MonadIO)


-- | The EL type serves as a label for an @Edge@ in a @CodeGraph@
data EL h = forall fn. Typeable fn => Instruction { elImmediates  :: [Int]
                                                  , elName        :: String     -- name of the instruction
                                                  , elInterpFn    :: fn         -- interp instruction
                                                  }
          | Spill { elName :: String }
          | Despill { elName :: String }
          -- | ScratchLoad { elImmediates :: [Int], elName :: String }
          -- | ScratchStore { elImmediates :: [Int], elName :: String }
          | Move { elName :: String, elIsOverwrite :: Bool } -- ^ placeholder for a generic move (i.e. register copy instruction), isOverwrite specifies if its necessary to prevent an overwritten node
          | InitMR String [Word64] -- ^ a virtual memory region, uniquely identified by its connected Node
          | Meet -- ^ where multiple memory accesses meet
          | Join -- ^ where two seperate/equivalent code paths end

instance Eq (EL h) where
  Meet      == Meet      = True
  _         == Meet      = False
  Meet      == _         = False
  Join      == Join      = True
  _         == Join      = False
  Join      == _         = False
  -- Compose   == Compose   = True
  -- _         == Compose   = False
  -- Compose   == _         = False
  -- Decompose == Decompose = True
  -- _         == Decompose = False
  -- Decompose == _         = False
  -- this covers all the Instr* constuctors
  _         == _         = False

instance Ord (EL h) where
  compare Meet       Meet      = EQ
  compare _          Meet      = GT
  compare Meet       _         = LT
  compare Join       Join      = EQ
  compare _          Join      = GT
  compare Join       _         = LT
  -- compare Compose    Compose   = EQ
  -- compare _          Compose   = GT
  -- compare Compose    _         = LT
  -- compare Decompose  Decompose = EQ
  -- compare _          Decompose = GT
  -- compare Decompose  _         = LT
  -- this covers all the Instr* constuctors
  compare _          _         = EQ

instance Show (EL h) where
  show Meet = "Meet"
  show Join = "Join"
  show (InitMR label wrds) = "InitMR " ++ label -- ++ " " ++ show wrds
  show (Instruction _ name _) = "Instruction " ++ name
  show (Move name isOverwrite) = "Move " ++ name ++ " isOverwrite: " ++ show isOverwrite
  show (Spill name) = "Spill " ++ name
  show (Despill name) = "Despill " ++ name
  -- show (ScratchLoad imms name) = "ScratchLoad " ++ name
  -- show (ScratchStore imms name) = "ScratchStore " ++ name
  -- show Compose = "Compose"
  -- show Decompose = "Decompose"


-- |Resource type attached to each node of a @CodeGraph@
data ResType h = RegisterRes (RegType h) -- ^ register type indexed by a @Hardware@ instance
               | MemoryRes Int           -- ^ a memory region
               | SpillRes                -- ^ a SINGLE SLOT in a special memory region for spilling
               | EmptyRes                -- ^ An empty result type
               -- | RList (ResType h) -- TODO figure out how to use this to map / fold CodeGraphs
               -- TODO: probably better wrap rest of type in Maybe
               -- deriving (Eq,Ord,Show,Read)

-- TODO Add Control Flow Graph
-- data ControlFlowGraph h = Map [NodeName] (CodeGraph NodeName (ResType h) (EL h))

instance Eq (RegType h) => Eq (ResType h) where
  RegisterRes r0 == RegisterRes r1 = r0 == r1
  MemoryRes m0   == MemoryRes m1   = m0 == m1 -- NOTE all mem regions are equal iff their sizes are
  SpillRes       == SpillRes       = True -- FIXME should all SpillRes be considered equal?
  EmptyRes       == EmptyRes       = True
  _              == _              = False

-- TODO Show (RegType h) only exists because createCG (form CoconutHyperGraph library) wants it
-- this is a little annoying, as RegType is a type family which mean deriving show must be done
-- on each type instance and we need to add a variety of constraints on functions that wouldn't otherwise
-- need them
instance Show (RegType h) => Show (ResType h) where
  showsPrec _ r = (++) (case r of
    RegisterRes r -> show r
    MemoryRes m -> "MemoryRes " <> show m
    SpillRes -> "SpillRes"
    EmptyRes -> "()"
    )

-- TODO why does this exist?
-- instance Ord h => Ord (ResType h) where
--   compare (RegisterRes r0) (RegisterRes r1) = compare r0 r1
--   compare EmptyRes EmptyRes = EQ

class Cacheable repr where
  cache :: ByteString -> repr a -> repr a
  fCache :: ByteString -> [repr a] -> repr a -> repr a
  -- clearCache :: repr a -> repr a


qFCache :: Cacheable repr => ByteString
  -> (repr a -> repr a)
  -> (repr a, repr a, repr a, repr a)
  -> (repr a, repr a, repr a, repr a)
qFCache fName func (x0,x1,x2,x3) = (fCache fName [x0] $ func x0
                                   ,fCache fName [x1] $ func x1
                                   ,fCache fName [x2] $ func x2
                                   ,fCache fName [x3] $ func x3
                                   )

qCache (s0,s1,s2,s3) (a0,a1,a2,a3) = (cache s0 a0,cache s1 a1,cache s2 a2,cache s3 a3)
instance Cacheable Interp where
  cache _ = id
  fCache _ _ = id
  -- clearCache = id

