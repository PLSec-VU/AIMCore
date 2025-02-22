module Leak where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.Tuple (swap)
import GHC.TypeNats
import Instruction hiding (Jump, decode, halt)
import qualified Instruction
import Pipe
import Regfile
import Simulate (readWord)
import Types
import Prelude hiding (Ordering (..), Word, const, init, not, undefined, (!!), (&&), (||))
import qualified Prelude

-- `fromJust` is safe here because the `fetch` stage unconditionally always reads
-- from memory (and its read may just be superseded by the `memory` stage).
obs :: Output -> Address
obs = memAddress . fromJust . getFirst . outMem

-- data LeakInst
--  = RegStore RegIdx (Word -> Word -> Word)
--  | Jump (Address -> Address)
--  | Nop

-- data Hazard a
--   = WithHazard RegIdx a
--   | NoHazard

data WithReg a
  = Constant a
  | Unary RegIdx (Word -> a)
  | Binary RegIdx RegIdx (Word -> Word -> a)

data JumpType
  = Offset
  | Absolute

data LeakInst
  = RegStore RegIdx (WithReg Word)
  | MemLoad RegIdx (WithReg Address)
  | MemStore Address
  | Jump JumpType Address
  | Nop

-- data Pipeline =
--  Pipeline {
--    pipeDec :: Maybe Instruction,
--    pipeEx :: Maybe Instruction,
--    pipeMem :: Maybe Instruction,
--    pipeWb :: Maybe Instruction
--           }

data LeakOut = LeakOut
  { leakOutInst :: First LeakInst,
    leakOutRs1 :: First Word,
    leakOutRs2 :: First Word
  }

instance Semigroup LeakOut where
  LeakOut i r1 r2 <> LeakOut i' r1' r2' = LeakOut (i <> i') (r1 <> r1') (r2 <> r2')

instance Monoid LeakOut where
  mempty = LeakOut mempty mempty mempty

data SimIn = SimIn
  { simInInst :: LeakInst,
    simInRs1 :: Word,
    simInRs2 :: Word
  }

type LeakM = RWS () LeakOut LeakState

data LeakState = LeakState
  { leakBubble :: Bool,
    leakLastInst :: Instruction
  }
  deriving (Show, Eq)

initLeak :: LeakState
initLeak =
  LeakState
    { leakBubble = False
    }

-- fetch:
-- read from memory  (outputs read loc)
--
-- decode:
-- read from the register file  (outputs reg locs)
--
-- execute:
-- no input/output
--
-- memory:
-- read/write to memory
--
-- writeback:
-- reads from intput (return from memory)
-- writes to register file

-- cpu only has outputs in the `memory` and `writeback` stages?

-- binary :: RegIdx -> (Word -> Word -> Word) -> LeakInst
-- binary = RegStore
--
-- unary :: RegIdx -> (Word -> Word) -> LeakInst
-- unary idx = binary idx . Prelude.const
--
-- const :: RegIdx -> Word -> LeakInst
-- const idx = unary idx . Prelude.const

instOut :: LeakInst -> LeakM ()
instOut inst =
  tell $ mempty {leakOutInst = pure inst}

leak :: Input -> LeakM ()
leak (Input mem rs1 rs2) = do
  case Instruction.decode mem of
    RType op rd r1 r2 ->
      tell $
        mempty {leakOutInst = pure $ RegStore rd $ Binary r1 r2 $ alu op}
    IType iop rd r1 imm -> do
      let op =
            case iop of
              Arith op' -> op'
              _ -> ADD
          comp_res rs1 = alu op rs1 $ signExtend imm
      case iop of
        Arith {} ->
          tell $
            mempty
              { leakOutInst = pure $ RegStore rd $ Unary r1 comp_res
              }
        Load size sign -> do
          let loadExtend rs1 = \case
                (Byte, Signed) -> signExtend $ slice d7 d0 $ comp_res rs1
                (Byte, Unsigned) -> zeroExtend $ slice d7 d0 $ comp_res rs1
                (Half, Signed) -> signExtend $ slice d15 d0 $ comp_res rs1
                (Half, Unsigned) -> signExtend $ slice d15 d0 $ comp_res rs1
                (Word, _) -> signExtend $ slice d31 d0 $ comp_res rs1
              comp_val rs1 = loadExtend rs1 (size, sign)
          tell $
            mempty
              { leakOutInst =
                  pure $
                    MemLoad rd $
                      Unary r1 $
                        bitCoerce . alu ADD (signExtend imm) . comp_val
              }
    JType rd imm ->
      tell $
        mempty {leakOutInst = pure $ Jump Offset (bitCoerce $ signExtend imm)}

-- JType rd imm -> do
--  writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--  LJ . bitCoerce
--    <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))

-- s <- get
-- case leakLastInst s of
--  RType op rd _ _ ->
--    instOut $ RegWrite rd $ alu op rs1 rs2

-- modify $ \s -> s {leakLastInst = Instruction.decode mem}

-- IType iop rd r1 imm -> do
--  let op =
--        case iop of
--          Arith op' -> op'
--          _ -> ADD
--  res <- alu op rs1 $ pure (signExtend imm)
--  case iop of
--    Arith {} -> pure Nop
--    Load size sign -> do
--      let loadExtend = \case
--            (Byte, Signed) -> signExtend $ slice d7 d0 res
--            (Byte, Unsigned) -> zeroExtend $ slice d7 d0 res
--            (Half, Signed) -> signExtend $ slice d15 d0 res
--            (Half, Unsigned) -> signExtend $ slice d15 d0 res
--            (Word, _) -> signExtend $ slice d31 d0 res
--          val = loadExtend (size, sign)
--      writeRF rd =<< readRAM (unpack val)
--      pure Nop
--    Jump -> do
--      writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--      pure $ LJ $ bitCoerce res
--    Env {} -> error ""
-- SType size imm r1 r2 -> do
--  addr <- unpack <$> (alu ADD <$> readRF r1 <*> pure (signExtend imm))
--  val <- readRF r2
--  writeRAM size addr val
--  pure Nop
-- BType cmp imm r1 r2 -> do
--  branched <- branch cmp <$> readRF r1 <*> readRF r2
--  if branched
--    then
--      LJ . bitCoerce
--        <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
--    else pure Nop
-- UType Zero rd imm -> do
--  let imm' = imm ++# 0 `shiftL` 12
--  writeRF rd $ imm'
--  pure Nop
-- UType PC rd imm -> do
--  let imm' = imm ++# 0 `shiftL` 12
--  writeRF rd imm'
--  pure Nop
-- JType rd imm -> do
--  writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--  LJ . bitCoerce
--    <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))

-- leakRun :: (KnownNat n) => LeakState n -> Word -> (LeakState n, LeakInst)
-- leakRun s i = swap $ runState (leak i) s

-- leakStep :: (KnownNat n) => (LeakState n, SimState) -> Word -> ((LeakState n, SimState), Address)
-- leakStep (ls, ss) i = ((ls', ss'), pc)
--  where
--    (ls', inst) = leakRun ls i
--    (ss', pc) = simRun ss inst
--
-- simOn :: Int -> Vec n Word -> [Address]
-- simOn n prog = simOn' n initState
--  where
--    simOn' 0 _ = mempty
--    simOn' _ _ = mempty
--    simOn' n s =
--      let (s', pc) = leakStep s
--       in pc : simOn' (n - 1) s'
--    initState = (initLeak prog, initSim)
--
type SimM = RWS SimIn (First Address) SimState

data SimState = SimState
  { simPc :: Address,
    simDeInstr :: LeakInst,
    simExInstr :: LeakInst,
    simExRes :: Word,
    simMemInstr :: LeakInst,
    simMemRes :: Word,
    simWbInstr :: LeakInst,
    simBubble :: Bool
  }

withReg :: WithReg a -> SimM a
withReg (Constant a) = pure a
withReg (Unary _ f) = f <$> asks simInRs1
withReg (Binary _ _ f) = f <$> asks simInRs1 <*> asks simInRs2

initSim :: SimState
initSim =
  SimState
    { simPc = 4 * 50,
      simDeInstr = Nop,
      simExInstr = Nop,
      simExRes = 0,
      simMemInstr = Nop,
      simMemRes = 0,
      simWbInstr = Nop,
      simBubble = False
    }

simFetch :: SimM ()
simFetch = do
  s <- get
  unless (simBubble s) $ do
    instr <- asks simInInst
    let instr' =
          case instr of
            Jump Offset offset -> Jump Absolute $ simPc s + offset
            _ -> instr
    modify $ \s ->
      s
        { simDeInstr = instr',
          simPc = simPc s + 4
        }
  tell $ pure $ simPc s

simDecode :: SimM ()
simDecode = do
  s <- get
  modify $ \s ->
    s
      { simExInstr =
          if simBubble s
            then Nop
            else simDeInstr s
      }

simExecute :: SimM ()
simExecute = do
  s <- get
  case simExInstr s of
    RegStore rd f ->
      withReg f >> pure ()
    Jump Absolute addr ->
      modify $ \s ->
        s
          { simPc = addr,
            simBubble = True
          }
  modify $ \s -> s {simMemInstr = simExInstr s}

simMemory :: SimM ()
simMemory = do
  instr <- gets simMemInstr
  case instr of
    _ -> pure ()

  modify $ \s -> s {simWbInstr = simExInstr s}

simWriteback :: SimM ()
simWriteback = do
  instr <- gets simWbInstr
  case instr of
    RegStore rd val -> pure ()
    MemStore addr -> tell $ pure addr
    -- MemLoad _ (Right addr) -> tell $ pure addr
    _ -> pure ()

-- case simWbInstr of
--  RegStore rd f -> undefined

simTick :: LeakInst -> SimM Address
simTick inst =
  fmap (fromMaybe 0 . getFirst . snd) $
    listen $ do
      simWriteback
      simMemory
      simExecute
      simFetch

--
-- simRun :: SimState -> LeakInst -> (SimState, Address)
-- simRun s i = swap $ runState (simTick i) s

-- readRF :: RegIdx -> LeakM n Word
-- readRF idx = gets $ lookupRF idx . leakRF
--
-- writeRF :: RegIdx -> Word -> LeakM n ()
-- writeRF idx val =
--  modify $ \s -> s {leakRF = modifyRF idx val $ leakRF s}
--
---- TODO: Fix the code duplication with Simulator.hs
-- writeRAM :: (KnownNat n) => Size -> Address -> Word -> LeakM n ()
-- writeRAM size addr w =
--  modify $ \s -> s {leakRAM = write size addr w $ leakRAM s}
--  where
--    write size addr w mem =
--      let b0 = slice d7 d0 w
--          b1 = slice d15 d8 w
--          b2 = slice d23 d16 w
--          b3 = slice d31 d24 w
--          writeByte =
--            replace addr b0
--          writeHalf =
--            replace (addr + 1) b1 . writeByte
--          writeWord =
--            replace (addr + 3) b3
--              . replace (addr + 2) b2
--              . writeHalf
--       in case size of
--            Byte -> writeByte mem
--            Half -> writeHalf mem
--            Word -> writeWord mem
--
-- readRAM :: (KnownNat n) => Address -> LeakM n Word
-- readRAM addr = gets $ readWord addr . leakRAM
--  where
--    readWord addr mem =
--      (mem !! (addr + 3))
--        ++# (mem !! (addr + 2))
--        ++# (mem !! (addr + 1))
--        ++# (mem !! addr)
