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

data LeakInst
  = RegStore RegIdx Word
  | MemLoad RegIdx (Word -> Address)
  | MemStore Address
  | Jump (Either (Address -> Address) Address)
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
    leakOutALUOp :: First (Word -> Word -> Word),
    leakOutALU :: First Word
  }

instance Semigroup LeakOut where
  LeakOut i f a <> LeakOut i' f' a' = LeakOut (i <> i') (f <> f') (a <> a')

instance Monoid LeakOut where
  mempty = LeakOut mempty mempty mempty

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

aluOut :: Word -> LeakM ()
aluOut v =
  tell $ mempty {leakOutALU = pure v}

instOut :: LeakInst -> LeakM ()
instOut inst =
  tell $ mempty {leakOutInst = pure inst}

leak :: Input -> LeakM ()
leak (Input mem rs1 rs2) = do
  case Instruction.decode mem of
    RType op rd _ _ ->
      tell $
        mempty {leakOutInst = pure $ RegStore rd $ alu op rs1 rs2}
    IType (Load size sign) rd rs1 imm ->
      tell $
        mempty {leakOutInst = pure $ MemLoad rd $ bitCoerce . alu ADD (signExtend imm)}
    JType rd imm -> do
      let addr pc =
            bitCoerce $
              alu ADD (bitCoerce pc) (signExtend imm)
      tell $
        mempty {leakOutInst = pure $ Jump $ Left addr}

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
type SimM = RWS LeakOut (First Address) SimState

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

data SimControl = SimControl
  { simJump :: Maybe (Address)
  }

initControl =
  SimControl
    { simJump = Nothing
    }

-- checkLines :: (MonadState SimState m) => [SimControl -> Bool] -> m Bool
-- checkLines ls = do
--  ctrl <- gets simControl
--  pure $ or [test ctrl | test <- ls]
--
-- setLines :: (MonadState SimState m) => (SimControl -> SimControl) -> m ()
-- setLines f = modify $ \s -> s {simControl = f $ simControl s}

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
    minstr <- asks $ getFirst . leakOutInst
    let instr =
          case fromMaybe Nop minstr of
            Jump (Left f) -> Jump $ Right $ f $ simPc s
    modify $ \s ->
      s
        { simDeInstr = fromMaybe Nop minstr,
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
    Jump (Right addr) ->
      modify $ \s ->
        s
          { simPc = addr,
            simBubble = True
          }
  -- setLines $ \c -> c {simJump = pure addr}

  modify $ \s -> s {simMemInstr = simExInstr s}

simMemory :: SimM ()
simMemory = do
  instr <- gets simMemInstr
  undefined
  -- case instr of {}

  modify $ \s -> s {simWbInstr = simExInstr s}

simWriteback :: SimM ()
simWriteback = do
  instr <- gets simWbInstr
  case instr of
    RegStore rd val -> pure ()
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
