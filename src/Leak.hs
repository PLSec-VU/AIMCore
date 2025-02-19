module Leak where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Instruction hiding (decode, halt)
import qualified Instruction
import Pipe
import Regfile
import Types
import Prelude hiding (Ordering (..), Word, init, not, undefined, (!!), (&&), (||))

-- `fromJust` is safe here because the `fetch` stage unconditionally always reads
-- from memory (and its read may just be superseded by the `memory` stage).
obs :: Output -> Address
obs = memAddress . fromJust . getFirst . outMem

-- | Instructions passed to the Simulator
data LeakInst
  = LBranch Bool Address
  | LJ RegIdx Address
  | LJAbsolute RegIdx Address
  | LAddPC Address
  | LOther
  deriving (Eq, Ord, Show)

data LeakState n = LState
  { leakRF :: Regfile,
    leakRAM :: Vec n Byte,
    leakBubble :: Bool
  }
  deriving (Eq, Show)

type LeakM n = State (LeakState n)

leak :: (KnownNat n) => Word -> LeakM n LeakInst
leak mem =
  case Instruction.decode mem of
    RType op rd r1 r2 -> do
      writeRF rd =<< alu op <$> readRF r1 <*> readRF r2
      pure LOther
    IType iop rd r1 imm -> do
      let op =
            case iop of
              Arith op' -> op'
              _ -> ADD
      res <- alu op <$> readRF r1 <*> pure (signExtend imm)
      case iop of
        Arith {} -> pure LOther
        Load size sign -> do
          let loadExtend = \case
                (Byte, Signed) -> signExtend $ slice d7 d0 res
                (Byte, Unsigned) -> zeroExtend $ slice d7 d0 res
                (Half, Signed) -> signExtend $ slice d15 d0 res
                (Half, Unsigned) -> signExtend $ slice d15 d0 res
                (Word, _) -> signExtend $ slice d31 d0 res
              val = loadExtend (size, sign)
          writeRF rd =<< readRAM (unpack val)
          pure LOther
        Jump -> do
          writeRF rd res
          pure $ LJAbsolute rd $ bitCoerce res
        Env {} -> error ""
    SType size imm r1 r2 -> do
      addr <- unpack <$> (alu ADD <$> readRF r1 <*> pure (signExtend imm))
      val <- readRF r2
      writeRAM size addr val
      pure LOther
    BType cmp imm r1 r2 -> do
      branched <- branch cmp <$> readRF r1 <*> readRF r2
      pure $ LBranch branched $ unpack $ signExtend imm
    UType Zero rd imm -> do
      let imm' = imm ++# 0 `shiftL` 12
      writeRF rd $ imm'
      pure LOther
    UType PC rd imm ->
      let imm' = imm ++# 0 `shiftL` 12
       in pure $ LAddPC $ unpack imm'
    JType rd imm ->
      pure $ LJ rd $ bitCoerce $ signExtend imm

type SimM = State SimState

data SimState = SimState
  { simPc :: Address,
    simNextPc :: Address,
    simInstr :: LeakInst
  }
  deriving (Show, Eq)

simFetch :: LeakInst -> SimM Address
simFetch _ = gets simNextPc

simExecute :: SimM ()
simExecute = do
  instr <- gets simInstr
  case instr of
    LBranch branch imm
      | branch ->
          modify $ \s -> s {simNextPc = simPc s + imm}
      | otherwise ->
          modify $ \s -> s {simNextPc = simPc s + 4}
    LOther ->
      modify $ \s -> s {simNextPc = simPc s + 4}

simMemory :: SimM ()
simMemory = undefined

readRF :: RegIdx -> LeakM n Word
readRF idx = gets $ lookupRF idx . leakRF

writeRF :: RegIdx -> Word -> LeakM n ()
writeRF idx val =
  modify $ \s -> s {leakRF = modifyRF idx val $ leakRF s}

-- TODO: Fix the code duplication with Simulator.hs
writeRAM :: (KnownNat n) => Size -> Address -> Word -> LeakM n ()
writeRAM size addr w =
  modify $ \s -> s {leakRAM = write size addr w $ leakRAM s}
  where
    write size addr w mem =
      let b0 = slice d7 d0 w
          b1 = slice d15 d8 w
          b2 = slice d23 d16 w
          b3 = slice d31 d24 w
          writeByte =
            replace addr b0
          writeHalf =
            replace (addr + 1) b1 . writeByte
          writeWord =
            replace (addr + 3) b3
              . replace (addr + 2) b2
              . writeHalf
       in case size of
            Byte -> writeByte mem
            Half -> writeHalf mem
            Word -> writeWord mem

readRAM :: (KnownNat n) => Address -> LeakM n Word
readRAM addr = gets $ readWord addr . leakRAM
  where
    readWord addr mem =
      (mem !! (addr + 3))
        ++# (mem !! (addr + 2))
        ++# (mem !! (addr + 1))
        ++# (mem !! addr)
