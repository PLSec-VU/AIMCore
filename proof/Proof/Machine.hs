{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The \"system\" that the driver and the invariant talk about.
--
-- 'Core.circuit' is only the pipeline: memory lives outside it. The core emits
-- a 'MemAccess' on its 'Output' and receives the response on the next
-- 'Input'. So a single system step is one 'Core.circuit' step plus the memory
-- service that 'Simulate.simulator' performs, and the state the proof reasons
-- about is the triple
--
-- > (Core.State f, Input f, Mem)
--
-- which is exactly the shape the driver and invariant notes use.
module Proof.Machine
  ( SysG (..),
    Sys,
    stepSys,
    stepSysOut,
    stepSysN,
    initSys,
    running,
    exInstr,
    isNopInstr,
    isBubble,
    readMemWord,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Data.Maybe (isNothing)
import Data.Monoid (getFirst)
import Instruction
import Memory.Types
import RegFile
import Types
import qualified Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Core state, the input it is about to consume, and memory.
data SysG r m = Sys
  { sysState :: Core.StateG r Identity,
    sysInput :: Input Identity,
    sysMem :: m
  }

-- | The concrete system the QuickCheck harness runs: @Vec@-backed throughout.
type Sys = SysG RegFile MemBytes

deriving instance (Show (r Identity), Show m) => Show (SysG r m)

-- | Structural equality. 'Input' has no 'Eq' instance in "Core" (adding one
-- there clashes with the existing @Eq Out@ in the Leak modules), so we compare
-- its fields directly.
instance (Eq (r Identity), Eq m) => Eq (SysG r m) where
  Sys s1 i1 m1 == Sys s2 i2 m2 =
    s1 == s2
      && inputIsInstr i1 == inputIsInstr i2
      && runIdentity (inputMem i1) == runIdentity (inputMem i2)
      && m1 == m2

-- | One system step: run the pipeline for a cycle, then service whatever
-- memory access it emitted. Mirrors 'Simulate.simulator', except that a halted
-- core simply stops changing instead of ending the stream.
stepSys :: (RegFileOps r, MemOps m) => SysG r m -> SysG r m
stepSys = fst . stepSysOut

-- | 'stepSys', but also returning the pipeline's 'Output' for that cycle.
--
-- The leakage proof observes the cycle-by-cycle memory traffic, which 'stepSys'
-- discards. Defining both here means what it observes is exactly the traffic
-- the memory service responds to.
stepSysOut :: (RegFileOps r, MemOps m) => SysG r m -> (SysG r m, Output Identity)
stepSysOut (Sys s i m) =
  let (s', o) = Core.circuit s i
      (i', m') = service (getFirst (outMem o)) m
   in (Sys s' i' m', o)
  where
    service (Just (MemAccess isInstr addr size mval)) mem =
      case mval of
        -- A read. Note 'Memory.Vec.ramRead' ignores the size and always reads a
        -- word; the size-dependent narrowing happens in 'Core.writeback' via
        -- 'loadExtend'. We reproduce that here.
        Nothing -> (Input isInstr (pure (memReadWord addr mem)), mem)
        -- A write.
        Just val -> (Input isInstr (pure 0), memWriteWord size addr (runIdentity val) mem)
    service Nothing mem = (Input False (pure 0), mem)

stepSysN :: (RegFileOps r, MemOps m) => Int -> SysG r m -> SysG r m
stepSysN n s
  | n <= 0 = s
  | otherwise = stepSysN (n - 1) (stepSys s)

-- | The system as it starts up, with @prog@ loaded at 'initPc'.
initSys :: Vec PROG_SIZE Word -> Sys
initSys prog =
  Sys
    { sysState = Core.init,
      sysInput = initInput,
      sysMem = mkRAM @PROG_SIZE @RAM_SIZE_BYTES prog
    }

running :: SysG r m -> Bool
running = isNothing . stateHalt . sysState

exInstr :: SysG r m -> Instruction
exInstr = stateExInstr . sysState

isNopInstr :: Instruction -> Bool
isNopInstr (Nop _) = True
isNopInstr _ = False

-- | Is this a pipeline bubble, as opposed to a real instruction?
--
-- The driver's stated invariant is \"step until the execute stage is no longer
-- a no-op\", but that is slightly too coarse: @Nop DecodeFail@ is what an
-- undecodable memory word decodes to, so it /is/ the architectural instruction
-- at that PC and the ISA steps over it like any other. Only the stall reasons
-- are genuine bubbles.
isBubble :: Instruction -> Bool
isBubble (Nop DecodeFail) = False
isBubble (Nop _) = True
isBubble _ = False

readMemWord :: Address -> MemBytes -> Word
readMemWord = readWord
