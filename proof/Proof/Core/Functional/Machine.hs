{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The system the core's proofs reason about: the core together with ideal
-- memory.
--
-- 'Core.circuit' is only the pipeline: memory lives outside it. The core emits
-- a 'MemAccess' on its 'Output' and receives the response on the next 'Input'.
-- So a single system step is one 'Core.circuit' step plus ideal memory's answer
-- ('idealServe'), and the state the proofs reason about is the triple
--
-- > (Core.State, Input, memory)
module Proof.Core.Functional.Machine
  ( SysG (..),
    Sys,
    stepSys,
    stepSysOut,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Memory.Types
import Memory.Ideal (idealServe)
import Core.RegFile
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Core state, the input it is about to consume, and memory.
data SysG r m = Sys
  { sysCore :: Core.StateG r,
    sysInput :: Input,
    sysMem :: m
  }

-- | The concrete system the QuickCheck harness runs: @Vec@-backed throughout.
type Sys = SysG RegFile MemBytes

deriving instance (Show r, Show m) => Show (SysG r m)

-- | Structural equality.
instance (Eq r, Eq m) => Eq (SysG r m) where
  Sys s1 i1 m1 == Sys s2 i2 m2 =
    s1 == s2
      && inputMem i1 == inputMem i2
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
--
-- The system is the core in a loop with 'idealServe': the core's request goes
-- to memory, and memory's response is the core's input in the next cycle.
stepSysOut :: (RegFileOps r, MemOps m) => SysG r m -> (SysG r m, Output)
stepSysOut (Sys core inp mem) =
  let (core', o) = Core.circuit core inp
      (mem', inp') = idealServe mem (outMem o)
   in (Sys core' inp' mem', o)
