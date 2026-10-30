-- | The system state in a form the solver can take as a fresh symbolic input.
--
-- The pipeline registers are passed as an ADT of scalars ('KState'), and the
-- register file and memory as SMT arrays: an ADT of scalars can be a fresh
-- symbolic value, a record containing a function cannot, and the Clash
-- @Vec@ API is opaque to the plugin (see "Proof.SMT.Array"). 'sysOf' assembles
-- the pieces into a 'Proof.Machine.SysG', and 'freshSys' makes one from fresh
-- pieces.
--
-- Shared by the functional and the leakage inductions, so that neither imports
-- the other: their obligations are discharged while GHC compiles them, and an
-- import between them would re-run one proof whenever the other changes.
module Proof.SMT.System
  ( KState (..),
    sysOf,
    freshSys,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Instruction (Instruction)
import Proof.Machine
import Proof.SMT.Array
import Proof.SMT.Symbolic ()
import qualified SymFC.BuiltIn as SymFC
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The pipeline registers, as plain scalars.
data KState = KState
  { kFePc :: Address,
    kDePc :: Address,
    kDeExpInstr :: Bool,
    kExPc :: Address,
    kExIr :: Instruction,
    kMeIr :: Instruction,
    kMeRes :: Word,
    kMeAddr :: Address,
    kWbIr :: Instruction,
    kWbRes :: Word,
    kCtrl :: Core.Control Identity,
    kHalt :: Maybe Core.HaltState,
    kHaltNextPc :: Address
  }
  deriving (Generic)

instance SymFC.Symbolic KState

-- | Assemble a system state from the symbolic pieces.
sysOf :: KState -> Core.Input Identity -> RegArr -> MemArr -> SysG RegArrF MemArr
sysOf ss i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = kFePc ss,
            Core.stateDePc = kDePc ss,
            Core.stateDeExpInstr = kDeExpInstr ss,
            Core.stateExPc = kExPc ss,
            Core.stateExInstr = kExIr ss,
            Core.stateMeInstr = kMeIr ss,
            Core.stateMeRes = Identity (kMeRes ss),
            Core.stateMeAddr = kMeAddr ss,
            Core.stateWbInstr = kWbIr ss,
            Core.stateWbRes = Identity (kWbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = kCtrl ss,
            Core.stateHalt = kHalt ss,
            Core.stateHaltNextPc = kHaltNextPc ss
          },
      sysInput = i,
      sysMem = ma
    }

-- | A system state whose every part is a fresh symbolic value.
freshSys :: SymFC.Sym (SysG RegArrF MemArr)
freshSys = sysOf <$> SymFC.fresh <*> SymFC.fresh <*> freshRA <*> freshMA
