module Leak.MonitorPC.MonitorLeak (
  LeakMonitor (..),
  (^&^),
  leakPC,
  Instr (..),
  BaseInstr (..),
  toLeakInstr,
  nop',
)
where

import Control.Monad.Identity
import qualified Core
import Instruction
import Types

-- A leakage is a mealy machine with state sl, input ii, output ol,
-- equipped with a state projection from si to sl.
data LeakMonitor si ii sl ol = LeakMonitor
  { leakCircuit :: (sl -> ii -> (sl, ol))
  , leakProject :: si -> sl
  }

-- Combine two leakages into a leakage that leaks both.
(^&^) :: LeakMonitor si ii sl1 ol1 -> LeakMonitor si ii sl2 ol2 -> LeakMonitor si ii (sl1, sl2) (ol1, ol2)
(LeakMonitor f1 p1) ^&^ (LeakMonitor f2 p2) = LeakMonitor leak (\s -> (p1 s, p2 s))
 where
  leak (sl1, sl2) i = ((sl1', sl2'), (o1, o2))
   where
    (sl1', o1) = f1 sl1 i
    (sl2', o2) = f2 sl2 i

leakPC :: LeakMonitor (Core.State Identity) (Core.Input Identity) ((), Core.State Identity) (Instr, Maybe Address)
leakPC = leakInstructionType ^&^ leakJumpAddress

data BaseInstr
  = Jump'
  | Load'
  | Store'
  | Other'
  | Call'
  | Break'
  deriving (Show, Eq)

data Instr = Instr BaseInstr (Maybe RegIdx) (Maybe RegIdx)
  deriving (Show, Eq)

toLeakInstr :: Instruction -> Instr
toLeakInstr input = Instr (mkInstr input) (getRs1 input) (getRs2 input)
 where
  mkInstr :: Instruction -> BaseInstr
  mkInstr instr = case instr of
    RType{} -> Other'
    IType iop _ _ _ -> case iop of
      Arith{} -> Other'
      Load{} -> Load'
      Jump -> Jump'
      Env Break -> Break'
      Env Call -> Call'
    SType{} -> Store'
    BType{} -> Jump'
    UType Zero _ _ -> Other'
    UType PC _ _ -> Other'
    JType{} -> Jump'

nop' :: Instr
nop' = toLeakInstr nop

leakInstructionType :: LeakMonitor (Core.State Identity) (Core.Input Identity) () Instr
leakInstructionType = LeakMonitor (\() i -> ((), leak i)) (const ())
 where
  leak :: Core.Input Identity -> Instr
  leak input | Core.inputIsInstr input = toLeakInstr $ decode' $ runIdentity $ Core.inputMem input
  leak _ = nop'

leakJumpAddress :: LeakMonitor (Core.State Identity) (Core.Input Identity) (Core.State Identity) (Maybe Address)
leakJumpAddress = LeakMonitor leak id
 where
  leak :: Core.State Identity -> Core.Input Identity -> (Core.State Identity, Maybe Address)
  leak s i = let (s', _) = Core.circuit s i in (s', Core.ctrlExBranch $ Core.stateCtrl s')