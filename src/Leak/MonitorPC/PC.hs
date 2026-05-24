{-# LANGUAGE UndecidableInstances #-}

module Leak.MonitorPC.PC
  ( obs,
    proj,
    leak,
    Sim.State,
    -- comment them out to disable Pantomime checks for faster compilation
    -- tickStateCorrespondence,
    -- projectionCoherence,
  )
where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Core (Input (..), MemAccess (..), Output (..))
import qualified Core
import Data.Functor.Identity
import Data.Maybe (isJust)
import Data.Monoid
import qualified Leak.MonitorPC.MonitorLeak as Leak
import qualified Leak.MonitorPC.Sim as Sim
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

stateless :: (a -> b) -> () -> a -> ((), b)
stateless f _ x = ((), f x)

obs :: () -> Output Identity -> ((), Maybe Address)
obs = stateless obs'

obs' :: Output Identity -> Maybe Address
obs' o_sim = do
  mem <- getFirst $ outMem o_sim
  guard $ memIsInstr mem
  pure $ memAddress mem

leak :: ((), Core.State Identity) -> Input Identity -> (((), Core.State Identity), (Leak.Instr, Maybe Address))
leak = Leak.leakCircuit Leak.monitorPC

proj :: Core.State Identity -> (((), Core.State Identity), Sim.State)
proj s = (ts, ss)
  where
    ts = Leak.leakProject Leak.monitorPC s
    halted = Core.stateHalt s /= Core.Running
    ss =
      Sim.State
        { Sim.stateFePc = if halted then 0 else Core.stateFePc s,
          Sim.stateDePc = if halted then 0 else Core.stateDePc s,
          Sim.stateExPc = if halted then 0 else Core.stateExPc s,
          Sim.stateExInstr = if halted then Leak.nop' else Leak.toLeakInstr $ Core.stateExInstr s,
          Sim.stateMemInstr = if halted then Leak.nop' else killJump $ Leak.toLeakInstr $ Core.stateMemInstr s,
          Sim.stateMemRes = if halted then 0 else unAccess $ Core.stateMemRes s,
          Sim.stateWbInstr = if halted then Leak.nop' else killJump $ Leak.toLeakInstr $ Core.stateWbInstr s,
          Sim.stateWbRes = if halted then 0 else unAccess $ Core.stateWbRes s,
          Sim.stateHalt = halted,
          Sim.stateStallFetch = not halted && toStallFetch (Core.stateCtrl s),
          Sim.stateStallDecode = not halted && toStallDecode (Core.stateCtrl s),
          Sim.stateJumpAddr = if halted then Nothing else Core.ctrlExAddress $ Core.stateCtrl s,
          Sim.stateFirstCycle = not halted && Core.ctrlFirstCycle (Core.stateCtrl s)
        }

    killJump :: Leak.Instr -> Leak.Instr
    killJump (Leak.Instr (Leak.Jump' {}) _ _) = Leak.nop'
    killJump i = i

    toStallFetch :: Core.Control Identity -> Bool
    toStallFetch ctrl =
      Core.ctrlDeCall ctrl
        || Core.ctrlMeMemInstr ctrl
        || isJust (Core.ctrlExBranch ctrl)

    toStallDecode :: Core.Control Identity -> Bool
    toStallDecode ctrl =
      Core.ctrlFirstCycle ctrl
        || isJust (Core.ctrlDeLoadHazard ctrl)
        || isJust (Core.ctrlExBranch ctrl)
