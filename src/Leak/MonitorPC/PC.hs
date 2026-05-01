{-# LANGUAGE UndecidableInstances #-}

module Leak.MonitorPC.PC (
  obs,
  -- comment them out to disable Pantomime checks for faster compilation
  -- tickStateCorrespondence,
  -- projectionCoherence,
)
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Core (Input (..), MemAccess (..), Output (..))
import qualified Core
import Data.Bifunctor (second)
import Data.Composition
import Data.Functor.Identity
import Data.Maybe (isJust)
import Data.Monoid
import qualified Leak.MonitorPC.Sim as Sim
import qualified Leak.MonitorPC.MonitorLeak as Leak
import qualified Pantomime as P
import qualified Pantomime.Base as Base
import qualified Pantomime.Clash as Clash
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

implementation :: Core.State Identity -> Input Identity -> (Core.State Identity, Output Identity)
implementation = Core.circuit

circuits :: P.NonInterference (Core.State Identity) ((), Core.State Identity) Sim.State (Input Identity) (Leak.Instr, Maybe Address) (Maybe Address)
circuits =
  P.NonInterference
    { P.implementation = second obs' .: implementation
    , P.leakage = Leak.leakCircuit Leak.monitorPC
    , P.projection = proj
    }

{-# ANN tickStateCorrespondence (P.Theory $ Base.axioms <> Clash.axioms) #-}
tickStateCorrespondence :: Core.State Identity -> Input Identity -> Bool
tickStateCorrespondence = P.tickStateCorrespondence circuits

{-# ANN projectionCoherence (P.Theory $ Base.axioms <> Clash.axioms) #-}
projectionCoherence :: Core.State Identity -> Input Identity -> Core.State Identity -> Input Identity -> Bool
projectionCoherence = P.projectionCoherence circuits

stateless :: (a -> b) -> () -> a -> ((), b)
stateless f _ x = ((), f x)

obs :: () -> Output Identity -> ((), Maybe Address)
obs = stateless obs'

obs' :: Output Identity -> Maybe Address
obs' o_sim = do
  mem <- getFirst $ outMem o_sim
  guard $ memIsInstr mem
  pure $ memAddress mem

proj :: Core.State Identity -> (((), Core.State Identity), Sim.State)
proj s = (ts, ss)
 where
  ts = Leak.leakProject Leak.monitorPC s
  ss =
    Sim.State
      { Sim.stateFePc = Core.stateFePc s
      , Sim.stateDePc = Core.stateDePc s
      , Sim.stateExPc = Core.stateExPc s
      , Sim.stateExInstr = Leak.toLeakInstr $ Core.stateExInstr s
      , Sim.stateMemInstr = Leak.toLeakInstr $ Core.stateMemInstr s
      , Sim.stateWbInstr = Leak.toLeakInstr $ Core.stateWbInstr s
      , Sim.stateHalt = Core.stateHalt s /= Core.Running
      , Sim.stateStallFetch = toStallFetch $ Core.stateCtrl s
      , Sim.stateStallDecode = toStallDecode $ Core.stateCtrl s
      , Sim.stateJumpAddr = Core.ctrlExBranch $ Core.stateCtrl s
      , Sim.stateFirstCycle = Core.ctrlFirstCycle $ Core.stateCtrl s
      }

  toStallFetch :: Core.Control Identity -> Bool
  toStallFetch ctrl =
    Core.ctrlDecodeLoad ctrl
      || Core.ctrlMeOutputActive ctrl
      || isJust (Core.ctrlExBranch ctrl)

  toStallDecode :: Core.Control Identity -> Bool
  toStallDecode ctrl =
    Core.ctrlFirstCycle ctrl
      || isJust (Core.ctrlExBranch ctrl)
      || Core.ctrlMeBranch ctrl
      || Core.ctrlExLoad ctrl
      || Core.ctrlWbMemInstr ctrl
