{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.Relational
  ( RelOutcome (..)
  , LeakKind (..)
  , relStep
  , Verdict (..)
  , relRun
  ) where

import Clash.Prelude (bitCoerce)
import ISA (DepReg (..), Func, Instr (..), PC, apply, interp, unDone)
import Instruction (decode', loadExtend)
import Stepper (Fuel (..))
import Stepper.Rel (Rel (..), agree, left)
import Stepper.RelState
import SymFC.BuiltIn qualified as SymFC
import Prelude hiding (Word)

data LeakKind
  = LeakBranch
  | LeakJumpTarget
  | LeakLoadAddress
  | LeakStoreAddress
  deriving (Eq)

data RelOutcome
  = RelNext RelState
  | RelHalted
  | RelOutsideCode
  | RelInfeasible
  | RelFork RelState RelState
  | RelLeaks PC LeakKind

relStep :: RelState -> SymFC.Sym RelOutcome
relStep st@(RelState path code pc regs mem) = case code pc of
  Nothing -> pure RelOutsideCode
  Just word -> execute (interp (decode' word))
  where
    execute instr = case instr of
      Reg rd f ->
        pure $ RelNext st {relPc = pc + 4, relRegs = writeReg rd (ap f) regs}
      Load size sign rd f -> withAgreed LeakLoadAddress (ap f) \addr ->
        let value = loadExtend size sign <$> readMemWord addr mem
         in RelNext st {relPc = pc + 4, relRegs = writeReg rd value regs}
      Jump rd link target -> withAgreed LeakJumpTarget (ap target) \target' ->
        RelNext st {relPc = target', relRegs = writeReg rd (bitCoerce <$> ap link) regs}
      Store size addr rs2 -> withAgreed LeakStoreAddress (ap addr) \addr' ->
        RelNext st {relPc = pc + 4, relMem = writeMem size addr' (readReg regs (Just rs2)) mem}
      Branch cond target -> do
        agreed <- agree path (ap cond)
        case agreed of
          Nothing -> pure (RelLeaks pc LeakBranch)
          Just taken -> forkOn taken st {relPc = left (ap target)} st {relPc = pc + 4}
      Nop -> pure $ RelNext st {relPc = pc + 4}
      Break -> pure RelHalted
      Syscall -> pure RelHalted
      where
        ap :: Func a -> Rel a
        ap f =
          (\r1 r2 -> unDone (apply f r1 r2 pc))
            <$> readReg regs (fst (deps f))
            <*> readReg regs (snd (deps f))

    withAgreed :: Eq a => LeakKind -> Rel a -> (a -> RelOutcome) -> SymFC.Sym RelOutcome
    withAgreed kind value continue = do
      agreed <- agree path value
      pure case agreed of
        Nothing -> RelLeaks pc kind
        Just a -> continue a

    forkOn :: Bool -> RelState -> RelState -> SymFC.Sym RelOutcome
    forkOn cond whenTrue whenFalse = do
      let truePath = path && cond
      let falsePath = path && not cond
      canTake <- SymFC.satisfiable $ SymFC.boolean truePath
      canSkip <- SymFC.satisfiable $ SymFC.boolean falsePath
      pure case (canTake, canSkip) of
        (SymFC.None, SymFC.None) -> RelInfeasible
        (SymFC.None, _) -> RelNext whenFalse {relPath = falsePath}
        (_, SymFC.None) -> RelNext whenTrue {relPath = truePath}
        _ -> RelFork whenTrue {relPath = truePath} whenFalse {relPath = falsePath}

data Verdict
  = NoLeak
  | OutOfFuel
  | Leaks PC LeakKind

laterPathVerdict :: Verdict -> Verdict -> Verdict
laterPathVerdict _ leak@Leaks {} = leak
laterPathVerdict OutOfFuel _ = OutOfFuel
laterPathVerdict _ later = later

relRun :: Fuel -> RelState -> SymFC.Sym Verdict
relRun Done _ = pure OutOfFuel
relRun (More fuel) st = do
  outcome <- relStep st
  case outcome of
    RelNext st' -> relRun fuel st'
    RelHalted -> pure NoLeak
    RelOutsideCode -> pure NoLeak
    RelInfeasible -> pure NoLeak
    RelFork whenTrue whenFalse -> do
      verdict <- relRun fuel whenTrue
      case verdict of
        Leaks {} -> pure verdict
        _ -> laterPathVerdict verdict <$> relRun fuel whenFalse
    RelLeaks pc kind -> pure (Leaks pc kind)
