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
import Stepper.Concrete (concreteAddress, knownBool)
import Stepper.Rel (Rel (..), agree, left)
import Stepper.RelState
import SymFC.BuiltIn qualified as SymFC
import Types (Address)
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
  | RelFork [RelOutcome]
  | RelLeaks PC LeakKind

data Candidate = Candidate
  { candidateGuard :: Bool
  , candidateOutcome :: Bool -> RelOutcome
  }

continueOn :: Bool -> RelState -> Candidate
continueOn guard next = Candidate guard \path' -> RelNext next {relPath = path'}

relStep :: RelState -> SymFC.Sym RelOutcome
relStep st@(RelState path code pc regs mem) = case codeFetch code pc of
  Nothing -> pure RelOutsideCode
  Just word -> execute (interp (decode' word))
  where
    execute instr = case instr of
      Reg rd f ->
        pure $ RelNext st {relPc = pc + 4, relRegs = writeReg rd (ap f) regs}
      Load size sign rd f -> withAgreed LeakLoadAddress (ap f) \addr ->
        let value = loadExtend size sign <$> readMemWord addr mem
         in pure $ RelNext st {relPc = pc + 4, relRegs = writeReg rd value regs}
      Jump rd link target -> withAgreed LeakJumpTarget (ap target) \target' ->
        jumpTo target' \addr -> st {relPc = addr, relRegs = writeReg rd (bitCoerce <$> ap link) regs}
      Store size addr rs2 -> withAgreed LeakStoreAddress (ap addr) \addr' -> do
        mem' <- writeMem path size addr' (readReg regs (Just rs2)) mem
        pure $ RelNext st {relPc = pc + 4, relMem = mem'}
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

    withAgreed :: Eq a => LeakKind -> Rel a -> (a -> SymFC.Sym RelOutcome) -> SymFC.Sym RelOutcome
    withAgreed kind value continue = do
      agreed <- agree path value
      case agreed of
        Nothing -> pure (RelLeaks pc kind)
        Just a -> continue a

    forkOn :: Bool -> RelState -> RelState -> SymFC.Sym RelOutcome
    forkOn cond whenTrue whenFalse = case knownBool cond of
      Just True -> pure (RelNext whenTrue)
      Just False -> pure (RelNext whenFalse)
      Nothing -> forkAmong [continueOn cond whenTrue, continueOn (not cond) whenFalse]

    jumpTo :: PC -> (PC -> RelState) -> SymFC.Sym RelOutcome
    jumpTo target landAt = case (concreteAddress target, codeAddresses code) of
      (Nothing, KnownAddresses addrs) -> forkAmong (map landOn addrs ++ [leaveCode])
        where
          landOn addr = continueOn (target == addr) (landAt addr)
          leaveCode = Candidate (missesEvery addrs target) (const RelOutsideCode)
      _ -> pure (RelNext (landAt target))

    forkAmong :: [Candidate] -> SymFC.Sym RelOutcome
    forkAmong candidates = oneOutcome <$> feasibleOutcomes candidates

    feasibleOutcomes :: [Candidate] -> SymFC.Sym [RelOutcome]
    feasibleOutcomes [] = pure []
    feasibleOutcomes (Candidate guard outcome : rest) = do
      let guardedPath = path && guard
      feasible <- SymFC.satisfiable $ SymFC.boolean guardedPath
      later <- feasibleOutcomes rest
      pure case feasible of
        SymFC.None -> later
        _ -> outcome guardedPath : later

missesEvery :: [Address] -> PC -> Bool
missesEvery addrs target = foldr (\addr rest -> target /= addr && rest) True addrs

oneOutcome :: [RelOutcome] -> RelOutcome
oneOutcome [] = RelInfeasible
oneOutcome [outcome] = outcome
oneOutcome outcomes = RelFork outcomes

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
relRun (More fuel) st = follow fuel =<< relStep st

follow :: Fuel -> RelOutcome -> SymFC.Sym Verdict
follow fuel outcome = case outcome of
  RelNext st' -> relRun fuel st'
  RelHalted -> pure NoLeak
  RelOutsideCode -> pure NoLeak
  RelInfeasible -> pure NoLeak
  RelFork branches -> followAll fuel branches
  RelLeaks pc kind -> pure (Leaks pc kind)

followAll :: Fuel -> [RelOutcome] -> SymFC.Sym Verdict
followAll _ [] = pure NoLeak
followAll fuel (branch : rest) = do
  verdict <- follow fuel branch
  case verdict of
    Leaks {} -> pure verdict
    _ -> laterPathVerdict verdict <$> followAll fuel rest
