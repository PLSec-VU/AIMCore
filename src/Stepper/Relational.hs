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
import ISA (Func, Instr (..), PC, apply, unDone)
import Instruction (loadExtend)
import Stepper (Fuel (..))
import Stepper.Concrete (concreteAddress, knownBool)
import Stepper.Rel (Rel (..), agree, left)
import Stepper.RelState
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx)
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
relStep st@(RelState path code pc _ regs mem) = case currentFetch st of
  Nothing -> pure RelOutsideCode
  Just (here, next) -> execute here next
  where
    execute here next = case fetchedInstr here of
      Reg rd f ->
        pure $ RelNext (fallThrough st {relRegs = writeSlot (fieldSlot rd) (apSources f) regs})
      Load size sign rd f -> withAgreed LeakLoadAddress (apSources f) \addr ->
        let loaded word = writeSlot (fieldSlot rd) (loadExtend size sign <$> word) regs
         in pure $ RelNext $ fallThrough case loadKnownWord addr mem of
              Just (word, mem') -> st {relRegs = loaded word, relMem = mem'}
              Nothing -> st {relRegs = loaded (readMemWord addr mem)}
      Jump rd link target -> withAgreed LeakJumpTarget (apSources target) \target' ->
        jumpTo target' \addr -> landAt addr st {relRegs = writeSlot (fieldSlot rd) (bitCoerce <$> ap link) regs}
      Store size addr rs2 -> withAgreed LeakStoreAddress (apSources addr) \addr' -> do
        mem' <- writeMem path size addr' (readSlot regs (fieldSlot rs2)) mem
        pure $ RelNext (fallThrough st {relMem = mem'})
      Branch cond target -> do
        agreed <- agree path (apSources cond)
        case agreed of
          Nothing -> pure (RelLeaks pc LeakBranch)
          Just taken -> forkOn taken (landAt (left (ap target)) st) (fallThrough st)
      Nop -> pure $ RelNext (fallThrough st)
      Break -> pure RelHalted
      Syscall -> pure RelHalted
      where
        fallThrough later = case next of
          Just following -> later {relPc = fetchedAddress following, relFetchHint = Just following}
          Nothing -> later {relPc = pc + 4, relFetchHint = Nothing}

        cached = fetchedOperands here

        fieldSlot :: RegIdx -> RegSlot
        fieldSlot reg = maybe (slotFor reg) registerFieldSlot cached

        apSources :: Func a -> Rel a
        apSources f = apWith (maybe (funcSlots f) sourceSlots cached) f

        ap :: Func a -> Rel a
        ap f = apWith (funcSlots f) f

        apWith :: (RegSlot, RegSlot) -> Func a -> Rel a
        apWith (first, second) f =
          (\r1 r2 -> unDone (apply f r1 r2 pc))
            <$> readSlot regs first
            <*> readSlot regs second

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
    jumpTo target landing = case (concreteAddress target, codeAddresses code) of
      (Nothing, KnownAddresses addrs) -> do
        outside <- feasibleOutcomes [leaveCode]
        oneOutcome <$> landingsAmong addrs outside
        where
          leaveCode = Candidate (missesEvery addrs target) (const RelOutsideCode)

          landOn addr = RelNext (landing addr) {relPath = path && target == addr}

          reachable addrs' = isFeasible (path && hitsSome addrs' target)

          landingsAmong addrs' later = do
            canLand <- reachable addrs'
            if canLand then landingsAmongReachable addrs' later else pure later

          landingsAmongReachable [] later = pure later
          landingsAmongReachable [addr] later = pure (landOn addr : later)
          landingsAmongReachable addrs' later = case halves addrs' of
            (lower, upper) -> do
              lowerReachable <- reachable lower
              if lowerReachable
                then landingsAmongReachable lower =<< landingsAmong upper later
                else landingsAmongReachable upper later
      _ -> pure (RelNext (landing target))

    isFeasible :: Bool -> SymFC.Sym Bool
    isFeasible guardedPath = do
      feasible <- SymFC.satisfiable $ SymFC.boolean guardedPath
      pure case feasible of
        SymFC.None -> False
        _ -> True

    forkAmong :: [Candidate] -> SymFC.Sym RelOutcome
    forkAmong candidates = oneOutcome <$> feasibleOutcomes candidates

    feasibleOutcomes :: [Candidate] -> SymFC.Sym [RelOutcome]
    feasibleOutcomes [] = pure []
    feasibleOutcomes (Candidate guard outcome : rest) = do
      let guardedPath = path && guard
      feasible <- isFeasible guardedPath
      later <- feasibleOutcomes rest
      pure (if feasible then outcome guardedPath : later else later)

landAt :: PC -> RelState -> RelState
landAt addr st = st {relPc = addr, relFetchHint = Nothing}

currentFetch :: RelState -> Maybe (Fetched, Maybe Fetched)
currentFetch st = case relFetchHint st of
  Just hint -> Just (hint, fetchedFollowing hint)
  Nothing -> withFollowing <$> codeFetch (relCode st) (relPc st)
  where
    withFollowing here = (here, if fetchedAtPc here then fetchedFollowing here else Nothing)
    fetchedAtPc here = case knownBool (fetchedAddress here == relPc st) of
      Just True -> True
      _ -> False

missesEvery :: [Address] -> PC -> Bool
missesEvery addrs target = foldr (\addr rest -> target /= addr && rest) True addrs

hitsSome :: [Address] -> PC -> Bool
hitsSome addrs target = foldr (\addr rest -> target == addr || rest) False addrs

halves :: [a] -> ([a], [a])
halves items = splitBeside items items
  where
    splitBeside (item : rest) (_ : _ : faster) = case splitBeside rest faster of
      (front, back) -> (item : front, back)
    splitBeside rest _ = ([], rest)

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
