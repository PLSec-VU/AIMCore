{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.Relational
  ( RelOutcome (..)
  , relStep
  , Verdict (..)
  , relRun
  , leakOfReadsAgree
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Maybe (isJust)
import ISA (Flow (..), IsaStateG (..), Machine (..), PC, isaStepM)
import Instruction (Instruction)
import Instruction qualified as I
import Memory.Types (MemOps (..))
import Proof.Leakage.Model (leakOf)
import RegFile (RegFn (..))
import Stepper (Fuel (..))
import Stepper.Concrete (concreteAddress, concreteRegIdx, knownBool)
import Stepper.Memory (Memory)
import Stepper.Memory qualified as Memory
import Stepper.Rel (Rel (..), agree, left, right)
import Stepper.RelRegs (RegSlot (AnyRegister), knownSlot)
import Stepper.RelState
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Size (Word))
import Prelude hiding (Word)

data RelOutcome
  = RelNext RelState
  | RelHalted
  | RelOutsideCode
  | RelInfeasible
  | RelFork [RelOutcome]
  | RelLeaks PC

data Candidate = Candidate
  { candidateGuard :: Bool
  , candidateOutcome :: Bool -> RelOutcome
  }

continueOn :: Bool -> RelState -> Candidate
continueOn guard next = Candidate guard \path' -> RelNext next {relPath = path'}

type RunState = IsaStateG RegFn (Memory 32 8)

runStates :: RelState -> Fetched -> Maybe Fetched -> Rel RunState
runStates st here next = Pair (runState left leftMem) (runState right rightMem)
  where
    runState side sideMem =
      IsaState
        { isaPc = relPc st
        , isaRegFile = RegFn \idx -> Identity (side (readSlot (relRegs st) (AnyRegister idx)))
        , isaMem = memoryLeakOfReads (sideMem (relMem st))
        }
    memoryLeakOfReads sideMem = case maybe (codeFetch (relCode st) (relPc st + 4)) Just next of
      Just following -> withCodeWord here (withCodeWord following Memory.empty)
      Nothing -> withCodeWord here sideMem
    withCodeWord instr = memWriteWord Word (fetchedAddress instr) (fetchedWord instr)

leakOfReadsAgree :: Applicative m => (RegIdx -> m Bool) -> m Bool -> Instruction -> m Bool
leakOfReadsAgree agrees followingAgrees ir = case ir of
  I.BType _ _ rs1 rs2 -> (&&) <$> agrees rs1 <*> agrees rs2
  I.IType I.Jump _ rs1 _ -> agrees rs1
  I.IType (I.Load _ _) _ rs1 _ -> (&&) <$> agrees rs1 <*> followingAgrees
  I.SType _ _ rs1 _ -> agrees rs1
  _ -> pure True

isSame :: Rel a -> Bool
isSame (Same _) = True
isSame (Pair _ _) = False

sameRegister :: RelRegs -> RegIdx -> SymFC.Sym Bool
sameRegister regs reg = do
  known <- concreteRegIdx reg
  pure case known of
    Just idx -> isSame (readSlot regs (knownSlot idx))
    Nothing -> False

-- | One step of 'isaStepM' on both runs at once.
newtype RelM a = RelM {runRelM :: RelState -> SymFC.Sym (a, RelState)}

instance Functor RelM where
  fmap f (RelM run) = RelM \st -> (\(a, st') -> (f a, st')) <$> run st

instance Applicative RelM where
  pure a = RelM \st -> pure (a, st)
  RelM runF <*> RelM runA = RelM \st -> do
    (f, st') <- runF st
    (a, st'') <- runA st'
    pure (f a, st'')

instance Monad RelM where
  RelM run >>= k = RelM \st -> do
    (a, st') <- run st
    runRelM (k a) st'

instance Machine Rel RelM where
  readReg idx = RelM \st -> do
    slot <- slotFor idx
    pure (readSlot (relRegs st) slot, st)
  writeReg idx value = RelM \st -> do
    slot <- slotFor idx
    pure ((), st {relRegs = writeSlot slot value (relRegs st)})
  loadWord addr = RelM \st -> do
    (word, mem') <- loadMemWord addr (relMem st)
    pure (word, st {relMem = mem'})
  storeWord size addr value = RelM \st -> do
    mem' <- writeMem (relPath st) size addr value (relMem st)
    pure ((), st {relMem = mem'})

relStep :: RelState -> SymFC.Sym RelOutcome
relStep st@(RelState path code pc _ regs _) = do
  current <- currentFetch st
  case current of
    Nothing -> pure RelOutsideCode
    Just (here, next) -> do
      readsAgree <- leakOfReadsAgree (sameRegister regs) (pure (isJust next)) (fetchedDecodedWord here)
      sameLeak <-
        if readsAgree
          then pure True
          else isJust <$> agree path (leakOf <$> runStates st here next)
      if sameLeak
        then continueAfter next =<< runRelM (isaStepM pc (fetchedInstr here)) st
        else pure (RelLeaks pc)
  where
    continueAfter next (control, later) = case control of
      FallThrough -> pure (RelNext (fallThrough later))
      Goto target -> jumpTo target (`landAt` later)
      BranchTo taken target -> forkOn taken (landAt target later) (fallThrough later)
      Halt -> pure RelHalted
      where
        fallThrough after = case next of
          Just following -> after {relPc = fetchedAddress following, relFetchHint = Just following}
          Nothing -> after {relPc = pc + 4, relFetchHint = Nothing}

    forkOn :: Bool -> RelState -> RelState -> SymFC.Sym RelOutcome
    forkOn cond whenTrue whenFalse = do
      known <- knownBool cond
      case known of
        Just True -> pure (RelNext whenTrue)
        Just False -> pure (RelNext whenFalse)
        Nothing -> forkAmong [continueOn cond whenTrue, continueOn (not cond) whenFalse]

    jumpTo :: PC -> (PC -> RelState) -> SymFC.Sym RelOutcome
    jumpTo target landing = do
      known <- concreteAddress target
      case (known, codeAddresses code) of
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

currentFetch :: RelState -> SymFC.Sym (Maybe (Fetched, Maybe Fetched))
currentFetch st = case relFetchHint st of
  Just hint -> pure (Just (hint, fetchedFollowing hint))
  Nothing -> traverse withFollowing (codeFetch (relCode st) (relPc st))
  where
    withFollowing here = do
      atPc <- knownBool (fetchedAddress here == relPc st)
      pure case atPc of
        Just True -> (here, fetchedFollowing here)
        _ -> (here, Nothing)

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
  | Leaks PC

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
  RelLeaks pc -> pure (Leaks pc)

followAll :: Fuel -> [RelOutcome] -> SymFC.Sym Verdict
followAll _ [] = pure NoLeak
followAll fuel (branch : rest) = do
  verdict <- follow fuel branch
  case verdict of
    Leaks {} -> pure verdict
    _ -> laterPathVerdict verdict <$> followAll fuel rest
