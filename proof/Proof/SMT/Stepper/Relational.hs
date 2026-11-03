{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.Relational
  ( expectValid
  , expectCounterexample
  , Probe (..)
  , freshProbe
  , Run (..)
  , valueIn
  , memoryIn
  , matchesState
  , sameWordAt
  , relStepMatchesIsaStep
  , leakOfReadsOnlyLeakOfReads
  , relStepMismatchFails
  , addOnSecretNoLeak
  , branchOnPublicNoLeak
  , storeSecretNoLeak
  , storeIntoCodeIsReported
  , branchOnSecretLeaks
  , loadFromSecretLeaks
  , secretThroughAddLeaks
  , storeToSecretAddressLeaks
  , jumpToSecretLeaks
  , loadSecretWordNoLeak
  , loadPublicWordBesideSecretNoLeak
  , branchOnSecretWordLeaks
  , publicBranchForksNoLeak
  , leakOnOnePathLeaks
  , leakOnInfeasiblePathNoLeak
  , provenAgreementDoesNotHideLeak
  , publicJumpToLeakyTargetLeaks
  , publicJumpAvoidingLeakyTargetNoLeak
  , unconstrainedPublicJumpLeaks
  , confinedJumpInLongCodeNoLeak
  ) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Core.ISA (IsaStateG (..), StepG (..), isaInstrAt, isaStep)
import Memory.Types (MemOps (..))
import Proof.Core.Leakage.Model (leakOf)
import Core.RegFile (RegFileOps (..))
import Stepper (Fuel (..), IsaSys)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.Memory (Memory)
import Stepper.Memory qualified as Memory
import Stepper.Rel (Rel (..), left, right)
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Word)
import Prelude hiding (Word)

expectValid :: Bool -> SymFC.Sym ()
expectValid prop = do
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.None -> pure ()
    _ -> SymFC.halt

expectCounterexample :: Bool -> SymFC.Sym ()
expectCounterexample prop = do
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.Instance -> pure ()
    _ -> SymFC.halt

data Probe = Probe
  { probeReg :: RegIdx
  , probeSlot :: RegSlot
  , probeAddr :: Address
  }

freshProbe :: SymFC.Sym Probe
freshProbe = do
  reg <- SymFC.fresh @RegIdx
  addr <- SymFC.fresh @Address
  slot <- slotFor reg
  pure (Probe reg slot addr)

data Run = FirstRun | SecondRun

valueIn :: Run -> Rel a -> a
valueIn FirstRun = left
valueIn SecondRun = right

memoryIn :: Run -> RelMem -> Memory 32 8
memoryIn FirstRun = leftMem
memoryIn SecondRun = rightMem

matchesState :: Run -> Probe -> RelState -> IsaSys -> Bool
matchesState run probe st isa =
  relPc st == isaPc isa
    && valueIn run (readSlot (relRegs st) (probeSlot probe)) == lookupRFg (probeReg probe) (isaRegFile isa)
    && Memory.read (probeAddr probe) (memoryIn run (relMem st)) == Memory.read (probeAddr probe) (isaMem isa)

matchesIsaStep :: Probe -> IsaSys -> RelOutcome -> Bool
matchesIsaStep = matchesIsaStepIn FirstRun

matchesIsaStepIn :: Run -> Probe -> IsaSys -> RelOutcome -> Bool
matchesIsaStepIn run probe isa outcome = case (outcome, isaStep isa) of
  (RelNext st, Next isa') -> matchesState run probe st isa'
  (RelFork branches, Next isa') ->
    someReachable branches && allReachableMatch run probe isa' branches
  (RelHalted, IsaHalted) -> True
  _ -> False

someReachable :: [RelOutcome] -> Bool
someReachable [] = False
someReachable (branch : rest) = reachableNext branch || someReachable rest

allReachableMatch :: Run -> Probe -> IsaSys -> [RelOutcome] -> Bool
allReachableMatch _ _ _ [] = True
allReachableMatch run probe isa (branch : rest) =
  reachableNextMatches run probe isa branch && allReachableMatch run probe isa rest

reachableNext :: RelOutcome -> Bool
reachableNext (RelNext st) = relPath st
reachableNext _ = False

reachableNextMatches :: Run -> Probe -> IsaSys -> RelOutcome -> Bool
reachableNextMatches run probe isa (RelNext st) = not (relPath st) || matchesState run probe st isa
reachableNextMatches _ _ _ _ = False

{-# ANN relStepMatchesIsaStep (SymFC ['stepperEmbeddings]) #-}
relStepMatchesIsaStep :: SymFC.Sym ()
relStepMatchesIsaStep = do
  isa <- SymFC.fresh @IsaSys
  probe <- freshProbe
  outcome <- relStep (sameState (codeFromMemory (isaMem isa)) isa)
  expectValid $ matchesIsaStep probe isa outcome

registerOf :: IsaSys -> RegIdx -> Word
registerOf isa reg = lookupRFg reg (isaRegFile isa)

sameWordAt :: IsaSys -> IsaSys -> Address -> Bool
sameWordAt isaA isaB addr = memReadWord addr (isaMem isaA) == memReadWord addr (isaMem isaB)

{-# ANN leakOfReadsOnlyLeakOfReads (SymFC ['stepperEmbeddings]) #-}
leakOfReadsOnlyLeakOfReads :: SymFC.Sym ()
leakOfReadsOnlyLeakOfReads = do
  isaA <- SymFC.fresh @IsaSys
  isaB <- SymFC.fresh @IsaSys
  let pc = isaPc isaA
      sameRegister reg = Identity (registerOf isaA reg == registerOf isaB reg)
      sameFollowing = Identity (sameWordAt isaA isaB (pc + 4))
      readsAgree = runIdentity (leakOfReadsAgree sameRegister sameFollowing (isaInstrAt isaA))
      sameInstruction = isaPc isaB == pc && sameWordAt isaA isaB pc
  expectValid $ not (sameInstruction && readsAgree) || leakOf isaA == leakOf isaB

{-# ANN relStepMismatchFails (SymFC ['stepperEmbeddings]) #-}
relStepMismatchFails :: SymFC.Sym ()
relStepMismatchFails = do
  isa <- SymFC.fresh @IsaSys
  probe <- freshProbe
  outcome <- relStep (sameState (codeFromMemory (isaMem isa)) isa {isaPc = isaPc isa + 4})
  expectCounterexample $ matchesIsaStep probe isa outcome

secretReg :: RegIdx
secretReg = 10

ebreak :: Word
ebreak = 0x00100073

toyFuel :: Fuel
toyFuel = More (More (More Done))

program :: Word -> Word -> Code
program first second =
  withInstruction 0 first $
    withInstruction 4 second $
      withInstruction 8 ebreak $
        withInstruction 12 ebreak noCode

secretAddr :: Address
secretAddr = 0

type Secret = Word -> Word -> RelState -> SymFC.Sym RelState

runToy :: Secret -> Word -> Word -> SymFC.Sym Verdict
runToy secret first second = do
  isa <- SymFC.fresh @IsaSys
  secretA <- SymFC.fresh @Word
  secretB <- SymFC.fresh @Word
  relRun toyFuel =<< secret secretA secretB (sameState (program first second) isa {isaPc = 0})

inSecretReg :: Secret
inSecretReg = withSecretReg secretReg

inSecretWord :: Secret
inSecretWord = withSecretWord secretAddr

expectNoLeak :: SymFC.Sym Verdict -> SymFC.Sym ()
expectNoLeak run = do
  verdict <- run
  case verdict of
    NoLeak -> pure ()
    _ -> SymFC.halt

expectLeakAt :: Address -> SymFC.Sym Verdict -> SymFC.Sym ()
expectLeakAt expectedPc run = do
  verdict <- run
  case verdict of
    Leaks pc | pc == expectedPc -> pure ()
    _ -> SymFC.halt

addA1A0A0 :: Word
addA1A0A0 = 0x00A505B3

addA2A1A1 :: Word
addA2A1A1 = 0x00B58633

beqX0X0Plus8 :: Word
beqX0X0Plus8 = 0x00000463

beqA0X0Plus8 :: Word
beqA0X0Plus8 = 0x00050463

beqA1X0Plus8 :: Word
beqA1X0Plus8 = 0x00058463

lwA1FromA0 :: Word
lwA1FromA0 = 0x00052583

lwA1FromZero :: Word
lwA1FromZero = 0x00002583

lwA1From64 :: Word
lwA1From64 = 0x04002583

lwA2FromA1 :: Word
lwA2FromA1 = 0x0005A603

swA0ToZero :: Word
swA0ToZero = 0x00A02023

swA0To64 :: Word
swA0To64 = 0x04A02023

swA1ToA0 :: Word
swA1ToA0 = 0x00B52023

jalrToA0 :: Word
jalrToA0 = 0x00050067

{-# ANN addOnSecretNoLeak (SymFC ['stepperEmbeddings]) #-}
addOnSecretNoLeak :: SymFC.Sym ()
addOnSecretNoLeak = expectNoLeak $ runToy inSecretReg addA1A0A0 ebreak

{-# ANN branchOnPublicNoLeak (SymFC ['stepperEmbeddings]) #-}
branchOnPublicNoLeak :: SymFC.Sym ()
branchOnPublicNoLeak = expectNoLeak $ runToy inSecretReg beqX0X0Plus8 ebreak

{-# ANN storeSecretNoLeak (SymFC ['stepperEmbeddings]) #-}
storeSecretNoLeak :: SymFC.Sym ()
storeSecretNoLeak = expectNoLeak $ runToy inSecretReg swA0To64 ebreak

{-# ANN storeIntoCodeIsReported (SymFC ['stepperEmbeddings]) #-}
storeIntoCodeIsReported :: SymFC.Sym ()
storeIntoCodeIsReported = do
  verdict <- runToy inSecretReg swA0ToZero ebreak
  case verdict of
    WritesCode 0 -> pure ()
    _ -> SymFC.halt

{-# ANN branchOnSecretLeaks (SymFC ['stepperEmbeddings]) #-}
branchOnSecretLeaks :: SymFC.Sym ()
branchOnSecretLeaks = expectLeakAt 0 $ runToy inSecretReg beqA0X0Plus8 ebreak

{-# ANN loadFromSecretLeaks (SymFC ['stepperEmbeddings]) #-}
loadFromSecretLeaks :: SymFC.Sym ()
loadFromSecretLeaks = expectLeakAt 0 $ runToy inSecretReg lwA1FromA0 ebreak

{-# ANN secretThroughAddLeaks (SymFC ['stepperEmbeddings]) #-}
secretThroughAddLeaks :: SymFC.Sym ()
secretThroughAddLeaks = expectLeakAt 4 $ runToy inSecretReg addA1A0A0 beqA1X0Plus8

{-# ANN storeToSecretAddressLeaks (SymFC ['stepperEmbeddings]) #-}
storeToSecretAddressLeaks :: SymFC.Sym ()
storeToSecretAddressLeaks = expectLeakAt 0 $ runToy inSecretReg swA1ToA0 ebreak

{-# ANN jumpToSecretLeaks (SymFC ['stepperEmbeddings]) #-}
jumpToSecretLeaks :: SymFC.Sym ()
jumpToSecretLeaks = expectLeakAt 0 $ runToy inSecretReg jalrToA0 ebreak

{-# ANN loadSecretWordNoLeak (SymFC ['stepperEmbeddings]) #-}
loadSecretWordNoLeak :: SymFC.Sym ()
loadSecretWordNoLeak = expectNoLeak $ runToy inSecretWord lwA1FromZero addA2A1A1

{-# ANN loadPublicWordBesideSecretNoLeak (SymFC ['stepperEmbeddings]) #-}
loadPublicWordBesideSecretNoLeak :: SymFC.Sym ()
loadPublicWordBesideSecretNoLeak = expectNoLeak $ runToy inSecretWord lwA1From64 lwA2FromA1

{-# ANN branchOnSecretWordLeaks (SymFC ['stepperEmbeddings]) #-}
branchOnSecretWordLeaks :: SymFC.Sym ()
branchOnSecretWordLeaks = expectLeakAt 4 $ runToy inSecretWord lwA1FromZero beqA1X0Plus8

bneA1X0Plus8 :: Word
bneA1X0Plus8 = 0x00059463

forkFuel :: Fuel
forkFuel = More (More (More (More (More (More Done)))))

runForkToy :: Secret -> Code -> SymFC.Sym Verdict
runForkToy secret code = do
  isa <- SymFC.fresh @IsaSys
  secretA <- SymFC.fresh @Word
  secretB <- SymFC.fresh @Word
  relRun forkFuel =<< secret secretA secretB (sameState code isa {isaPc = 0})

publicBranchProgram :: Code
publicBranchProgram =
  withInstruction 0 lwA1From64 $
    withInstruction 4 beqA1X0Plus8 $
      withInstruction 8 ebreak $
        withInstruction 12 ebreak noCode

secretBranchOnFallthroughProgram :: Code
secretBranchOnFallthroughProgram =
  withInstruction 0 lwA1From64 $
    withInstruction 4 beqA1X0Plus8 $
      withInstruction 8 beqA0X0Plus8 $
        withInstruction 12 ebreak $
          withInstruction 16 ebreak noCode

secretBranchBehindInfeasibleProgram :: Code
secretBranchBehindInfeasibleProgram =
  withInstruction 0 lwA1From64 $
    withInstruction 4 beqA1X0Plus8 $
      withInstruction 8 ebreak $
        withInstruction 12 bneA1X0Plus8 $
          withInstruction 16 ebreak $
            withInstruction 20 beqA0X0Plus8 $
              withInstruction 24 ebreak $
                withInstruction 28 ebreak noCode

{-# ANN publicBranchForksNoLeak (SymFC ['stepperEmbeddings]) #-}
publicBranchForksNoLeak :: SymFC.Sym ()
publicBranchForksNoLeak = expectNoLeak $ runForkToy inSecretReg publicBranchProgram

{-# ANN leakOnOnePathLeaks (SymFC ['stepperEmbeddings]) #-}
leakOnOnePathLeaks :: SymFC.Sym ()
leakOnOnePathLeaks = expectLeakAt 8 $ runForkToy inSecretReg secretBranchOnFallthroughProgram

{-# ANN leakOnInfeasiblePathNoLeak (SymFC ['stepperEmbeddings]) #-}
leakOnInfeasiblePathNoLeak :: SymFC.Sym ()
leakOnInfeasiblePathNoLeak = expectNoLeak $ runForkToy inSecretReg secretBranchBehindInfeasibleProgram

lwA3FromZero :: Word
lwA3FromZero = 0x00002683

beqA3X0Plus8 :: Word
beqA3X0Plus8 = 0x00068463

leakAfterProvenAgreementProgram :: Code
leakAfterProvenAgreementProgram =
  withInstruction 0 lwA1From64 $
    withInstruction 4 lwA2FromA1 $
      withInstruction 8 lwA3FromZero $
        withInstruction 12 beqA3X0Plus8 $
          withInstruction 16 ebreak $
            withInstruction 20 ebreak noCode

{-# ANN provenAgreementDoesNotHideLeak (SymFC ['stepperEmbeddings]) #-}
provenAgreementDoesNotHideLeak :: SymFC.Sym ()
provenAgreementDoesNotHideLeak = expectLeakAt 12 $ runForkToy inSecretWord leakAfterProvenAgreementProgram

jalrToA1 :: Word
jalrToA1 = 0x00058067

jumpTargetReg :: RegIdx
jumpTargetReg = 11

cleanTarget, leakyTarget, otherCleanTarget :: Word
cleanTarget = 8
leakyTarget = 12
otherCleanTarget = 16

publicJumpProgram :: Code
publicJumpProgram =
  withInstruction 0 jalrToA1 $
    withInstruction 4 ebreak $
      withInstruction 8 ebreak $
        withInstruction 12 beqA0X0Plus8 $
          withInstruction 16 ebreak $
            withInstruction 20 ebreak noCode

runPublicJumpToy :: (Word -> Bool) -> SymFC.Sym Verdict
runPublicJumpToy = runJumpIn publicJumpProgram

{-# ANN publicJumpToLeakyTargetLeaks (SymFC ['stepperEmbeddings]) #-}
publicJumpToLeakyTargetLeaks :: SymFC.Sym ()
publicJumpToLeakyTargetLeaks =
  expectLeakAt 12 $ runPublicJumpToy \target -> target == cleanTarget || target == leakyTarget

{-# ANN publicJumpAvoidingLeakyTargetNoLeak (SymFC ['stepperEmbeddings]) #-}
publicJumpAvoidingLeakyTargetNoLeak :: SymFC.Sym ()
publicJumpAvoidingLeakyTargetNoLeak =
  expectNoLeak $ runPublicJumpToy \target -> target == cleanTarget || target == otherCleanTarget

{-# ANN unconstrainedPublicJumpLeaks (SymFC ['stepperEmbeddings]) #-}
unconstrainedPublicJumpLeaks :: SymFC.Sym ()
unconstrainedPublicJumpLeaks = expectLeakAt 12 $ runPublicJumpToy (const True)

ebreaksFrom :: Address -> Fuel -> Code -> Code
ebreaksFrom _ Done code = code
ebreaksFrom addr (More count) code = withInstruction addr ebreak (ebreaksFrom (addr + 4) count code)

doubled :: Fuel -> Fuel
doubled Done = Done
doubled (More count) = More (More (doubled count))

sixtyFour :: Fuel
sixtyFour = doubled (doubled (doubled (doubled (doubled (doubled (More Done))))))

longCodeLeakyAddr :: Address
longCodeLeakyAddr = 260

firstLongTarget, secondLongTarget :: Word
firstLongTarget = 176
secondLongTarget = 180

longJumpProgram :: Code
longJumpProgram =
  withInstruction 0 jalrToA1 $
    ebreaksFrom 4 sixtyFour $
      withInstruction longCodeLeakyAddr beqA0X0Plus8 $
        ebreaksFrom (longCodeLeakyAddr + 4) sixtyFour noCode

runJumpIn :: Code -> (Word -> Bool) -> SymFC.Sym Verdict
runJumpIn code allowedTarget = do
  isa <- SymFC.fresh @IsaSys
  secretA <- SymFC.fresh @Word
  secretB <- SymFC.fresh @Word
  target <- SymFC.fresh @Word
  let start = sameState code isa {isaPc = 0}
  regs <- writeReg jumpTargetReg (Same target) (relRegs start)
  let withTarget = start {relPath = allowedTarget target, relRegs = regs}
  relRun forkFuel =<< inSecretReg secretA secretB withTarget

{-# ANN confinedJumpInLongCodeNoLeak (SymFC ['stepperEmbeddings]) #-}
confinedJumpInLongCodeNoLeak :: SymFC.Sym ()
confinedJumpInLongCodeNoLeak =
  expectNoLeak $ runJumpIn longJumpProgram \target ->
    target == firstLongTarget || target == secondLongTarget
