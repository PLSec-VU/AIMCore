{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}

module Proof.SMT.Stepper.Invariant
  ( relStepKeepsInvariant
  , relStepKeepsInvariantAtLastWord
  , relStepKeepsInvariantWithoutHint
  , relStepWrongHintFails
  , relStepSwappedRunsFails
  ) where

import Core.ISA (IsaStateG (..), PC, StepG (..), isaStep)
import Memory.Types (MemOps (..))
import Proof.SMT.Stepper.Relational
import Core.RegFile (RegFileOps (..))
import Stepper (IsaSys)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.Memory (Memory)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Size (..), Word)
import Prelude hiding (Word)
import Proof.Core.Leakage.Model (Leak (..), leakOf)

codeAddrs :: [Address]
codeAddrs = [0, 4, 8]

-- | A fold over a literal list becomes 'elem', which SymFC cannot unfold.
everyCodeAddr :: (Address -> Bool) -> Bool
everyCodeAddr holds = holds 0 && holds 4 && holds 8

middlePc :: PC
middlePc = 4

lastPc :: PC
lastPc = 8

-- | Where a reached state stands and how it fetches: through a hint, as after
-- a fall-through, or through the code table, as after a jump.
data Start = Start
  { startPc :: PC
  , startHint :: Code -> Maybe Fetched
  }

hintedAt :: PC -> Start
hintedAt pc = Start pc (`codeFetch` pc)

unhintedAt :: PC -> Start
unhintedAt pc = Start pc (const Nothing)

storedWordAddr :: Address
storedWordAddr = 0x100

storedByteAddr :: Address
storedByteAddr = 0x105

fetchFrom :: Memory 32 8 -> Address -> Maybe Fetched -> Fetched
fetchFrom mem addr following = (fetchedFromMemory addr (memReadWord addr mem)) {fetchedFollowing = following}

linkedCode :: Memory 32 8 -> Code
linkedCode mem =
  Code
    { codeFetch = \pc -> if pc == 0 then Just atZero else if pc == 4 then Just atFour else if pc == 8 then Just atEight else Nothing
    , codeAddresses = KnownAddresses codeAddrs
    , codeSpan = Just (0, 12)
    }
  where
    atEight = fetchFrom mem 8 Nothing
    atFour = fetchFrom mem 4 (Just atEight)
    atZero = fetchFrom mem 0 (Just atFour)

-- | Two ISA states and a 'RelState' that stands for them, in the shape a
-- state has after some steps: a fetch hint, a register written through an
-- unknown index, pending whole-word and byte writes, and a path condition.
data Reached = Reached
  { reachedFirst :: IsaSys
  , reachedSecond :: IsaSys
  , reachedState :: RelState
  }

data History = History
  { historyReg :: RegIdx
  , historyRegValues :: Rel Word
  , historyWords :: Rel Word
  , historyBytes :: Rel Word
  }

freshHistory :: SymFC.Sym History
freshHistory = do
  reg <- SymFC.fresh @RegIdx
  regValues <- Pair <$> SymFC.fresh @Word <*> SymFC.fresh @Word
  storedWords <- Pair <$> SymFC.fresh @Word <*> SymFC.fresh @Word
  bytes <- Pair <$> SymFC.fresh @Word <*> SymFC.fresh @Word
  pure (History reg regValues storedWords bytes)

replayOn :: PC -> Run -> History -> IsaSys -> IsaSys
replayOn pc run history isa =
  isa
    { isaPc = pc
    , isaRegFile = modifyRFg (historyReg history) (valueIn run (historyRegValues history)) (isaRegFile isa)
    , isaMem =
        memWrite storedByteAddr Byte (valueIn run (historyBytes history)) $
          memWrite storedWordAddr Word (valueIn run (historyWords history)) (isaMem isa)
    }

replayOnState :: History -> RelState -> SymFC.Sym RelState
replayOnState history st = do
  slot <- slotFor (historyReg history)
  mem <-
    writeMem (relPath st) Word storedWordAddr (historyWords history) (relMem st)
      >>= writeMem (relPath st) Byte storedByteAddr (historyBytes history)
  pure st {relRegs = writeSlot slot (historyRegValues history) (relRegs st), relMem = mem}

freshReached :: Start -> SymFC.Sym Reached
freshReached (Start pc hintFrom) = do
  first0 <- SymFC.fresh @IsaSys
  second0 <- SymFC.fresh @IsaSys
  path <- SymFC.fresh @Bool
  history <- freshHistory
  let first = replayOn pc FirstRun history first0
      second = replayOn pc SecondRun history second0
      code = linkedCode (isaMem first)
      start = (pairState code first0 second0) {relPath = path && checkable code first second, relPc = pc, relFetchHint = hintFrom code}
  st <- replayOnState history start
  pure (Reached first second st)

storesIntoCode :: Code -> IsaSys -> Bool
storesIntoCode code isa = case leakOf isa of
  LStore size addr -> writesCode code size addr
  _ -> False

-- | The steps the checker must follow rather than stop at: both runs hold the
-- same code and leak the same, and the step leaves the code alone.
checkable :: Code -> IsaSys -> IsaSys -> Bool
checkable code first second =
  everyCodeAddr (sameWordAt first second)
    && leakOf first == leakOf second
    && not (storesIntoCode code first)

codeIntact :: Run -> Code -> RelState -> Bool
codeIntact run code st = everyCodeAddr \addr -> maybe True (fetchIntact mem addr) (codeFetch code addr)
  where
    mem = memoryIn run (relMem st)

fetchIntact :: Memory 32 8 -> Address -> Fetched -> Bool
fetchIntact mem pc f = fetchedAddress f == pc && fetchedWord f == memReadWord pc mem

hintIntact :: Run -> RelState -> Bool
hintIntact run st = case relFetchHint st of
  Nothing -> True
  Just hint -> fetchIntact mem pc hint && maybe True (fetchIntact mem (pc + 4)) (fetchedFollowing hint)
  where
    mem = memoryIn run (relMem st)
    pc = relPc st

keepsInvariant :: Run -> Probe -> Code -> RelState -> IsaSys -> Bool
keepsInvariant run probe code st isa =
  matchesState run probe st isa && codeIntact run code st && hintIntact run st

outsideCode :: PC -> Bool
outsideCode pc = everyCodeAddr (pc /=)

anyBranch :: (RelOutcome -> Bool) -> [RelOutcome] -> Bool
anyBranch _ [] = False
anyBranch holds (branch : rest) = holds branch || anyBranch holds rest

everyBranch :: (RelOutcome -> Bool) -> [RelOutcome] -> Bool
everyBranch _ [] = True
everyBranch holds (branch : rest) = holds branch && everyBranch holds rest

followsRun :: Run -> Probe -> IsaSys -> RelOutcome -> Bool
followsRun run probe isa outcome = case outcome of
  RelNext st -> relPath st && matchesState run probe st isa
  RelFork branches -> anyBranch (followsRun run probe isa) branches
  RelOutsideCode -> outsideCode (isaPc isa)
  _ -> False

everyBranchKeeps :: Run -> Probe -> Code -> IsaSys -> RelOutcome -> Bool
everyBranchKeeps run probe code isa outcome = case outcome of
  RelNext st -> not (relPath st) || keepsInvariant run probe code st isa
  RelFork branches -> everyBranch (everyBranchKeeps run probe code isa) branches
  RelOutsideCode -> True
  _ -> False

stepKeepsInvariant :: Run -> Probe -> Code -> IsaSys -> RelOutcome -> Bool
stepKeepsInvariant run probe code isa outcome = case (outcome, isaStep isa) of
  (RelHalted, IsaHalted) -> True
  (_, Next isa') -> followsRun run probe isa' outcome && everyBranchKeeps run probe code isa' outcome
  _ -> False

bothRunsKeepInvariant :: (Reached -> (IsaSys, IsaSys)) -> Probe -> Reached -> RelOutcome -> Bool
bothRunsKeepInvariant runsOf probe reached outcome =
  stepKeepsInvariant FirstRun probe code first outcome
    && stepKeepsInvariant SecondRun probe code second outcome
  where
    code = relCode (reachedState reached)
    (first, second) = runsOf reached

inOrder :: Reached -> (IsaSys, IsaSys)
inOrder reached = (reachedFirst reached, reachedSecond reached)

swapped :: Reached -> (IsaSys, IsaSys)
swapped reached = (reachedSecond reached, reachedFirst reached)

checkFromReached :: Start -> SymFC.Sym Bool
checkFromReached = checkRunsFromReached inOrder

checkRunsFromReached :: (Reached -> (IsaSys, IsaSys)) -> Start -> SymFC.Sym Bool
checkRunsFromReached runsOf start = do
  reached <- freshReached start
  probe <- freshProbe
  outcome <- relStep (reachedState reached)
  pure $ not (relPath (reachedState reached)) || bothRunsKeepInvariant runsOf probe reached outcome

{-# ANN relStepKeepsInvariant (SymFC ['stepperEmbeddings]) #-}
relStepKeepsInvariant :: SymFC.Sym ()
relStepKeepsInvariant = expectValid =<< checkFromReached (hintedAt middlePc)

{-# ANN relStepKeepsInvariantAtLastWord (SymFC ['stepperEmbeddings]) #-}
relStepKeepsInvariantAtLastWord :: SymFC.Sym ()
relStepKeepsInvariantAtLastWord = expectValid =<< checkFromReached (hintedAt lastPc)

{-# ANN relStepKeepsInvariantWithoutHint (SymFC ['stepperEmbeddings]) #-}
relStepKeepsInvariantWithoutHint :: SymFC.Sym ()
relStepKeepsInvariantWithoutHint = expectValid =<< checkFromReached (unhintedAt middlePc)

{-# ANN relStepWrongHintFails (SymFC ['stepperEmbeddings]) #-}
relStepWrongHintFails :: SymFC.Sym ()
relStepWrongHintFails = expectCounterexample =<< checkFromReached (Start middlePc wrongHint)
  where
    wrongHint code = (\atEight -> atEight {fetchedAddress = middlePc}) <$> codeFetch code 8

{-# ANN relStepSwappedRunsFails (SymFC ['stepperEmbeddings]) #-}
relStepSwappedRunsFails :: SymFC.Sym ()
relStepSwappedRunsFails = expectCounterexample =<< checkRunsFromReached swapped (hintedAt middlePc)
