-- | The proof obligation itself, in one place.
--
-- Both the symbolic property ('Verify.indStep0') and the QuickCheck harness
-- ('ProofSpec') go through these definitions, so the two cannot drift apart.
-- They previously had separate copies of the premise and conclusion, and had
-- already diverged in the @IsaHalted@ branch without anyone noticing -- which
-- made it impossible to argue about why one found counterexamples the other
-- did not.
--
-- The only thing the two are allowed to differ in is how the system state is
-- built: symbolic scalars on one side, a generator on the other. That is the
-- input space, not the property.
module Obligation
  ( isaOfG,
    isaOfHop,
    indStepObligation,
    indStepObligation1,
    indStepObligation1At,
    indStepObligation1AtP,
    indStepObligation1AtWord,
    indStepObligation1AtWordCase,
    indStepObligation2,
    indStepObligation3,
    K2Conj (..),
    indStepObligation2AtWord,
    indStepObligation2AtWordCase,
    K2EmptyCase (..),
    premise2NonRunningEmptyAt,
    premise2StoreHazardNoMemEmpty,
    DecodeWordCase (..),
    decodeWordCaseHolds,
    decodeWordCasesExhaustive,
    decodedEnvWordIsSystem,
    HopCase (..),
    PreProfile (..),
    EmptyCase (..),
    premiseEmptyAt,
    isStartupShape,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Driver (driver, isEnvInstr, isJumpInstr, isMemInstr, storeHazard)
import ISAStep
import Instruction
import Invariant
import Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The architectural state the invariant claims @sys@ corresponds to.
--
-- Derived from @sys@ by the flush rather than taken as a further input: the
-- invariant's container equalities then hold by construction, so assuming the
-- invariant at one witness reduces to assuming its scalar conjuncts -- which is
-- the full container invariant, not a weakening of it.
isaOfG :: (RegFileOps r, MemOps m) => InvConfig -> SysG r m -> IsaStateG r m
isaOfG cfg (Sys st inp mem) =
  IsaState {isaPc = Core.stateExPc st, isaRegFile = frf, isaMem = fm}
  where
    (fm, frf) =
      flushMeStage
        (jumpsWriteRdInMe cfg)
        (Core.stateMeInstr st)
        (runIdentity (Core.stateMeRes st))
        (Core.stateMeAddr st)
        ( flushWbStage
            (Core.stateWbInstr st)
            (runIdentity (Core.stateWbRes st))
            (runIdentity (Core.inputMem inp))
            (mem, Core.stateRegFile st)
        )

-- | The k = 0 inductive step, at witness register @wr@ and witness byte
-- address @wa@:
--
-- > inv(a, c) /\ driver(c) = 0  ==>  inv(isaStep(a), stepSys(c))
--
-- The no-self-modifying-store side condition is /assumed/ of both the pre- and
-- post-state, which rules out transitions that create an aliasing store rather
-- than obliging us to show none can arise. See 'Invariant.noStoreAlias'.
indStepObligation ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG proposed sys
    sys' = stepSys sys

    premises =
      invAtFree proposed wr wa isa sys
        && driver sys == 0
        && noStoreAlias sys
        && noStoreAlias sys'

    conclusion =
      case isaStep isa of
        Next isa' -> invAtFree proposed wr wa isa' sys'
        -- driver == 0 excludes environment instructions, so the ISA cannot
        -- halt on this hop.
        IsaHalted -> True

-- | Does the pipeline have the shape the invariant's @startup@ case describes?
--
-- Only the pipeline registers are inspected, not the PC relation: this is used
-- to decide which /hop semantics/ apply, and the PC clause is the invariant's
-- business.
--
-- The distinction matters from @k = 1@ onwards. A startup hop brings the first
-- instruction into the execute stage without executing anything, so the
-- architectural state does /not/ advance across it -- exactly the @firstHop@
-- special case in @ProofSpec.invTrace@. Every other hop retires one
-- instruction and does advance it. At @k = 0@ the question could not arise,
-- because @driver@ sends every startup state to a two-cycle hop.
isStartupShape :: SysG r m -> Bool
isStartupShape (Sys st inp _) =
  Core.stateWbInstr st == Nop FirstCycle
    && Core.stateMeInstr st == Nop FirstCycle
    && Core.stateExInstr st == Nop FirstCycle
    && not (Core.inputIsInstr inp)

-- | The architectural state for a hop, startup included.
--
-- 'isaOfG' reads the architectural PC off the /execute/ stage, which is right
-- for a running state: the execute stage holds the instruction the ISA is
-- about to retire. A startup state has nothing in the pipe yet, and the
-- invariant's startup case correspondingly pins @isaPc@ to the /fetch/ stage.
-- Deriving it from @exPc@ there would produce an architectural state that no
-- case of the invariant admits, and the obligation would hold vacuously on
-- every startup state instead of saying anything about it.
--
-- The register file and memory need no special case: on a startup-shaped state
-- both flushes are the identity, since every stage holds @Nop FirstCycle@.
--
-- 'indStepObligation' ( @k = 0@ ) deliberately keeps using 'isaOfG': startup
-- states all have @driver == 1@, so the two agree wherever @k = 0@ applies,
-- and leaving that property's statement untouched means its proof still
-- stands.
isaOfHop :: (RegFileOps r, MemOps m) => InvConfig -> SysG r m -> IsaStateG r m
isaOfHop cfg sys
  | isStartupShape sys = (isaOfG cfg sys) {isaPc = Core.stateFePc (sysState sys)}
  | otherwise = isaOfG cfg sys

-- | The @k = 1@ inductive step: the driver's two-cycle hop.
--
-- Two things differ from 'indStepObligation' beyond the extra cycle.
--
-- First, the no-aliasing-store assumption has to cover the /intermediate/
-- state as well. A store sitting in the memory stage of @s1@ commits during
-- the @s1 -> s2@ step, so it can rewrite an instruction word mid-hop without
-- ever appearing in the memory stage of the pre- or post-state.
--
-- Second, the hop semantics are case-dependent: see 'isStartupShape'.
indStepObligation1 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation1 wr wa sys =
  not premises || conclusion
  where
    isa = isaOfHop proposed sys
    s1 = stepSys sys
    s2 = stepSys s1

    premises =
      invAtFree proposed wr wa isa sys
        && driver sys == 1
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2

    conclusion
      -- The startup hop does not retire an instruction, so the architectural
      -- state is carried across unchanged.
      | isStartupShape sys = invAtFree proposed wr wa isa s2
      | otherwise =
          case isaStep isa of
            Next isa' -> invAtFree proposed wr wa isa' s2
            -- driver == 1 excludes environment instructions (they route to a
            -- three-cycle hop), so the ISA cannot halt here either.
            IsaHalted -> True

-- | Which case of the invariant the pre-state is assumed to be in.
--
-- The @k = 1@ premise is a disjunction: a state reaches a two-cycle hop either
-- from startup or from a running state with a memory instruction in writeback.
-- The halted cases cannot arise -- a halted core has @driver == 0@ -- so these
-- two exhaust it.
data HopCase = HopStartup | HopRunning
  deriving (Eq, Show)

-- | The @k = 1@ inductive step, split by pre-state case and post-state
-- conjunct.
--
-- Logically, @'indStepObligation1' wr wa sys@ follows from the conjunction of
-- @'indStepObligation1At' hop c wr wa sys@ over both 'HopCase's and all four
-- 'RunConj's. See the note above 'Invariant.RunConj' for why the two
-- directions split differently -- the premise by distributing over the
-- disjunction, the conclusion by strengthening it to a single case first.
--
-- The conclusion here claims the post-state satisfies a RUNNING case
-- specifically, which is stronger than the disjunction 'invAtFree' asserts.
-- That is sound, and for a @k = 1@ hop it is also true: the hop lands with a
-- real instruction in the execute stage, so neither the startup nor a halted
-- case applies.
indStepObligation1At ::
  (RegFileOps r, MemOps m) =>
  HopCase -> RunConj -> RegIdx -> Address -> SysG r m -> Bool
indStepObligation1At = indStepObligation1AtP PreFull

-- | How much of the pre-state case to assume.
--
-- Splitting the CONCLUSION alone left the premise monolithic: every
-- 'HopRunning' obligation assumed the whole running case, decode tree and
-- memory flush included, however little of it the conclusion needed. That
-- showed up directly in the measurements -- the two 'HopStartup' properties,
-- whose premise is a handful of @Nop@ equalities and two point reads, came
-- back in about a second, while 'HopRunning' ones with a comparable conclusion
-- took minutes.
--
-- DROPPING a premise conjunct is sound as a proof strategy: it /strengthens/
-- the statement, since the obligation then has to hold on more states. The
-- strengthened statement can of course be false. QuickCheck is useful as a
-- cheap counterexample search, but it does not settle that question: in
-- particular, the generator can miss instructions outside the image of
-- 'decode''. Any profile used by the proof still has to be discharged
-- symbolically.
data PreProfile
  = -- | Assume the whole invariant case.
    PreFull
  | -- | Assume everything except @ex == decode (mem[isaPc])@. That conjunct is
    -- the one term relating the architectural memory to the pipeline through a
    -- full decode tree, so it is the expensive one to carry.
    --
    -- Note this does /not/ remove decoding from the query: 'Core.decode' calls
    -- @decode'@ on @inputMem@ every cycle, so the core's own decodes remain.
    -- What it removes is the correlation between the two arrays.
    PreNoDecode
  deriving (Eq, Show)

-- | 'indStepObligation1At', parameterised by how much of the premise to assume.
indStepObligation1AtP ::
  (RegFileOps r, MemOps m) =>
  PreProfile -> HopCase -> RunConj -> RegIdx -> Address -> SysG r m -> Bool
indStepObligation1AtP prof hop c wr wa sys =
  not premises || conclusion
  where
    isa = isaOfHop proposed sys
    s1 = stepSys sys
    s2 = stepSys s1

    -- 'checkExDecode' already expresses exactly this: setting it False makes
    -- the decode conjunct trivially true wherever the invariant is used.
    preCfg = case prof of
      PreFull -> proposed
      PreNoDecode -> proposed {checkExDecode = False}

    -- Each branch assumes ONE case of the invariant, so no disjunction
    -- survives into the query.
    preCase = case hop of
      HopStartup -> isStartupShape sys && startupCaseAt preCfg wr wa isa sys
      HopRunning -> not (isStartupShape sys) && runningCasesAt preCfg wr wa isa sys

    premises =
      preCase
        && driver sys == 1
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2

    -- The conclusion always uses the full configuration: weakening it would
    -- weaken the theorem, which is the opposite of what dropping a premise
    -- conjunct does.
    conclusion = case hop of
      -- The startup hop retires nothing, so the architectural state carries
      -- across unchanged.
      HopStartup -> runningConjAt proposed c wr wa isa s2
      -- The full pre-state invariant says exactly that @exInstr sys@ is the
      -- instruction obtained by decoding the ISA memory at @isaPc@. Execute
      -- that already-decoded instruction here instead of calling 'isaStep',
      -- which would decode the same word again inside the next-state
      -- computation. This is a semantics-preserving rewrite under 'PreFull'
      -- and avoids the nested decode tree that dominates the SMT query.
      --
      -- 'PreNoDecode' is only an exploratory strengthening and lacks the
      -- equality that justifies the rewrite, so keep its original semantics.
      HopRunning -> case archStep of
        Next isa' -> runningConjAt proposed c wr wa isa' s2
        IsaHalted -> True

    archStep = case prof of
      PreFull -> isaStepDecoded (exInstr sys) isa
      PreNoDecode -> isaStep isa

-- | A running @k = 1@ obligation parameterised by the instruction word at the
-- architectural PC.
--
-- This is the same statement as
--
-- > indStepObligation1At HopRunning c wr wa sys
--
-- when @iw = memReadWord (isaPc isa) (isaMem isa)@.  Its premise factors the
-- expensive decode equality into:
--
-- > exInstr sys == decode' iw
-- > memReadWord (isaPc isa) (isaMem isa) == iw
--
-- The symbolic harness constructs @exInstr sys@ as @decode' iw@, so the first
-- equality disappears by reduction. The remaining array constraint is a plain
-- word equality; the ISA transition and the core both consume the shared
-- decoded instruction instead of asking the solver to invert an equality
-- between an arbitrary 'Instruction' ADT and a decoder tree.
indStepObligation1AtWord ::
  (RegFileOps r, MemOps m) =>
  RunConj -> Word -> RegIdx -> Address -> SysG r m -> Bool
indStepObligation1AtWord c iw wr wa sys =
  not premises || conclusion
  where
    isa = isaOfHop proposed sys
    s1 = stepSys sys
    s2 = stepSys s1
    noDecode = proposed {checkExDecode = False}
    decoded = decode' iw

    premises =
      not (isStartupShape sys)
        && runningCasesAt noDecode wr wa isa sys
        && exInstr sys == decoded
        && memReadWord (isaPc isa) (isaMem isa) == iw
        && driver sys == 1
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2

    conclusion =
      case isaStepDecoded decoded isa of
        Next isa' -> runningConjAt proposed c wr wa isa' s2
        IsaHalted -> True

-- | A top-level RISC-V opcode case. Keeping this separate from the full
-- 'Instruction' constructors is intentional: malformed encodings within a
-- recognised opcode still decode to @Nop DecodeFail@ and must remain covered.
data DecodeWordCase
  = WordR
  | WordIArith
  | WordLoad
  | WordJalr
  | WordSystem
  | WordStore
  | WordBranch
  | WordLui
  | WordAuipc
  | WordJal
  | WordOther
  deriving (Eq, Show)

-- | Does a word belong to one static top-level opcode case?
decodeWordCaseHolds :: DecodeWordCase -> Word -> Bool
decodeWordCaseHolds wc iw =
  case wc of
    WordR -> op == 0b011_0011
    WordIArith -> op == 0b001_0011
    WordLoad -> op == 0b000_0011
    WordJalr -> op == 0b110_0111
    WordSystem -> op == 0b111_0011
    WordStore -> op == 0b010_0011
    WordBranch -> op == 0b110_0011
    WordLui -> op == 0b011_0111
    WordAuipc -> op == 0b001_0111
    WordJal -> op == 0b110_1111
    WordOther ->
      op /= 0b011_0011
        && op /= 0b001_0011
        && op /= 0b000_0011
        && op /= 0b110_0111
        && op /= 0b111_0011
        && op /= 0b010_0011
        && op /= 0b110_0011
        && op /= 0b011_0111
        && op /= 0b001_0111
        && op /= 0b110_1111
  where
    op = slice d6 d0 iw

-- | The opcode split used by the symbolic harness is exhaustive. Written
-- without lists or folds so Pantomime can discharge it directly.
decodeWordCasesExhaustive :: Word -> Bool
decodeWordCasesExhaustive iw =
  decodeWordCaseHolds WordR iw
    || decodeWordCaseHolds WordIArith iw
    || decodeWordCaseHolds WordLoad iw
    || decodeWordCaseHolds WordJalr iw
    || decodeWordCaseHolds WordSystem iw
    || decodeWordCaseHolds WordStore iw
    || decodeWordCaseHolds WordBranch iw
    || decodeWordCaseHolds WordLui iw
    || decodeWordCaseHolds WordAuipc iw
    || decodeWordCaseHolds WordJal iw
    || decodeWordCaseHolds WordOther iw

-- | Every word decoded as @ecall@ or @ebreak@ has the SYSTEM opcode.
--
-- This small decoder fact lets the @k = 2@ halted obligations specialise to
-- 'WordSystem'; all other opcode cases are vacuous for a halted ISA step.
decodedEnvWordIsSystem :: Word -> Bool
decodedEnvWordIsSystem iw =
  not (isCall decoded || isBreak decoded)
    || decodeWordCaseHolds WordSystem iw
  where
    decoded = decode' iw

-- | One opcode-specialised piece of 'indStepObligation1AtWord'. The
-- conjunction of this property over all 'DecodeWordCase's implies the unsplit
-- word obligation by 'decodeWordCasesExhaustive'.
indStepObligation1AtWordCase ::
  (RegFileOps r, MemOps m) =>
  DecodeWordCase ->
  RunConj ->
  Word ->
  RegIdx ->
  Address ->
  SysG r m ->
  Bool
indStepObligation1AtWordCase wc c iw wr wa sys =
  not (decodeWordCaseHolds wc iw)
    || indStepObligation1AtWord c iw wr wa sys

-- k = 2 ----------------------------------------------------------------------

-- | The driver's three-cycle inductive step.
--
-- Unlike @k = 0@ and @k = 1@, this hop can execute an environment
-- instruction. The architectural state then stays at the trapping instruction
-- while the core reaches one of the two halted invariant cases.
indStepObligation2 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation2 wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG proposed sys
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2

    premises =
      invAtFree proposed wr wa isa sys
        && driver sys == 2
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2
        && noStoreAlias s3

    conclusion =
      case isaStep isa of
        Next isa' -> invAtFree proposed wr wa isa' s3
        IsaHalted -> invAtFree proposed wr wa isa s3

-- k = 3 ----------------------------------------------------------------------

-- | The driver's four-cycle inductive step, the longest hop the table produces.
--
-- Reached by a store hazard with a memory instruction in execute, by a load
-- hazard, and by the steady case with memory instructions in all three stages.
--
-- Stated exactly as 'indStepObligation2', one cycle longer. The @IsaHalted@
-- alternative is kept even though this hop should not be able to trap -- the
-- driver routes environment instructions to a three-cycle hop -- because
-- covering it costs nothing and assuming it away would be another unchecked
-- side argument of the kind that has already gone wrong twice here.
indStepObligation3 ::
  (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
indStepObligation3 wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG proposed sys
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2
    s4 = stepSys s3

    premises =
      invAtFree proposed wr wa isa sys
        && driver sys == 3
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2
        && noStoreAlias s3
        && noStoreAlias s4

    conclusion =
      case isaStep isa of
        Next isa' -> invAtFree proposed wr wa isa' s4
        IsaHalted -> invAtFree proposed wr wa isa s4

-- | One post-state group for a @k = 2@ obligation.
--
-- The running and halted alternatives are conditional on the result of the
-- ISA step. Taking the conjunction over all constructors proves the relevant
-- post-state case without retaining a large disjunction inside each query.
data K2Conj
  = K2Run RunConj
  | K2Halt HaltKind HaltConj
  deriving (Eq, Show)

-- | Fresh-instruction-word form of the running pre-state @k = 2@ obligation.
--
-- @driver == 2@ rules out startup and already-halted invariant cases (proved
-- separately by 'premise2NonRunningEmptyAt'), so only the running pre-state
-- needs the decoder-friendly encoding. The symbolic harness constructs the
-- execute instruction as @decode' iw@ and the architectural transition
-- consumes that same decoded value.
indStepObligation2AtWord ::
  (RegFileOps r, MemOps m) =>
  K2Conj -> Word -> RegIdx -> Address -> SysG r m -> Bool
indStepObligation2AtWord c iw wr wa sys =
  not premises || conclusion
  where
    isa = isaOfG proposed sys
    s1 = stepSys sys
    s2 = stepSys s1
    s3 = stepSys s2
    noDecode = proposed {checkExDecode = False}
    decoded = decode' iw

    premises =
      runningCasesAt noDecode wr wa isa sys
        && exInstr sys == decoded
        && memReadWord (isaPc isa) (isaMem isa) == iw
        && driver sys == 2
        && noStoreAlias sys
        && noStoreAlias s1
        && noStoreAlias s2
        && noStoreAlias s3

    conclusion =
      case isaStepDecoded decoded isa of
        Next isa' -> case c of
          K2Run rc -> runningConjAt proposed rc wr wa isa' s3
          K2Halt _ _ -> True
        IsaHalted -> case c of
          K2Run _ -> True
          K2Halt kind hc ->
            not (decodedHasKind kind decoded)
              || haltedConjAt proposed kind hc wr wa isa s3

    decodedHasKind kind = case kind of
      HaltBreak -> isBreak
      HaltCall -> isCall

-- | One opcode-specialised piece of 'indStepObligation2AtWord'.
indStepObligation2AtWordCase ::
  (RegFileOps r, MemOps m) =>
  DecodeWordCase ->
  K2Conj ->
  Word ->
  RegIdx ->
  Address ->
  SysG r m ->
  Bool
indStepObligation2AtWordCase wc c iw wr wa sys =
  not (decodeWordCaseHolds wc iw)
    || indStepObligation2AtWord c iw wr wa sys

-- | Startup and halted invariant cases cannot have @driver == 2@.
--
-- This closes the cases omitted by the fresh-word obligation above. It is
-- deliberately one cheap property per invariant case rather than an informal
-- side argument.
data K2EmptyCase
  = K2EmptyStartup
  | K2EmptyHaltedBreak
  | K2EmptyHaltedCall
  deriving (Eq, Show)

premise2NonRunningEmptyAt ::
  (RegFileOps r, MemOps m) =>
  K2EmptyCase -> RegIdx -> Address -> SysG r m -> Bool
premise2NonRunningEmptyAt ec wr wa sys =
  not (caseHolds && driver sys == 2)
  where
    isa = isaOfHop proposed sys
    caseHolds = case ec of
      K2EmptyStartup ->
        startupCaseAt proposed wr wa isa sys
      K2EmptyHaltedBreak ->
        haltedCaseAt proposed HaltBreak wr wa isa sys
      K2EmptyHaltedCall ->
        haltedCaseAt proposed HaltCall wr wa isa sys

-- | The driver's @storeHazard/nomem@ case is inconsistent with the
-- no-self-modifying-code assumption used by the theorem.
--
-- Once environment instructions and taken jumps (the earlier table rows) are
-- excluded, a non-memory execute instruction cannot itself be the store.
-- 'storeHazard' must therefore come from a memory-stage store exactly at
-- @dePc@, which 'noStoreAlias' rejects.
premise2StoreHazardNoMemEmpty ::
  (RegFileOps r) => SysG r m -> Bool
premise2StoreHazardNoMemEmpty sys =
  not
    ( running sys
        && not (isEnvInstr (exInstr sys))
        && not (isJumpInstr sys)
        && storeHazard sys
        && not (isMemInstr (exInstr sys))
        && noStoreAlias sys
    )

-- Premise cases claimed to be empty -------------------------------------------
--
-- Case-splitting the k = 1 obligation on the invariant's four cases and on
-- 'isStartupShape' yields eight obligations, of which 'indStepObligation1At'
-- discharges two. The other six were justified in prose: their premises are
-- contradictory, so they hold vacuously.
--
-- Prose is not a proof, and it is exactly the sort of side-argument that has
-- already gone wrong twice here (the 'isaOfG' vacuity, and 'noStoreAlias'
-- ignoring wraparound). Worse, QuickCheck cannot catch a mistake in them:
-- 'ProofSpec.genArbSys1' never produces a halted state, so a generator-based
-- check stays green whatever the halted cases do.
--
-- So each claim gets stated as an obligation instead. @premise ==> False@ IS
-- the claim that the premise is unsatisfiable, and if the claim is right the
-- solver should see it almost immediately -- these are the cheapest properties
-- in the set, not the most expensive.
--
-- If one of them turns out to be satisfiable, that is not a disaster: it means
-- the corresponding real obligation has to be discharged after all, and we
-- learn which one before trusting a green run that silently skipped it.
data EmptyCase
  = -- | A running state that is also startup-shaped. Empty while the decode
    -- conjunct is assumed -- @ex == decode' (mem[isaPc])@ and @Nop
    -- FirstCycle@ is not in the image of 'decode'' -- but NOT empty under
    -- 'PreNoDecode', which is why that profile needs this checked rather than
    -- assumed.
    EmptyRunStartup
  | -- | A startup state that is not startup-shaped. Empty by definition of the
    -- startup case, which pins all three stages to @Nop FirstCycle@.
    EmptyStartupRunning
  | -- | A halted-on-ebreak state with @driver == 1@. Empty because a halted
    -- core fails 'running', and 'driver' returns 0 for it.
    EmptyHaltedBreak
  | -- | Likewise for @ecall@.
    EmptyHaltedCall
  deriving (Eq, Show)

-- | The claim that a premise case cannot arise at @driver == 1@.
--
-- Stated as @not premise@, i.e. @premise ==> False@.
premiseEmptyAt ::
  (RegFileOps r, MemOps m) =>
  PreProfile -> EmptyCase -> RegIdx -> Address -> SysG r m -> Bool
premiseEmptyAt prof ec wr wa sys = not premise
  where
    isa = isaOfHop proposed sys
    preCfg = case prof of
      PreFull -> proposed
      PreNoDecode -> proposed {checkExDecode = False}

    caseHolds = case ec of
      EmptyRunStartup -> isStartupShape sys && runningCasesAt preCfg wr wa isa sys
      EmptyStartupRunning -> not (isStartupShape sys) && startupCaseAt preCfg wr wa isa sys
      EmptyHaltedBreak -> haltedCaseAt preCfg HaltBreak wr wa isa sys
      EmptyHaltedCall -> haltedCaseAt preCfg HaltCall wr wa isa sys

    premise = caseHolds && driver sys == 1
