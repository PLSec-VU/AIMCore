-- | The @k = 1@ inductive step, decomposed into independent obligations.
--
-- WHY. The monolithic @k = 1@ query ('VerifyK1.indStep1') does not scale: Z3
-- ran nearly four hours of CPU on it without terminating, and a later attempt
-- died at over 450MB of solver memory. The symbolic executor had finished in
-- minutes both times, so the cost is the SMT query, not path explosion -- which
-- means the main lever is the shape of the statement and of its symbolic
-- inputs. In particular, the running obligations below construct execute from
-- a fresh instruction word and split the largest remaining query by opcode.
--
-- HOW. See the note above 'Invariant.RunConj'. In short:
--
--   * the premise is a disjunction over the invariant's cases, and an
--     implication out of a disjunction splits into one obligation per case;
--   * the conclusion is a disjunction too, which cannot be split -- but it can
--     be strengthened to a single named case, and once it names one case it is
--     a conjunction, which does split.
--
-- So one large query becomes eight small independent ones: two pre-state cases
-- ('Obligation.HopCase') by four conclusion conjunct groups
-- ('Invariant.RunConj'). Their conjunction implies 'VerifyK1.indStep1'; the
-- test @\"k=1 split obligations hold, and imply the unsplit one\"@ in
-- @ProofSpec@ checks that implication on generated states, so the
-- decomposition cannot quietly stop covering the thing it replaces.
--
-- ORDER. Properties are checked in the order 'results' lists them, so they run
-- cheapest-first: the scalar conjuncts, then the register-file half of the
-- flush, then its memory half, then the decode tree. A stall therefore says
-- which conjunct is expensive rather than just that the hop is.
--
-- COST NOTE. Each property re-executes the two cycles symbolically, so GHC
-- time is paid eight times over. That is the deliberate trade: executor time
-- is minutes and predictable, solver time on the monolith was hours and was
-- not terminating.
module VerifyK1Split
  ( k1StartStruct,
    k1StartFlushRf,
    k1StartFlushMem,
    k1StartDecode,
    k1EmptyRunStartup,
    k1EmptyStartupRunning,
    k1EmptyHaltedBreak,
    k1EmptyHaltedCall,
    k1WordCasesExhaustive,
    k1RunStruct,
    k1RunFlushRfR,
    k1RunFlushRfIArith,
    k1RunFlushRfLoad,
    k1RunFlushRfJalr,
    k1RunFlushRfSystem,
    k1RunFlushRfStore,
    k1RunFlushRfBranch,
    k1RunFlushRfLui,
    k1RunFlushRfAuipc,
    k1RunFlushRfJal,
    k1RunFlushRfOther,
    k1RunFlushMem,
    k1RunDecode,
    results,
  )
where

import ArrayRF
import Axioms (arrayAxioms)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Instruction
import Invariant (RunConj (..))
import Machine
import Obligation (DecodeWordCase (..), EmptyCase (..), HopCase (..), PreProfile (..), decodeWordCasesExhaustive, indStepObligation1At, indStepObligation1AtWord, indStepObligation1AtWordCase, premiseEmptyAt)
import Pantomime (Theory (..), pantomime)
import qualified Pantomime.BuiltIn as Pantomime
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The pipeline registers as plain scalars: an ADT of scalars can be a
-- symbolic argument, a record containing a function cannot.
data K1State = K1State
  { k1FePc :: Address,
    k1DePc :: Address,
    k1ExPc :: Address,
    k1ExIr :: Instruction,
    k1MeIr :: Instruction,
    k1MeRes :: Word,
    k1MeAddr :: Address,
    k1WbIr :: Instruction,
    k1WbRes :: Word,
    k1Ctrl :: Core.Control Identity,
    k1Halt :: Maybe Core.HaltState,
    k1HaltPending :: Maybe Core.HaltState
  }

-- | Running-state scalars, deliberately without an execute-stage
-- 'Instruction'. The running properties receive a fresh symbolic instruction
-- word and construct that stage as @decode' iw@, so an arbitrary instruction
-- outside the decoder's image never enters the SMT query.
data K1RunState = K1RunState
  { k1rFePc :: Address,
    k1rDePc :: Address,
    k1rExPc :: Address,
    k1rMeIr :: Instruction,
    k1rMeRes :: Word,
    k1rMeAddr :: Address,
    k1rWbIr :: Instruction,
    k1rWbRes :: Word,
    k1rCtrl :: Core.Control Identity,
    k1rHalt :: Maybe Core.HaltState,
    k1rHaltPending :: Maybe Core.HaltState
  }

sysOf :: K1State -> Core.Input Identity -> RegArr -> MemArr -> SysG RegArrF MemArr
sysOf ss i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = k1FePc ss,
            Core.stateDePc = k1DePc ss,
            Core.stateExPc = k1ExPc ss,
            Core.stateExInstr = k1ExIr ss,
            Core.stateMeInstr = k1MeIr ss,
            Core.stateMeRes = Identity (k1MeRes ss),
            Core.stateMeAddr = k1MeAddr ss,
            Core.stateWbInstr = k1WbIr ss,
            Core.stateWbRes = Identity (k1WbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = k1Ctrl ss,
            Core.stateHalt = k1Halt ss,
            Core.stateHaltPending = k1HaltPending ss
          },
      sysInput = i,
      sysMem = ma
    }

sysOfRun ::
  K1RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  SysG RegArrF MemArr
sysOfRun ss iw i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = k1rFePc ss,
            Core.stateDePc = k1rDePc ss,
            Core.stateExPc = k1rExPc ss,
            Core.stateExInstr = decode' iw,
            Core.stateMeInstr = k1rMeIr ss,
            Core.stateMeRes = Identity (k1rMeRes ss),
            Core.stateMeAddr = k1rMeAddr ss,
            Core.stateWbInstr = k1rWbIr ss,
            Core.stateWbRes = Identity (k1rWbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = k1rCtrl ss,
            Core.stateHalt = k1rHalt ss,
            Core.stateHaltPending = k1rHaltPending ss
          },
      sysInput = i,
      sysMem = ma
    }

-- | One split obligation, as a Pantomime property.
prop ::
  HopCase ->
  RunConj ->
  K1State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
prop hop c ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation1At hop c wr wa (sysOf ss i ra ma)

-- | A running split obligation with a shared fresh instruction word.
runProp ::
  RunConj ->
  K1RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runProp c ss iw i ra ma wr wa =
  Pantomime.boolean $
    indStepObligation1AtWord c iw wr wa (sysOfRun ss iw i ra ma)

runCaseProp ::
  DecodeWordCase ->
  RunConj ->
  K1RunState ->
  Word ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
runCaseProp wc c ss iw i ra ma wr wa =
  Pantomime.boolean $
    indStepObligation1AtWordCase wc c iw wr wa (sysOfRun ss iw i ra ma)

-- | An emptiness claim, as a Pantomime property: @premise ==> False@.
emptyProp ::
  EmptyCase ->
  K1State ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
emptyProp ec ss i ra ma wr wa =
  Pantomime.boolean $ premiseEmptyAt PreFull ec wr wa (sysOf ss i ra ma)

-- The obligations, in the order 'results' checks them.
--
-- MEASURED (run of 2026-07-28, before this reordering): the two 'HopStartup'
-- properties came back in 1.2s and 0.8s, 'HopRunning'/'RunStruct' in 16.3s,
-- and 'HopRunning'/'RunFlushRf' had not finished after 30 minutes. The
-- separator is the PREMISE, not the conclusion: a 'HopStartup' premise is a
-- few @Nop@ equalities and two point reads, while every 'HopRunning' premise
-- carries the whole running case, decode tree and memory flush included.
--
-- Hence this order: all four 'HopStartup' obligations first, then the four
-- emptiness claims (whose premises should be outright contradictory), then the
-- expensive 'HopRunning' ones last. Everything that can be learned cheaply is
-- learned before anything expensive is attempted.

-- Cheap: startup premise. ------------------------------------------------------

{-# ANN k1StartStruct (Theory arrayAxioms) #-}
k1StartStruct :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1StartStruct = prop HopStartup RunStruct

{-# ANN k1StartFlushRf (Theory arrayAxioms) #-}
k1StartFlushRf :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1StartFlushRf = prop HopStartup RunFlushRf

{-# ANN k1StartFlushMem (Theory arrayAxioms) #-}
k1StartFlushMem :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1StartFlushMem = prop HopStartup RunFlushMem

{-# ANN k1StartDecode (Theory arrayAxioms) #-}
k1StartDecode :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1StartDecode = prop HopStartup RunDecode

-- Cheaper still, if the claims are right: contradictory premises. -------------
--
-- These four replace prose. Splitting the obligation across the invariant's
-- cases produces eight, of which the six not listed above were dismissed by
-- argument. Four of those arguments are non-trivial, so they are stated here
-- as obligations; the remaining two are the same claim at the other
-- 'HopCase' and follow immediately.

{-# ANN k1EmptyRunStartup (Theory arrayAxioms) #-}
k1EmptyRunStartup :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1EmptyRunStartup = emptyProp EmptyRunStartup

{-# ANN k1EmptyStartupRunning (Theory arrayAxioms) #-}
k1EmptyStartupRunning :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1EmptyStartupRunning = emptyProp EmptyStartupRunning

{-# ANN k1EmptyHaltedBreak (Theory arrayAxioms) #-}
k1EmptyHaltedBreak :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1EmptyHaltedBreak = emptyProp EmptyHaltedBreak

{-# ANN k1EmptyHaltedCall (Theory arrayAxioms) #-}
k1EmptyHaltedCall :: K1State -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1EmptyHaltedCall = emptyProp EmptyHaltedCall

-- Expensive: running premise. --------------------------------------------------

{-# ANN k1WordCasesExhaustive (Theory arrayAxioms) #-}
k1WordCasesExhaustive :: Word -> Pantomime.Bool
k1WordCasesExhaustive iw =
  Pantomime.boolean $ decodeWordCasesExhaustive iw

{-# ANN k1RunStruct (Theory arrayAxioms) #-}
k1RunStruct :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunStruct = runProp RunStruct

-- Register-file flush, split by top-level opcode. The unsplit fresh-word
-- version still exceeded five minutes in Z3; these eleven exhaustive pieces
-- replace it. Measured on 2026-07-28 with Z3 4.15.3: the R-type piece reached
-- UNSAT in 3m45.8s.

{-# ANN k1RunFlushRfR (Theory arrayAxioms) #-}
k1RunFlushRfR :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfR = runCaseProp WordR RunFlushRf

{-# ANN k1RunFlushRfIArith (Theory arrayAxioms) #-}
k1RunFlushRfIArith :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfIArith = runCaseProp WordIArith RunFlushRf

{-# ANN k1RunFlushRfLoad (Theory arrayAxioms) #-}
k1RunFlushRfLoad :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfLoad = runCaseProp WordLoad RunFlushRf

{-# ANN k1RunFlushRfJalr (Theory arrayAxioms) #-}
k1RunFlushRfJalr :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfJalr = runCaseProp WordJalr RunFlushRf

{-# ANN k1RunFlushRfSystem (Theory arrayAxioms) #-}
k1RunFlushRfSystem :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfSystem = runCaseProp WordSystem RunFlushRf

{-# ANN k1RunFlushRfStore (Theory arrayAxioms) #-}
k1RunFlushRfStore :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfStore = runCaseProp WordStore RunFlushRf

{-# ANN k1RunFlushRfBranch (Theory arrayAxioms) #-}
k1RunFlushRfBranch :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfBranch = runCaseProp WordBranch RunFlushRf

{-# ANN k1RunFlushRfLui (Theory arrayAxioms) #-}
k1RunFlushRfLui :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfLui = runCaseProp WordLui RunFlushRf

{-# ANN k1RunFlushRfAuipc (Theory arrayAxioms) #-}
k1RunFlushRfAuipc :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfAuipc = runCaseProp WordAuipc RunFlushRf

{-# ANN k1RunFlushRfJal (Theory arrayAxioms) #-}
k1RunFlushRfJal :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfJal = runCaseProp WordJal RunFlushRf

{-# ANN k1RunFlushRfOther (Theory arrayAxioms) #-}
k1RunFlushRfOther :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushRfOther = runCaseProp WordOther RunFlushRf

{-# ANN k1RunFlushMem (Theory arrayAxioms) #-}
k1RunFlushMem :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunFlushMem = runProp RunFlushMem

{-# ANN k1RunDecode (Theory arrayAxioms) #-}
k1RunDecode :: K1RunState -> Word -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Address -> Pantomime.Bool
k1RunDecode = runProp RunDecode

results :: [(String, Maybe String)]
results =
  [ ("k1StartStruct", $(pantomime 'k1StartStruct)),
    ("k1StartFlushRf", $(pantomime 'k1StartFlushRf)),
    ("k1StartFlushMem", $(pantomime 'k1StartFlushMem)),
    ("k1StartDecode", $(pantomime 'k1StartDecode)),
    ("k1EmptyRunStartup", $(pantomime 'k1EmptyRunStartup)),
    ("k1EmptyStartupRunning", $(pantomime 'k1EmptyStartupRunning)),
    ("k1EmptyHaltedBreak", $(pantomime 'k1EmptyHaltedBreak)),
    ("k1EmptyHaltedCall", $(pantomime 'k1EmptyHaltedCall)),
    ("k1WordCasesExhaustive", $(pantomime 'k1WordCasesExhaustive)),
    ("k1RunStruct", $(pantomime 'k1RunStruct)),
    ("k1RunFlushRfR", $(pantomime 'k1RunFlushRfR)),
    ("k1RunFlushRfIArith", $(pantomime 'k1RunFlushRfIArith)),
    ("k1RunFlushRfLoad", $(pantomime 'k1RunFlushRfLoad)),
    ("k1RunFlushRfJalr", $(pantomime 'k1RunFlushRfJalr)),
    ("k1RunFlushRfSystem", $(pantomime 'k1RunFlushRfSystem)),
    ("k1RunFlushRfStore", $(pantomime 'k1RunFlushRfStore)),
    ("k1RunFlushRfBranch", $(pantomime 'k1RunFlushRfBranch)),
    ("k1RunFlushRfLui", $(pantomime 'k1RunFlushRfLui)),
    ("k1RunFlushRfAuipc", $(pantomime 'k1RunFlushRfAuipc)),
    ("k1RunFlushRfJal", $(pantomime 'k1RunFlushRfJal)),
    ("k1RunFlushRfOther", $(pantomime 'k1RunFlushRfOther)),
    ("k1RunFlushMem", $(pantomime 'k1RunFlushMem)),
    ("k1RunDecode", $(pantomime 'k1RunDecode))
  ]
