{- HLINT ignore "Use isNothing" -}
-- | The functional-correctness invariant.
--
-- The invariant relates an architectural state @(isaPc, isaRegFile, isaMem)@ to
-- a system state @((core state), (input), mem)@. It is a disjunction of cases:
-- one for the running core, and two for the halted core. The reset state is
-- not among them: 'Proof.Core.Functional.Solver.baseCase' steps it two cycles,
-- into the running case, instead.
--
-- Two forms of the same predicate live here:
--
--   * 'invCases' represents each case as a named list of conjuncts, so a
--     failing QuickCheck run can report which clause broke ('explain');
--   * 'invAt' is the identical predicate built from '&&' and '||' with no
--     lists, because Pantomime's evaluator diverges on recursion it cannot
--     prove terminating -- even over a fully concrete list. Symbolic execution
--     uses this one.
--
-- The test \"fold-free invariant agrees with the list version\" keeps the two
-- from drifting.
--
-- The invariant describes the core attached to memory that answers every
-- request in the next cycle ('Proof.Core.Functional.Machine.stepSys'), so the
-- bus always holds an answer ('Core.inputMem' is 'Just').
module Proof.Core.Functional.Invariant
  ( applyWbStage,
    applyMeStage,
    Case (..),
    inv,
    invAtList,
    invCases,
    invCasesAt,
    invAt,
    explain,
    isaOf,
    sysInvAt,
    archPc,
    archRegFile,
    archMem,
    pcUnderInv,
    regFileUnderInv,
    memUnderInv,
    invFunctional,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.Pipeline as Core
import Data.Maybe (fromMaybe, isJust)
import Core.Instruction
import Memory.Types
import Core.ISA (IsaStateG (..), IsaState)
import Proof.Core.Functional.Machine
import Core.RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | Apply the pending effect of the writeback-stage instruction. The load case
-- reads its value off @inputMem@, matching 'Core.writeback'.
applyWbStage ::
  (RegFileOps r) =>
  Instruction ->
  Word ->
  Word ->
  (m, r) ->
  (m, r)
applyWbStage ir res inputWord (mem, rf) =
  case ir of
    RType _ rd _ _ -> (mem, put rd res)
    IType (Arith _) rd _ _ -> (mem, put rd res)
    IType (Load size sign) rd _ _ ->
      (mem, put rd (loadExtend size sign inputWord))
    SType {} -> (mem, rf)
    BType {} -> (mem, rf)
    JType rd _ -> (mem, put rd res)
    IType Jump rd _ _ -> (mem, put rd res)
    UType _ rd _ -> (mem, put rd res)
    IType (Env _) _ _ _ -> (mem, rf)
    Nop _ -> (mem, rf)
  where
    put rd v = modifyRFg rd v rf

-- | Apply the pending effect of the memory-stage instruction.
--
-- The two jump cases write @rd@, matching 'applyWbStage': an instruction in the
-- memory stage this cycle is in the writeback stage the next one, so its
-- pending effect has to be counted the same way in both.
applyMeStage ::
  (RegFileOps r, MemOps m) =>
  Instruction ->
  Word ->
  Address ->
  (m, r) ->
  (m, r)
applyMeStage ir res addr (mem, rf) =
  case ir of
    RType _ rd _ _ -> (mem, put rd res)
    IType (Arith _) rd _ _ -> (mem, put rd res)
    IType (Load size sign) rd _ _ ->
      (mem, put rd (loadExtend size sign (memReadWord addr mem)))
    SType size _ _ _ -> (memWrite addr size res mem, rf)
    BType {} -> (mem, rf)
    JType rd _ -> (mem, put rd res)
    IType Jump rd _ _ -> (mem, put rd res)
    UType _ rd _ -> (mem, put rd res)
    IType (Env _) _ _ _ -> (mem, rf)
    Nop _ -> (mem, rf)
  where
    put rd v = modifyRFg rd v rf

-- | A named case of the invariant, with its conjuncts.
data Case = Case
  { caseName :: String,
    caseConjuncts :: [(String, Bool)]
  }

holds :: Case -> Bool
holds = P.all snd . caseConjuncts

-- | Does any case of the invariant hold? Container form.
inv :: IsaState -> Sys -> Bool
inv isa sys = P.any holds (invCases isa sys)

-- | The invariant in pointwise form: instead of comparing whole register files
-- and memories, compare them at one witness register @wr@ and one witness byte
-- address @wa@. Quantifying over the witnesses recovers the container form.
-- Pointwise, because function-backed containers have no decidable equality but
-- can be read at a symbolic point.
--
-- Built from the named cases ('invCasesAt'), for the tests; the proofs use the
-- fold-free 'invAt', which the tests check agrees with it.
invAtList ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
invAtList wr wa isa sys = P.any holds (invCasesAt wr wa isa sys)

-- | Human-readable account of why the invariant failed: for each case, the
-- conjuncts that were false.
explain :: IsaState -> Sys -> String
explain isa sys =
  unlines
    [ "  case "
        P.++ caseName c
        P.++ ": failed "
        P.++ show (P.map fst (P.filter (P.not . snd) (caseConjuncts c)))
      | c <- invCases isa sys
    ]

-- | Container-level cases: compare register files and memories directly.
invCases :: IsaState -> Sys -> [Case]
invCases = invCasesGen (==) (==)

-- | The cases at one witness register and one witness byte address.
invCasesAt ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> [Case]
invCasesAt wr wa =
  invCasesGen
    (\a b -> lookupRFg wr a == lookupRFg wr b)
    (\a b -> memReadByte wa a == memReadByte wa b)

-- | The cases of the invariant, parameterised over how register files and
-- memories are compared.
--
-- No case constrains @stateCtrl@. The control lines carry no information
-- between cycles: 'Core.pipe' resets them at the start of each cycle, before
-- any stage reads them.
invCasesGen ::
  (RegFileOps r, MemOps m) =>
  (r -> r -> Bool) ->
  (m -> m -> Bool) ->
  IsaStateG r m ->
  SysG r m ->
  [Case]
invCasesGen eqRF eqMem (IsaState ipc irf imem) (Sys core inp mem) =
  [ runningCase,
    haltedCase "halted/ebreak" isBreak (EBreak (ipc + 4)),
    haltedCase "halted/ecall" isCall (Syscall (ipc + 4))
  ]
  where
    inputWord = fromMaybe 0 (inputMem inp)

    -- Bound once: under symbolic execution a repeated 'decode'' is not shared,
    -- and it is a large decision tree.
    isaInstr = decode' (memReadWord ipc imem)

    dePcWord = memReadWord (stateDePc core) mem

    -- The note's four running cases, collapsed.
    --
    -- They split on the note's @isArithOrJumpInstr@ and @isMemInstr@ for the
    -- writeback and memory stages, but the only conjuncts that vary across the
    -- four -- the fetch triple -- vary with the writeback stage alone: when a
    -- memory instruction is in writeback it held the bus last cycle, so nothing
    -- was fetched. The memory stage's classification changes no conjunct; its
    -- sole effect is to demand the stage hold something the two predicates
    -- cover, which is everything but @ecall@ and @ebreak@. Both stages are
    -- therefore stated directly as \"not an environment instruction\".
    runningCase =
      Case "running" $
        [ ("running", stateHalt core == Nothing),
          ("memory answered", isJust (inputMem inp)),
          ( "wb is not an env instruction",
            P.not (isEnvInstr (stateWbInstr core))
          ),
          ( "me is not an env instruction",
            P.not (isEnvInstr (stateMeInstr core))
          ),
          ("exPc == isaPc", stateExPc core == ipc),
          ("ex == decode (mem[isaPc])", stateExInstr core == isaInstr),
          ("dePc == exPc + 4", stateDePc core == stateExPc core + 4),
          ( "no load-use hazard me->ex",
            P.not (loadHazard (stateExInstr core) (stateMeInstr core))
          ),
          ( "(isaMem, isaRegFile) == pending writes applied",
            let (fm, frf) =
                  applyMeStage
                    (stateMeInstr core)
                    (stateMeRes core)
                    (stateMeAddr core)
                    ( applyWbStage
                        (stateWbInstr core)
                        (stateWbRes core)
                        inputWord
                        (mem, stateRegFile core)
                    )
             in eqMem imem fm && eqRF irf frf
          )
        ]
          P.++ if isMemInstr (stateWbInstr core)
            then
              [ ("not deExpInstr", P.not (stateDeExpInstr core)),
                ("fePc == exPc + 4", stateFePc core == stateExPc core + 4)
              ]
            else
              [ ("deExpInstr", stateDeExpInstr core),
                ("inputMem == mem[dePc]", inputWord == dePcWord),
                ("fePc == exPc + 8", stateFePc core == stateExPc core + 8)
              ]

    -- The halt carries the address the core would resume at, which
    -- 'Core.execute' set from the trapping instruction's own PC. Pinning it to
    -- @isaPc + 4@ is what fixes @isaPc@ as that instruction's address;
    -- "Core.ISA" leaves the architectural state where it was, so the ISA is
    -- still \"at\" the trap.
    haltedCase name isKind expected =
      Case
        name
        [ ("isa instruction is of this kind", isKind isaInstr),
          ("halt state matches", stateHalt core == Just expected),
          ("wb == Nop Halted", stateWbInstr core == Nop Halted),
          ("memory answered", isJust (inputMem inp)),
          ("me == Nop Halted", stateMeInstr core == Nop Halted),
          ("ex == Nop Halted", stateExInstr core == Nop Halted),
          ("isaRegFile == stateRegFile", eqRF irf (stateRegFile core)),
          ("isaMem == stateMem", eqMem imem mem)
        ]

-- The fold-free form ----------------------------------------------------------

-- | The invariant, pointwise, built from '&&' and '||' with no lists and no
-- folds. Semantically identical to @'P.any' holds ('invCasesAt' wr wa)@; see
-- the module header for why both exist.
invAt ::
  (RegFileOps r, MemOps m) =>
  RegIdx ->
  Address ->
  IsaStateG r m ->
  SysG r m ->
  Bool
invAt wr wa isa sys =
  runningCaseAt wr wa isa sys
    || haltedCaseAt HaltBreak wr wa isa sys
    || haltedCaseAt HaltCall wr wa isa sys

-- | Which of the two halted cases: @ebreak@ or @ecall@.
data HaltKind = HaltBreak | HaltCall

-- | The running case.
runningCaseAt ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
runningCaseAt wr wa (IsaState ipc irf imem) (Sys core inp mem) =
  stateHalt core == Nothing
    && isJust (inputMem inp)
    && not (isEnvInstr (stateWbInstr core))
    && not (isEnvInstr (stateMeInstr core))
    && stateExPc core == ipc
    && stateExInstr core == decode' (memReadWord ipc imem)
    && stateDePc core == stateExPc core + 4
    && not (loadHazard (stateExInstr core) (stateMeInstr core))
    && ( if isMemInstr (stateWbInstr core)
           then
             not (stateDeExpInstr core)
               && stateFePc core == stateExPc core + 4
           else
             stateDeExpInstr core
               && inputMem inp == Just (memReadWord (stateDePc core) mem)
               && stateFePc core == stateExPc core + 8
       )
    && memReadByte wa imem == memReadByte wa fm
    && lookupRFg wr irf == lookupRFg wr frf
  where
    -- The pending writes the container form applies, read at the witness.
    -- Writing it out pointwise by hand would duplicate 'applyMeStage' and
    -- 'memWrite' for no gain: 'isaOf' already puts them into every query, and
    -- the solver rewrites @select@ over a @store@ chain natively.
    (fm, frf) =
      applyMeStage
        (stateMeInstr core)
        (stateMeRes core)
        (stateMeAddr core)
        ( applyWbStage
            (stateWbInstr core)
            (stateWbRes core)
            (fromMaybe 0 (inputMem inp))
            (mem, stateRegFile core)
        )

-- | One halted case.
haltedCaseAt ::
  (RegFileOps r, MemOps m) =>
  HaltKind -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
haltedCaseAt kind wr wa (IsaState ipc irf imem) (Sys core inp mem) =
  isKind (decode' (memReadWord ipc imem))
    && stateHalt core == Just expected
    && stateWbInstr core == Nop Halted
    && isJust (inputMem inp)
    && stateMeInstr core == Nop Halted
    && stateExInstr core == Nop Halted
    && memReadByte wa imem == memReadByte wa mem
    && lookupRFg wr irf == lookupRFg wr (stateRegFile core)
  where
    isKind = case kind of
      HaltBreak -> isBreak
      HaltCall -> isCall
    expected = case kind of
      HaltBreak -> EBreak (ipc + 4)
      HaltCall -> Syscall (ipc + 4)

-- The architectural state, and properties of the invariant --------------------

-- | The architectural state that the invariant claims @sys@ corresponds to:
-- its PC, register file and memory ('archPc', 'archRegFile', 'archMem').
--
-- The containers are /derived/ from @sys@ ('archRegFile', 'archMem') rather
-- than taken as further inputs. They have to be: the invariant compares them
-- pointwise, at one witness register and one witness byte, so a freely
-- quantified register file and memory would be tied to @sys@ only at those two
-- points. 'Core.ISA.isaStep' reads memory at @isaPc@ and registers at
-- @rs1@/@rs2@, which are elsewhere, so the solver could pick an architectural
-- state whose instruction at @isaPc@ bears no relation to the one the core is
-- executing. Deriving them makes the container equalities hold by construction,
-- and what is left in the premise is the scalar conjuncts, assumed in full.
--
-- @isaPc@ is derived as well. The invariant pins it via a different conjunct
-- in each case -- @stateExPc@ when running and @stateHalt@ once halted -- and
-- 'archPc' splits on the case accordingly, so deriving it makes neither case
-- unsatisfiable as a premise.
isaOf :: (RegFileOps r, MemOps m) => SysG r m -> IsaStateG r m
isaOf sys =
  IsaState
    { isaPc = archPc sys,
      isaRegFile = archRegFile sys,
      isaMem = archMem sys
    }

-- | The system is one the invariant covers: it is related to its own
-- architectural state ('isaOf'), at register @wr@ and byte @wa@.
sysInvAt :: (RegFileOps r, MemOps m) => RegIdx -> Address -> SysG r m -> Bool
sysInvAt wr wa sys = invAt wr wa (isaOf sys) sys

-- | The PC the core stands for: the execute stage's while running; once halted,
-- the trapping instruction's, which is the resume address the halt carries
-- minus 4.
archPc :: SysG r m -> Address
archPc (Sys core _ _) = case Core.stateHalt core of
  Nothing -> Core.stateExPc core
  Just (EBreak resume) -> resume - 4
  Just (Syscall resume) -> resume - 4

-- | The register file the core stands for: its own, with the writes still in
-- the pipeline (the memory and writeback stages) applied.
archRegFile :: (RegFileOps r, MemOps m) => SysG r m -> r
archRegFile = snd . archMemRegFile

-- | The memory the core stands for: its own, with the stores still in the
-- pipeline (the memory and writeback stages) applied.
archMem :: (RegFileOps r, MemOps m) => SysG r m -> m
archMem = fst . archMemRegFile

-- | The pipeline's pending writes applied to memory and the register file.
archMemRegFile :: (RegFileOps r, MemOps m) => SysG r m -> (m, r)
archMemRegFile (Sys core inp mem) =
  applyMeStage
    (Core.stateMeInstr core)
    (Core.stateMeRes core)
    (Core.stateMeAddr core)
    ( applyWbStage
        (Core.stateWbInstr core)
        (Core.stateWbRes core)
        (fromMaybe 0 (Core.inputMem inp))
        (mem, Core.stateRegFile core)
    )

-- | The invariant relating the core to the ISA implies that the ISA's PC is
-- the core's architectural one ('archPc').
pcUnderInv ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
pcUnderInv wr wa isa sys =
  not premises || conclusion
  where
    premises = invAt wr wa isa sys

    conclusion = isaPc isa == archPc sys

-- | The invariant relating the core to the ISA implies that the ISA's register
-- file is the core's architectural one ('archRegFile'), at register @wr@.
regFileUnderInv ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
regFileUnderInv wr wa isa sys =
  not premises || conclusion
  where
    premises = invAt wr wa isa sys

    conclusion = lookupRFg wr (isaRegFile isa) == lookupRFg wr (archRegFile sys)

-- | The invariant relating the core to the ISA implies that the ISA's memory is
-- the core's architectural one ('archMem'), at byte @wa@.
memUnderInv ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
memUnderInv wr wa isa sys =
  not premises || conclusion
  where
    premises = invAt wr wa isa sys

    conclusion = memReadByte wa (isaMem isa) == memReadByte wa (archMem sys)

-- | The invariant is functional: two ISA states it relates to the same system
-- are equal, at register @wr@ and byte @wa@. Quantifying over @wr@ and @wa@
-- gives equality of the states.
invFunctional ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> IsaStateG r m -> SysG r m -> Bool
invFunctional wr wa isa1 isa2 sys =
  not premises || conclusion
  where
    premises = invAt wr wa isa1 sys && invAt wr wa isa2 sys

    conclusion =
      isaPc isa1 == isaPc isa2
        && lookupRFg wr (isaRegFile isa1) == lookupRFg wr (isaRegFile isa2)
        && memReadByte wa (isaMem isa1) == memReadByte wa (isaMem isa2)
