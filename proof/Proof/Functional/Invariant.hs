-- | The invariant from @proof/notes/invariant.txt@, as code.
--
-- The invariant relates an architectural state @(isaPc, isaRegFile, isaMem)@ to
-- a system state @((core state), (input), mem)@. It is a disjunction of cases:
-- one for startup, four for the running core, and two for the halted core.
--
-- Two forms of the same predicate live here:
--
--   * 'invCases' represents each case as a named list of conjuncts, so a
--     failing QuickCheck run can report which clause broke ('explain');
--   * 'invAtFree' is the identical predicate built from '&&' and '||' with no
--     lists, because Pantomime's evaluator diverges on recursion it cannot
--     prove terminating -- even over a fully concrete list. Symbolic execution
--     uses this one.
--
-- The test \"fold-free invariant agrees with the list version\" keeps the two
-- from drifting.
--
-- == Where this differs from @invariant.txt@
--
-- One place, marked again at its definition: NO @stateCtrl == initCtrl@ clause.
-- It is a holdover from an earlier 'Core' that reset the control lines at the
-- /end/ of each clock cycle; 'Core.withCtrlReset' now resets them at the start,
-- so no state reached by stepping the core satisfies it.
--
-- The note's four running cases are collapsed into one here. They differ only
-- in the fetch conjuncts, and those track the writeback stage alone; the memory
-- stage's classification does no work beyond excluding an environment
-- instruction. See 'invCasesGen'.
module Proof.Functional.Invariant
  ( flushWbStage,
    flushMeStage,
    isArithOrJumpInstr,
    Case (..),
    inv,
    invAt,
    invCases,
    invCasesAt,
    invAtFree,
    explain,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Instruction
import Memory.Types
import Proof.Driver (isEnvInstr, isMemInstr)
import ISA (IsaStateG (..), IsaState)
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | From @invariant.txt@: everything that is neither a memory instruction nor
-- an environment instruction.
--
-- Together with 'Proof.Driver.isMemInstr' this is what the note's four running
-- cases split on. The two are complementary apart from @ecall@ and @ebreak@,
-- which neither accepts, so requiring an instruction to satisfy one or the
-- other is exactly requiring that it not be an environment instruction -- which
-- is how the running case states it.
isArithOrJumpInstr :: Instruction -> Bool
isArithOrJumpInstr ir =
  case ir of
    RType {} -> True
    IType (Arith _) _ _ _ -> True
    IType (Load _ _) _ _ _ -> False
    SType {} -> False
    BType {} -> True
    JType _ _ -> True
    IType Jump _ _ _ -> True
    UType {} -> True
    IType (Env _) _ _ _ -> False
    Nop _ -> True

-- | Apply the pending effect of the writeback-stage instruction. The load case
-- reads its value off @inputMem@, matching 'Core.writeback'.
flushWbStage ::
  (RegFileOps r) =>
  Instruction ->
  Word ->
  Word ->
  (m, r Identity) ->
  (m, r Identity)
flushWbStage ir res inputWord (mem, rf) =
  case ir of
    RType _ rd _ _ -> (mem, put rd res)
    IType (Arith _) rd _ _ -> (mem, put rd res)
    IType (Load size sign) rd _ _ -> (mem, put rd (loadExtend size sign inputWord))
    SType {} -> (mem, rf)
    BType {} -> (mem, rf)
    JType rd _ -> (mem, put rd res)
    IType Jump rd _ _ -> (mem, put rd res)
    UType _ rd _ -> (mem, put rd res)
    IType (Env _) _ _ _ -> (mem, rf)
    Nop _ -> (mem, rf)
  where
    put rd v = modifyRFg rd (pure v) rf

-- | Apply the pending effect of the memory-stage instruction.
--
-- The two jump cases write @rd@, matching 'flushWbStage': an instruction in the
-- memory stage this cycle is in the writeback stage the next one, so its pending
-- effect has to be counted the same way in both.
flushMeStage ::
  (RegFileOps r, MemOps m) =>
  Instruction ->
  Word ->
  Address ->
  (m, r Identity) ->
  (m, r Identity)
flushMeStage ir res addr (mem, rf) =
  case ir of
    RType _ rd _ _ -> (mem, put rd res)
    IType (Arith _) rd _ _ -> (mem, put rd res)
    IType (Load size sign) rd _ _ ->
      (mem, put rd (loadExtend size sign (memReadWord addr mem)))
    SType size _ _ _ -> (memWriteWord size addr res mem, rf)
    BType {} -> (mem, rf)
    JType rd _ -> (mem, put rd res)
    IType Jump rd _ _ -> (mem, put rd res)
    UType _ rd _ -> (mem, put rd res)
    IType (Env _) _ _ _ -> (mem, rf)
    Nop _ -> (mem, rf)
  where
    put rd v = modifyRFg rd (pure v) rf

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
--
-- This is the version symbolic execution can use: function-backed containers
-- have no decidable equality, but they can be read at a symbolic point.
invAt :: (RegFileOps r, MemOps m) => RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
invAt wr wa isa sys = P.any holds (invCasesAt wr wa isa sys)

-- | Human-readable account of why the invariant failed: for each case, the
-- conjuncts that were false.
explain :: IsaState -> Sys -> String
explain isa sys =
  unlines
    [ "  case " P.++ caseName c P.++ ": failed " P.++ show (P.map fst (P.filter (P.not . snd) (caseConjuncts c)))
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
    (\a b -> runIdentity (lookupRFg wr a) == runIdentity (lookupRFg wr b))
    (\a b -> memReadByte wa a == memReadByte wa b)

-- | The cases of the invariant, parameterised over how register files and
-- memories are compared.
--
-- The one deviation from @invariant.txt@ is a silence: the note opens every
-- case with @stateCtrl == initCtrl@, and no case here has it. That clause dates from an
-- earlier 'Core' which reset the control lines at the end of each clock cycle,
-- leaving them at their reset values by the time the next cycle began.
-- 'Core.withCtrlReset' now resets them at the /start/ of the cycle and the
-- stages then set them, so the lines observed in any post-step state are the
-- ones the stages left -- 'Core.execute' alone always sets @ctrlExInstr@ to
-- 'Just'. Only 'Core.init' still satisfies the clause, so keeping it would make
-- the invariant hold of no state the driver lands on and every obligation
-- vacuous. Dropping it is sound because the lines carry no information between
-- cycles: 'Core.withCtrlReset' overwrites them before any stage reads them.
invCasesGen ::
  (RegFileOps r, MemOps m) =>
  (r Identity -> r Identity -> Bool) ->
  (m -> m -> Bool) ->
  IsaStateG r m ->
  SysG r m ->
  [Case]
invCasesGen eqRF eqMem (IsaState ipc irf imem) sys@(Sys st inp mem) =
  [ runningCase,
    Case
      "startup"
      [ ("running", running sys),
        ("wb == Nop FirstCycle", stateWbInstr st == Nop FirstCycle),
        ("me == Nop FirstCycle", stateMeInstr st == Nop FirstCycle),
        ("ex == Nop FirstCycle", stateExInstr st == Nop FirstCycle),
        ("not inputIsInstr", P.not (inputIsInstr inp)),
        ("fePc == isaPc", stateFePc st == ipc),
        ("isaRegFile == stateRegFile", eqRF irf (stateRegFile st)),
        ("isaMem == stateMem", eqMem imem mem)
      ],
    haltedCase "halted/ebreak" isBreak (EBreak (ipc + 4)),
    haltedCase "halted/ecall" isCall (Core.Syscall (ipc + 4))
  ]
  where
    inputWord = runIdentity (inputMem inp)

    -- Bound once: under symbolic execution a repeated 'decode'' is not shared,
    -- and it is a large decision tree.
    isaInstr = decode' (memReadWord ipc imem)

    dePcWord = memReadWord (stateDePc st) mem

    -- The note's four running cases, collapsed.
    --
    -- They split on @(isArithOrJumpInstr, isMemInstr)@ for the writeback and
    -- memory stages, but the only conjuncts that vary across the four -- the
    -- fetch triple -- vary with the writeback stage alone: when a memory
    -- instruction is in writeback it held the bus last cycle, so nothing was
    -- fetched. The memory stage's classification changes no conjunct; its sole
    -- effect is to demand the stage hold something the two predicates cover,
    -- which is everything but @ecall@ and @ebreak@. Both stages are therefore
    -- stated directly as \"not an environment instruction\".
    runningCase =
      Case "running" $
        [ ("running", running sys),
          ("wb is not an env instruction", P.not (isEnvInstr (stateWbInstr st))),
          ("me is not an env instruction", P.not (isEnvInstr (stateMeInstr st))),
          ("exPc == isaPc", stateExPc st == ipc),
          ("ex == decode (mem[isaPc])", stateExInstr st == isaInstr),
          ("dePc == exPc + 4", stateDePc st == stateExPc st + 4),
          ("no load-use hazard me->ex", P.not (loadHazard (stateExInstr st) (stateMeInstr st))),
          ( "(isaMem, isaRegFile) == flush",
            let (fm, frf) =
                  flushMeStage
                    (stateMeInstr st)
                    (runIdentity (stateMeRes st))
                    (stateMeAddr st)
                    (flushWbStage (stateWbInstr st) (runIdentity (stateWbRes st)) inputWord (mem, stateRegFile st))
             in eqMem imem fm && eqRF irf frf
          )
        ]
          P.++ if isMemInstr (stateWbInstr st)
            then
              [ ("not inputIsInstr", P.not (inputIsInstr inp)),
                ("fePc == exPc + 4", stateFePc st == stateExPc st + 4)
              ]
            else
              [ ("inputIsInstr", inputIsInstr inp),
                ("inputMem == mem[dePc]", inputWord == dePcWord),
                ("fePc == exPc + 8", stateFePc st == stateExPc st + 8)
              ]

    -- The halt carries the address the core would resume at, which 'Core.execute'
    -- set from the trapping instruction's own PC. Pinning it to @isaPc + 4@ is
    -- what fixes @isaPc@ as that instruction's address; "ISA" leaves
    -- the architectural state where it was, so the ISA is still \"at\" the trap.
    haltedCase name isKind expected =
      Case
        name
        [ ("isa instruction is of this kind", isKind isaInstr),
          ("halt state matches", stateHalt st == Just expected),
          ("wb == Nop Halted", stateWbInstr st == Nop Halted),
          ("me == Nop Halted", stateMeInstr st == Nop Halted),
          ("ex == Nop Halted", stateExInstr st == Nop Halted),
          ("isaRegFile == stateRegFile", eqRF irf (stateRegFile st)),
          ("isaMem == stateMem", eqMem imem mem)
        ]

-- The fold-free form ----------------------------------------------------------

-- | The invariant, pointwise, built from '&&' and '||' with no lists and no
-- folds. Semantically identical to @'P.any' holds ('invCasesAt' wr wa)@; see
-- the module header for why both exist.
invAtFree ::
  (RegFileOps r, MemOps m) =>
  RegIdx ->
  Address ->
  IsaStateG r m ->
  SysG r m ->
  Bool
invAtFree wr wa isa sys =
  runningCaseAt wr wa isa sys
    || startupCaseAt wr wa isa sys
    || haltedCaseAt HaltBreak wr wa isa sys
    || haltedCaseAt HaltCall wr wa isa sys

-- | Which of the two halted cases: @ebreak@ or @ecall@.
data HaltKind = HaltBreak | HaltCall

-- | The running case.
runningCaseAt ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
runningCaseAt wr wa (IsaState ipc irf imem) sys@(Sys st inp mem) =
  running sys
    && not (isEnvInstr (stateWbInstr st))
    && not (isEnvInstr (stateMeInstr st))
    && stateExPc st == ipc
    && stateExInstr st == decode' (memReadWord ipc imem)
    && stateDePc st == stateExPc st + 4
    && not (loadHazard (stateExInstr st) (stateMeInstr st))
    && ( if isMemInstr (stateWbInstr st)
           then not (inputIsInstr inp) && stateFePc st == stateExPc st + 4
           else
             inputIsInstr inp
               && runIdentity (inputMem inp) == memReadWord (stateDePc st) mem
               && stateFePc st == stateExPc st + 8
       )
    && memReadByte wa imem == memReadByte wa fm
    && runIdentity (lookupRFg wr irf) == runIdentity (lookupRFg wr frf)
  where
    -- The same flush the container form applies, read at the witness. Writing
    -- it out pointwise by hand would duplicate 'flushMeStage' and 'memWriteWord'
    -- for no gain: 'Proof.Functional.Obligation.isaAt' already puts this flush
    -- into every query, and the solver rewrites @select@ over a @store@ chain
    -- natively.
    (fm, frf) =
      flushMeStage
        (stateMeInstr st)
        (runIdentity (stateMeRes st))
        (stateMeAddr st)
        ( flushWbStage
            (stateWbInstr st)
            (runIdentity (stateWbRes st))
            (runIdentity (inputMem inp))
            (mem, stateRegFile st)
        )

-- | The startup case.
startupCaseAt ::
  (RegFileOps r, MemOps m) =>
  RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
startupCaseAt wr wa (IsaState ipc irf imem) sys@(Sys st inp mem) =
  running sys
    && stateWbInstr st == Nop FirstCycle
    && stateMeInstr st == Nop FirstCycle
    && stateExInstr st == Nop FirstCycle
    && not (inputIsInstr inp)
    && stateFePc st == ipc
    && runIdentity (lookupRFg wr irf) == runIdentity (lookupRFg wr (stateRegFile st))
    && memReadByte wa imem == memReadByte wa mem

-- | One halted case.
haltedCaseAt ::
  (RegFileOps r, MemOps m) =>
  HaltKind -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
haltedCaseAt kind wr wa (IsaState ipc irf imem) (Sys st _ mem) =
  isKind (decode' (memReadWord ipc imem))
    && stateHalt st == Just expected
    && stateWbInstr st == Nop Halted
    && stateMeInstr st == Nop Halted
    && stateExInstr st == Nop Halted
    && memReadByte wa imem == memReadByte wa mem
    && runIdentity (lookupRFg wr irf) == runIdentity (lookupRFg wr (stateRegFile st))
  where
    isKind = case kind of
      HaltBreak -> isBreak
      HaltCall -> isCall
    expected = case kind of
      HaltBreak -> EBreak (ipc + 4)
      HaltCall -> Core.Syscall (ipc + 4)
