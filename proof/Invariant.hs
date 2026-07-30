-- | The invariant from @proof/invariant.txt@, as code.
--
-- The invariant relates an architectural state @(isaPc, isaRegFile, isaMem)@ to
-- a system state @((core state), (input), mem)@. It is a disjunction of cases:
-- one for startup, one for the running core, and two for the halted core.
--
-- Two forms of the same predicate live here:
--
--   * 'invCasesWith' represents each case as a named list of conjuncts, so a
--     failing QuickCheck run can report which clause broke ('explain');
--   * 'invAtFree' is the identical predicate built from '&&' and '||' with no
--     lists, because Pantomime's evaluator diverges on recursion it cannot
--     prove terminating -- even over a fully concrete list. Symbolic execution
--     uses this one.
--
-- The test \"fold-free invariant agrees with the list version\" keeps the two
-- from drifting.
module Invariant
  ( flushWbStage,
    flushMeStage,
    Case (..),
    InvConfig (..),
    proposed,
    literal,
    inv,
    invAt,
    invCasesWith,
    invCasesAt,
    invAtFree,
    noStoreAlias,
    explain,
    explainWith,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import ISAStep
import Instruction
import Machine
import Memory.Types
import RegFile
import Types
import qualified Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

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
-- @jumpsWriteRd@ should be 'True': a jump writes @rd@ here just as it does in
-- 'flushWbStage' -- an instruction in the memory stage this cycle is in the
-- writeback stage the next one, so its pending effect must be counted
-- identically in both. @invariant.txt@ originally left @rd@ alone here, and
-- that asymmetry broke inductiveness; 'False' reproduces the original for the
-- regression test.
flushMeStage ::
  (RegFileOps r, MemOps m) =>
  Bool ->
  Instruction ->
  Word ->
  Address ->
  (m, r Identity) ->
  (m, r Identity)
flushMeStage jumpsWriteRd ir res addr (mem, rf) =
  case ir of
    RType _ rd _ _ -> (mem, put rd res)
    IType (Arith _) rd _ _ -> (mem, put rd res)
    IType (Load size sign) rd _ _ ->
      (mem, put rd (loadExtend size sign (memReadWord addr mem)))
    SType size _ _ _ -> (memWriteWord size addr res mem, rf)
    BType {} -> (mem, rf)
    JType rd _ -> (mem, if jumpsWriteRd then put rd res else rf)
    IType Jump rd _ _ -> (mem, if jumpsWriteRd then put rd res else rf)
    UType _ rd _ -> (mem, put rd res)
    IType (Env _) _ _ _ -> (mem, rf)
    Nop _ -> (mem, rf)
  where
    put rd v = modifyRFg rd (pure v) rf

-- | The places where @invariant.txt@ is ambiguous or wrong, as switches, so
-- the regression tests can check each reading.
data InvConfig = InvConfig
  { -- | Include the @stateCtrl == initCtrl@ clause. Unsatisfiable on any state
    -- reached by stepping the core, so normally 'False'.
    checkCtrl :: Bool,
    -- | Have 'flushMeStage' write @rd@ for jumps, matching 'flushWbStage'.
    jumpsWriteRdInMe :: Bool,
    -- | Require that no halt is in flight. Without it the core can halt
    -- part-way through a driver hop.
    checkHaltPending :: Bool
  }

-- | The reading this module recommends.
proposed :: InvConfig
proposed = InvConfig {checkCtrl = False, jumpsWriteRdInMe = True, checkHaltPending = True}

-- | Exactly what @invariant.txt@ says.
literal :: InvConfig
literal = InvConfig {checkCtrl = True, jumpsWriteRdInMe = False, checkHaltPending = False}

-- | A named case of the invariant, with its conjuncts.
data Case = Case
  { caseName :: String,
    caseConjuncts :: [(String, Bool)]
  }

holds :: Case -> Bool
holds = P.all snd . caseConjuncts

-- | Does any case of the invariant hold? Container form, 'proposed' reading.
inv :: IsaState -> Sys -> Bool
inv isa sys = P.any holds (invCasesWith proposed isa sys)

-- | The invariant in pointwise form: instead of comparing whole register files
-- and memories, compare them at one witness register @wr@ and one witness byte
-- address @wa@. Quantifying over the witnesses recovers the container form.
--
-- This is the version symbolic execution can use: function-backed containers
-- have no decidable equality, but they can be read at a symbolic point.
invAt :: (RegFileOps r, MemOps m) => RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
invAt wr wa isa sys = P.any holds (invCasesAt proposed wr wa isa sys)

-- | Human-readable account of why the invariant failed: for each case, the
-- conjuncts that were false.
explain :: IsaState -> Sys -> String
explain = explainWith proposed

explainWith :: InvConfig -> IsaState -> Sys -> String
explainWith cfg isa sys =
  unlines
    [ "  case " P.++ caseName c P.++ ": failed " P.++ show (P.map fst (P.filter (P.not . snd) (caseConjuncts c)))
      | c <- invCasesWith cfg isa sys
    ]

-- | Container-level cases: compare register files and memories directly.
invCasesWith :: InvConfig -> IsaState -> Sys -> [Case]
invCasesWith cfg = invCasesGen (==) (==) cfg

-- | The cases at one witness register and one witness byte address.
invCasesAt ::
  (RegFileOps r, MemOps m) =>
  InvConfig -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> [Case]
invCasesAt cfg wr wa =
  invCasesGen
    (\a b -> runIdentity (lookupRFg wr a) == runIdentity (lookupRFg wr b))
    (\a b -> memReadByte wa a == memReadByte wa b)
    cfg

-- | The cases of the invariant, parameterised over how register files and
-- memories are compared.
invCasesGen ::
  (RegFileOps r, MemOps m) =>
  (r Identity -> r Identity -> Bool) ->
  (m -> m -> Bool) ->
  InvConfig ->
  IsaStateG r m ->
  SysG r m ->
  [Case]
invCasesGen eqRF eqMem cfg (IsaState ipc irf imem) sys@(Sys st inp mem) =
  [ runningCase,
    Case
      "startup"
      [ ("running", running sys),
        ctrlClause,
        noPendingHalt,
        ("wb == Nop FirstCycle", stateWbInstr st == Nop FirstCycle),
        ("me == Nop FirstCycle", stateMeInstr st == Nop FirstCycle),
        ("ex == Nop FirstCycle", stateExInstr st == Nop FirstCycle),
        ("not inputIsInstr", P.not (inputIsInstr inp)),
        ("fePc == isaPc", stateFePc st == ipc),
        ("isaRegFile == stateRegFile", eqRF irf (stateRegFile st)),
        ("isaMem == stateMem", eqMem imem mem)
      ],
    haltedCase "halted/ebreak" isBreak isEBreak,
    haltedCase "halted/ecall" isCall isSyscall
  ]
  where
    ctrlClause
      | checkCtrl cfg = ("ctrl == initCtrl", stateCtrl st == initCtrl)
      | otherwise = ("ctrl (not checked)", True)

    -- 'Core.execute' raises a pending halt and 'Core.memory' consumes it the
    -- next cycle; at a state the driver lands on there is never one in flight.
    noPendingHalt
      | checkHaltPending cfg = ("no halt pending", stateHaltPending st == Nothing)
      | otherwise = ("halt pending (not checked)", True)

    inputWord = runIdentity (inputMem inp)

    -- Bound once: under symbolic execution a repeated 'decode'' is not shared,
    -- and it is a large decision tree.
    isaInstr = decode' (memReadWord ipc imem)

    dePcWord = memReadWord (stateDePc st) mem

    isMem ir = isLoad ir || isStore ir

    -- The fetch behaviour is determined by the writeback stage: if the
    -- instruction now in writeback was a load/store, it occupied the bus last
    -- cycle, so no instruction was fetched.
    runningCase =
      Case "running" $
        [ ("running", running sys),
          ctrlClause,
          noPendingHalt,
          ("exPc == isaPc", stateExPc st == ipc),
          ("ex == decode (mem[isaPc])", stateExInstr st == isaInstr),
          ("dePc == exPc + 4", stateDePc st == stateExPc st + 4),
          ("no load-use hazard me->ex", P.not (loadHazard (stateExInstr st) (stateMeInstr st))),
          ( "(isaMem, isaRegFile) == flush",
            let (fm, frf) =
                  flushMeStage
                    (jumpsWriteRdInMe cfg)
                    (stateMeInstr st)
                    (runIdentity (stateMeRes st))
                    (stateMeAddr st)
                    (flushWbStage (stateWbInstr st) (runIdentity (stateWbRes st)) inputWord (mem, stateRegFile st))
             in eqMem imem fm && eqRF irf frf
          )
        ]
          P.++ if isMem (stateWbInstr st)
            then
              [ ("not inputIsInstr", P.not (inputIsInstr inp)),
                ("fePc == exPc + 4", stateFePc st == stateExPc st + 4)
              ]
            else
              [ ("inputIsInstr", inputIsInstr inp),
                ("inputMem == mem[dePc]", inputWord == dePcWord),
                ("fePc == exPc + 8", stateFePc st == stateExPc st + 8)
              ]

    haltedCase name isKind matchesHalt =
      Case
        name
        [ ("isa instruction is of this kind", isKind isaInstr),
          ("halt state matches", maybe False matchesHalt (stateHalt st)),
          ctrlClause,
          noPendingHalt,
          ("wb == Nop Halted", stateWbInstr st == Nop Halted),
          ("me == Nop Halted", stateMeInstr st == Nop Halted),
          ("ex == Nop Halted", stateExInstr st == Nop Halted),
          ("isaRegFile == stateRegFile", eqRF irf (stateRegFile st)),
          ("isaMem == stateMem", eqMem imem mem)
        ]

    isEBreak (EBreak _) = True
    isEBreak _ = False

    isSyscall (Core.Syscall _) = True
    isSyscall _ = False

-- The fold-free form ----------------------------------------------------------

-- | The invariant, pointwise, built from '&&' and '||' with no lists and no
-- folds. Semantically identical to @'P.any' holds ('invCasesAt' cfg wr wa)@;
-- see the module header for why both exist.
invAtFree ::
  (RegFileOps r, MemOps m) =>
  InvConfig ->
  RegIdx ->
  Address ->
  IsaStateG r m ->
  SysG r m ->
  Bool
invAtFree cfg wr wa isa sys =
  runningCaseAt cfg wr wa isa sys
    || startupCaseAt cfg wr wa isa sys
    || haltedCaseAt cfg HaltBreak wr wa isa sys
    || haltedCaseAt cfg HaltCall wr wa isa sys

-- | Which of the two halted cases: @ebreak@ or @ecall@.
data HaltKind = HaltBreak | HaltCall

ctrlOkOf :: InvConfig -> Core.StateG r Identity -> Bool
ctrlOkOf cfg st = not (checkCtrl cfg) || stateCtrl st == initCtrl

haltPendingOkOf :: InvConfig -> Core.StateG r Identity -> Bool
haltPendingOkOf cfg st = not (checkHaltPending cfg) || stateHaltPending st == Nothing

-- | The running case.
runningCaseAt ::
  (RegFileOps r, MemOps m) =>
  InvConfig -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
runningCaseAt cfg wr wa (IsaState ipc irf imem) sys@(Sys st inp mem) =
  running sys
    && ctrlOkOf cfg st
    && haltPendingOkOf cfg st
    && stateExPc st == ipc
    && stateExInstr st == decode' (memReadWord ipc imem)
    && stateDePc st == stateExPc st + 4
    && not (loadHazard (stateExInstr st) (stateMeInstr st))
    && ( if isLoad (stateWbInstr st) || isStore (stateWbInstr st)
           then not (inputIsInstr inp) && stateFePc st == stateExPc st + 4
           else
             inputIsInstr inp
               && runIdentity (inputMem inp) == memReadWord (stateDePc st) mem
               && stateFePc st == stateExPc st + 8
       )
    && memReadByte wa imem == flushMemByteAt wa sys
    && runIdentity (lookupRFg wr irf) == flushRfWordAt cfg wr sys

-- | The startup case.
startupCaseAt ::
  (RegFileOps r, MemOps m) =>
  InvConfig -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
startupCaseAt cfg wr wa (IsaState ipc irf imem) sys@(Sys st inp mem) =
  running sys
    && ctrlOkOf cfg st
    && haltPendingOkOf cfg st
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
  InvConfig -> HaltKind -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
haltedCaseAt cfg kind wr wa (IsaState ipc irf imem) (Sys st _ mem) =
  isKind (decode' (memReadWord ipc imem))
    && maybe False matchesHalt (stateHalt st)
    && ctrlOkOf cfg st
    && haltPendingOkOf cfg st
    && stateWbInstr st == Nop Halted
    && stateMeInstr st == Nop Halted
    && stateExInstr st == Nop Halted
    && memReadByte wa imem == memReadByte wa mem
    && runIdentity (lookupRFg wr irf) == runIdentity (lookupRFg wr (stateRegFile st))
  where
    isKind = case kind of
      HaltBreak -> isBreak
      HaltCall -> isCall
    matchesHalt h = case kind of
      HaltBreak -> case h of EBreak _ -> True; _ -> False
      HaltCall -> case h of Core.Syscall _ -> True; _ -> False

-- | Read one byte from the memory produced by the invariant's pipeline flush,
-- without constructing that whole memory first.
--
-- 'flushWbStage' never changes memory; 'flushMeStage' changes it only for a
-- store, by writing up to four consecutive bytes. So this mux is exactly
-- @memReadByte wa@ of the flushed memory, but it sends the solver a pointwise
-- read-over-write formula instead of a @select@ over a nested @store@ chain.
flushMemByteAt :: (MemOps m) => Address -> SysG r m -> Byte
flushMemByteAt wa (Sys st _ mem) =
  case stateMeInstr st of
    SType size _ _ _ ->
      let a = stateMeAddr st
          w = runIdentity (stateMeRes st)
          b0 = slice d7 d0 w
          b1 = slice d15 d8 w
          b2 = slice d23 d16 w
          b3 = slice d31 d24 w
          old = memReadByte wa mem
       in case size of
            Types.Byte ->
              if wa == a then b0 else old
            Types.Half ->
              if wa == a
                then b0
                else if wa == a + 1 then b1 else old
            Types.Word ->
              if wa == a
                then b0
                else
                  if wa == a + 1
                    then b1
                    else
                      if wa == a + 2
                        then b2
                        else if wa == a + 3 then b3 else old
    _ -> memReadByte wa mem

-- | Read one register from the register file produced by the invariant's
-- pipeline flush, without constructing the two nested updates first.
--
-- Writeback is applied before memory, so a memory-stage write to the witness
-- register wins. Register zero is immutable.
flushRfWordAt ::
  (RegFileOps r, MemOps m) =>
  InvConfig -> RegIdx -> SysG r m -> Word
flushRfWordAt cfg wr (Sys st inp mem)
  | wr == 0 = 0
  | otherwise = applyMe (applyWb old)
  where
    old = runIdentity (lookupRFg wr (stateRegFile st))
    inputWord = runIdentity (inputMem inp)

    put rd value prior = if wr == rd then value else prior

    applyWb prior =
      case stateWbInstr st of
        RType _ rd _ _ -> put rd (runIdentity (stateWbRes st)) prior
        IType (Arith _) rd _ _ -> put rd (runIdentity (stateWbRes st)) prior
        IType (Load size sign) rd _ _ ->
          put rd (loadExtend size sign inputWord) prior
        JType rd _ -> put rd (runIdentity (stateWbRes st)) prior
        IType Jump rd _ _ -> put rd (runIdentity (stateWbRes st)) prior
        UType _ rd _ -> put rd (runIdentity (stateWbRes st)) prior
        _ -> prior

    applyMe prior =
      case stateMeInstr st of
        RType _ rd _ _ -> put rd (runIdentity (stateMeRes st)) prior
        IType (Arith _) rd _ _ -> put rd (runIdentity (stateMeRes st)) prior
        IType (Load size sign) rd _ _ ->
          put
            rd
            (loadExtend size sign (memReadWord (stateMeAddr st) mem))
            prior
        JType rd _ ->
          if jumpsWriteRdInMe cfg
            then put rd (runIdentity (stateMeRes st)) prior
            else prior
        IType Jump rd _ _ ->
          if jumpsWriteRdInMe cfg
            then put rd (runIdentity (stateMeRes st)) prior
            else prior
        UType _ rd _ -> put rd (runIdentity (stateMeRes st)) prior
        _ -> prior

-- | Side condition: no store aliases a word the fetch path is using.
--
-- This is an ASSUMPTION, not part of the invariant: RISC-V requires a FENCE.I
-- between writing an instruction and executing it, so a store that rewrites an
-- instruction already in flight is out of spec. The proof obligations assume
-- it of every state of a hop, which rules such transitions out rather than
-- obliging us to show none can arise. (It could not be part of the invariant:
-- it is not preserved -- a store in the execute stage becomes the memory-stage
-- store next cycle with an address nothing constrains.)
--
-- The overlap check must be wrap-correct: 'Address' is 'Unsigned 32', and the
-- original interval form @a < p + 4 && p < a + n@ silently admitted stores
-- into (or wrapping into) PC words near the top of the address space -- which
-- is exactly what the last @k = 0@ counterexample exploited (see
-- @counterexample-k0.txt@ and the wrap-around tests in @ProofSpec@). Byte @x@
-- lies in the word starting at @p@ iff @x - p < 4@ in wrapping arithmetic.
-- Written without lists or folds so Pantomime can execute it.
noStoreAlias :: SysG r m -> Bool
noStoreAlias (Sys st _ _) =
  case stateMeInstr st of
    SType size _ _ _ ->
      let a = stateMeAddr st
          inWordAt p x = x - p < 4
          hitsPc x =
            inWordAt (stateExPc st) x
              || inWordAt (stateDePc st) x
              || inWordAt (stateFePc st) x
          clash = case size of
            Types.Byte -> hitsPc a
            Types.Half -> hitsPc a || hitsPc (a + 1)
            Types.Word -> hitsPc a || hitsPc (a + 1) || hitsPc (a + 2) || hitsPc (a + 3)
       in P.not clash
    _ -> True
