-- | The invariant from @proof/invariant.txt@, as code.
--
-- The invariant relates an architectural state @(isaPc, isaRegFile, isaMem)@ to
-- a system state @((core state), (input), mem)@. It is a disjunction of cases:
-- one for startup, four for the running core, and two for the halted core.
--
-- Each case is represented as a named list of conjuncts so that a failing check
-- can report precisely which clause broke, rather than just @False@.
module Invariant
  ( flushWbStage,
    flushMeStage,
    Case (..),
    InvConfig (..),
    proposed,
    literal,
    invCases,
    invCasesWith,
    invCasesAt,
    inv,
    invAt,
    invAtWith,
    invStrictCtrl,
    invLiteral,
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
import Data.Proxy (Proxy (..))
import Memory.Types
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | Apply the pending effect of the writeback-stage instruction.
--
-- Transcribed from @invariant.txt@. The load case reads the value off
-- @inputMem@, matching 'Core.writeback'.
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
-- 'flushWbStage'. A \"pending effect\" has to be counted identically wherever the
-- instruction sits in the pipe, since an instruction in the memory stage this
-- cycle is in the writeback stage the next one. @invariant.txt@ originally left
-- @rd@ alone here (binding it but never using it), and that asymmetry is what
-- broke inductiveness. 'False' reproduces the original for the regression test.
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

-- | How to read the two places where @invariant.txt@ is ambiguous or wrong.
data InvConfig = InvConfig
  { -- | Include the @stateCtrl == initCtrl@ clause. Unsatisfiable on any state
    -- reached by stepping the core, so normally 'False'.
    checkCtrl :: Bool,
    -- | Have 'flushMeStage' write @rd@ for jumps, matching 'flushWbStage'.
    jumpsWriteRdInMe :: Bool,
    -- | Require that no halt is in flight. Without it the core can halt
    -- part-way through a driver hop.
    checkHaltPending :: Bool,
    -- | Include the @ex == decode (mem[isaPc])@ conjunct.
    --
    -- Setting this 'False' /defers/ the conjunct rather than dropping it: the
    -- caller is expected to discharge it in the cheaper word-level form (see
    -- 'indStep0'). It exists because in a post-state this conjunct applies
    -- 'decode'' to a read at an address that is itself the result of an
    -- earlier 'decode'', and under full-path symbolic execution the two
    -- decision trees multiply.
    checkExDecode :: Bool,
    -- | Consider only the four running cases. Restricting the disjunction
    -- makes the resulting statement /stronger/, not weaker.
    runningCasesOnly :: Bool
  }

-- | The reading this module recommends.
proposed :: InvConfig
proposed = InvConfig {checkCtrl = False, jumpsWriteRdInMe = True, checkHaltPending = True, checkExDecode = True, runningCasesOnly = False}

-- | Exactly what @invariant.txt@ says.
literal :: InvConfig
literal = InvConfig {checkCtrl = True, jumpsWriteRdInMe = False, checkHaltPending = False, checkExDecode = True, runningCasesOnly = False}

-- | A named case of the invariant, with its conjuncts.
data Case = Case
  { caseName :: String,
    caseConjuncts :: [(String, Bool)]
  }

holds :: Case -> Bool
holds = P.all snd . caseConjuncts

-- | Does any case of the invariant hold?
--
-- This uses the invariant with its @stateCtrl == initCtrl@ clause dropped; see
-- 'invCasesWith' for why.
inv :: IsaState -> Sys -> Bool
inv isa sys = P.any holds (invCases isa sys)

-- | The invariant in pointwise form: instead of comparing whole register files
-- and memories, compare them at one witness register @wr@ and one witness byte
-- address @wa@. Quantifying over the witnesses recovers the container form.
--
-- This is the version symbolic execution can use: function-backed containers
-- have no decidable equality, but they can be read at a symbolic point.
invAt :: (RegFileOps r, MemOps m) => RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
invAt = invAtWith proposed

invAtWith ::
  (RegFileOps r, MemOps m) =>
  InvConfig -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> Bool
invAtWith cfg wr wa isa sys = P.any holds (invCasesAt cfg wr wa isa sys)

-- | The invariant exactly as @invariant.txt@ writes it, including the
-- @stateCtrl == initCtrl@ clause. This is unsatisfiable on any state reached by
-- stepping the core.
invStrictCtrl :: IsaState -> Sys -> Bool
invStrictCtrl isa sys = P.any holds (invCasesWith proposed {checkCtrl = True} isa sys)

-- | The invariant exactly as @invariant.txt@ writes it.
invLiteral :: IsaState -> Sys -> Bool
invLiteral isa sys = P.any holds (invCasesWith literal isa sys)

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

invCases :: IsaState -> Sys -> [Case]
invCases = invCasesWith proposed

-- | Container-level cases, for the concrete representation: compare register
-- files and memories directly.
invCasesWith :: InvConfig -> IsaState -> Sys -> [Case]
invCasesWith cfg = invCasesGen (==) (==) cfg

-- | The cases of the invariant, under a given reading, at one witness register
-- and one witness byte address.
invCasesAt ::
  (RegFileOps r, MemOps m) =>
  InvConfig -> RegIdx -> Address -> IsaStateG r m -> SysG r m -> [Case]
invCasesAt cfg wr wa =
  invCasesGen
    (\a b -> runIdentity (lookupRFg wr a) == runIdentity (lookupRFg wr b))
    (\a b -> memReadByte wa a == memReadByte wa b)
    cfg

-- | The cases of the invariant, parameterised over how register files and
-- memories are compared. The concrete representation compares them outright;
-- symbolic execution compares them at a witness point, since function-backed
-- containers have no decidable equality.
invCasesGen ::
  (RegFileOps r, MemOps m) =>
  (r Identity -> r Identity -> Bool) ->
  (m -> m -> Bool) ->
  InvConfig ->
  IsaStateG r m ->
  SysG r m ->
  [Case]
invCasesGen eqRF eqMem cfg (IsaState ipc irf imem) sys@(Sys st inp mem) =
  [ runningCase "running/wb-nomem/me-nomem" False False,
    runningCase "running/wb-mem/me-nomem" True False,
    runningCase "running/wb-nomem/me-mem" False True,
    runningCase "running/wb-mem/me-mem" True True
  ]
    P.++ if runningCasesOnly cfg
      then []
      else
        [ Case
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

    -- 'Core.execute' raises a pending halt, and 'Core.memory' consumes it on
    -- the next cycle. At a state the driver lands on there is never one in
    -- flight, and leaving it unconstrained would let the core halt spuriously
    -- part-way through a hop.
    noPendingHalt
      | checkHaltPending cfg = ("no halt pending", stateHaltPending st == Nothing)
      | otherwise = ("halt pending (not checked)", True)

    inputWord = runIdentity (inputMem inp)

    -- Bound once rather than recomputed inside each case. Under full-path
    -- symbolic execution a repeated 'decode'' is not shared, and it is a large
    -- decision tree, so recomputing it per case multiplies the path count.
    isaInstr = decode' (memReadWord ipc imem)

    exDecodeClause
      | checkExDecode cfg = ("ex == decode (mem[isaPc])", stateExInstr st == isaInstr)
      | otherwise = ("ex == decode (mem[isaPc]) [deferred]", True)
    dePcWord = memReadWord (stateDePc st) mem

    isMem ir = isLoad ir || isStore ir

    -- The invariant's four running cases share a common core and differ only
    -- in whether the writeback and memory stages hold memory instructions.
    -- The fetch behaviour is determined by the writeback stage: if the
    -- instruction now in writeback was a load/store, it occupied the bus last
    -- cycle, so no instruction was fetched.
    runningCase name wbMem meMem =
      Case name $
        [ ("running", running sys),
          ctrlClause,
          noPendingHalt,
          ("isMemInstr wb == " P.++ show wbMem, isMem (stateWbInstr st) == wbMem),
          ("isMemInstr me == " P.++ show meMem, isMem (stateMeInstr st) == meMem),
          ("exPc == isaPc", stateExPc st == ipc),
          exDecodeClause,
          ("dePc == exPc + 4", stateDePc st == stateExPc st + 4),
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
          P.++ if wbMem
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
