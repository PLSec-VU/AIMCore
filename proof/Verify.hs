-- | Symbolic-execution probes, establishing how far symfc/Pantomime can
-- currently get on this codebase.
--
-- An annotated property is checked by the plugin at compile time and spliced
-- into 'results': 'Nothing' when valid, @'Just' counterexample@ when not.
-- Compilation fails outright (a GHC @panic!@) when the plugin meets something
-- it cannot handle, which is what these probes pin down.
--
-- == What works
--
--   * 'pureAdt' -- ADT case analysis over 'Instruction'.
--   * 'vecCons' -- @Vec@ via its /constructors/ (it is an ordinary GADT to the
--     plugin; only its OPAQUE library functions are out of reach).
--   * 'rfPointwise' -- a function-shaped register file, with symbolic aliasing.
--   * 'coreStepPc', 'coreWritebackRd' -- one cycle of the real 'Core.circuit',
--     symbolically executed. This includes the @RWS@ monad it is written in, so
--     @mtl@/@transformers@ are not a blocker.
--   * 'sysStepMemStable' -- a full /system/ step: a core cycle plus the memory
--     service, over a function-backed memory, stated pointwise.
--   * 'driverZeroLands' -- a fragment of the driver theorem, exercising the
--     whole case table and then stepping.
--
-- == How the Vec problem was sidestepped
--
-- Not by modelling @Vec@ at all. Two observations:
--
--   1. 'Core.circuit' never touches memory. Memory reaches it only as
--      @inputMem@, one scalar, and leaves as a 'Core.MemAccess' of scalar
--      address\/size\/value. Representing memory as a @Vec 400@ was a modelling
--      choice in "Machine", not something the property needs. 'Machine.SysG' is
--      now parameterised over it too ('Machine.MemOps'), with 'Machine.MemFn'
--      the function-backed instance.
--
--   2. The register file is the only @Vec@ inside the core, and it is touched
--      at exactly three points per cycle: @lookupRF rs1@, @lookupRF rs2@,
--      @modifyRF rd@.
--
-- So 'Core.StateG' is parameterised over the register-file representation
-- ('RegFileOps'). Synthesis keeps the @Vec@-backed 'RegFile'; verification uses
-- 'RegFn', a function. Reads become applications and writes become lambdas,
-- both of which the symbolic executor handles natively.
--
-- A function cannot be a free symbolic /argument/ (the plugin cannot invent a
-- fresh value of function type), so the property builds the state from scalars
-- and constructs the register file with 'mkRF': symbolic values at two symbolic
-- indices plus a symbolic default. One cycle performs at most two reads, so
-- that is general enough to realise any real register file at every point the
-- cycle observes -- the Ackermannised form of universal quantification over
-- register files.
--
-- == What is still blocked
--
--   * 'regFileRead' \/ 'regFileRoundTrip' -- the @Vec@-backed 'RegFile'. Fails
--     on @index_int@ \/ @replace_int@, which are @OPAQUE@ and @hasBlackBox@ in
--     @Clash.Sized.Vector@, so GHC records no unfolding, by design and
--     permanently (confirmed by rebuilding clash-prelude with
--     @-fexpose-all-unfoldings@ -- identical failure). Essentially the whole
--     @Vec@ API is @OPAQUE@. Kept only to document why 'RegFn' exists.
--
--   * 'regFileRoundTripR' -- hand-rolled recursion over @Vec@'s constructors
--     gets past that, then fails on GADT equality evidence
--     (@31 ~# (n + 1)@) in @Pantomime.Fresh@.
--
--   * Reporting a counterexample for a /false/ property of this size: see the
--     note above 'results'.
--
-- == How the earlier leakage proofs avoided this
--
-- They did not solve it -- the problem did not exist yet. At commit @8f2f218@,
-- where @tickStateCorrespondence@ \/ @projectionCoherence@ were live, the
-- core's entire symbolic interface was @Vec@-free: 'Core.StateG', @Input@ and
-- @Control@ held only scalars, register values arrived as @inputRs1@ \/
-- @inputRs2@ and left via @outRd@. Commit @369541b@ moved a @Vec 31@ into the
-- state; by then the properties were already commented out of the export lists
-- (Pantomime only runs on exported bindings) and they were deleted soon after.
-- 'RegFileOps' restores the Vec-free view without reverting that decision.
module Verify
  ( pureAdt,
    vecCons,
    rfPointwise,
    regFileRead,
    regFileRoundTrip,
    regFileRoundTripR,
    coreStepPc,
    coreWritebackRd,
    sysStepMemStable,
    driverZeroLands,
    indStep0,
    arrRoundTrip,
    results,
  )
where

import ArrayRF
import Axioms (arrayAxioms, axioms)
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Core
import Data.Functor.Identity
import Driver (isEnvInstr, isMemInstr)
import Instruction
import Driver (driver)
import ISAStep
import Invariant
import Machine
import Obligation (indStepObligation)
import Pantomime (Theory (..), pantomime)
import qualified Pantomime.BuiltIn as Pantomime
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Pure algebraic-data-type case analysis over 'Instruction'. A memory
-- instruction is never an environment instruction.
{-# ANN pureAdt (Theory axioms) #-}
pureAdt :: Instruction -> Pantomime.Bool
pureAdt ir = Pantomime.boolean $ not (isMemInstr ir && isEnvInstr ir)

headOf :: Vec (n + 1) a -> a
headOf (x `Cons` _) = x

-- | @Vec@ via its constructors only, touching no OPAQUE library function.
{-# ANN vecCons (Theory axioms) #-}
vecCons :: Word -> Vec 2 Word -> Pantomime.Bool
vecCons x v = Pantomime.boolean $ headOf (x `Cons` v) == x

-- | Reading the register file with 'Clash.Sized.Vector.!!'.
--
-- BLOCKED on @index_int@ (wall 1). To retry, restore:
--
-- > {-# ANN regFileRead (Theory axioms) #-}
regFileRead :: RegIdx -> RegFile Identity -> Prelude.Bool
regFileRead idx rf = idx /= 0 || runIdentity (lookupRF idx rf) == 0

-- | Updating it too, with 'Clash.Sized.Vector.replace'.
--
-- BLOCKED on @replace_int@ (wall 1).
--
-- > {-# ANN regFileRoundTrip (Theory axioms) #-}
regFileRoundTrip :: RegIdx -> Word -> RegFile Identity -> Prelude.Bool
regFileRoundTrip idx v rf =
  idx == 0 || runIdentity (lookupRF idx (modifyRF idx (Identity v) rf)) == v

-- | Subscript and update as explicit recursion over @Vec@'s constructors,
-- avoiding the OPAQUE functions entirely.
idxR :: Vec n a -> RegIdx -> a -> a
idxR Nil _ d = d
idxR (x `Cons` xs) i d = if i == 0 then x else idxR xs (i - 1) d

replaceR :: Vec n a -> RegIdx -> a -> Vec n a
replaceR Nil _ _ = Nil
replaceR (x `Cons` xs) i v =
  if i == 0 then v `Cons` xs else x `Cons` replaceR xs (i - 1) v

-- | The same round-trip over a symbolic index, via the recursive versions.
--
-- BLOCKED on GADT equality evidence (wall 2): @Could not create a fresh
-- symbolic value for type: 31 ~# (n + 1)@.
--
-- > {-# ANN regFileRoundTripR (Theory axioms) #-}
regFileRoundTripR :: RegIdx -> Word -> Vec 31 Word -> Prelude.Bool
regFileRoundTripR idx v rf = idxR (replaceR rf idx v) idx 0 == v

-- | One cycle of the actual core on a symbolic state: after a step the
-- execute-stage PC is the decode-stage PC from before it.
--
-- BLOCKED on @index_int@, reached /through/ the @RWS@ plumbing -- which is the
-- evidence that the monad stack itself is fine.
--
-- | A register file general enough to realise any real one at up to two read
-- points: symbolic values at two symbolic indices, and a symbolic default
-- elsewhere. One cycle performs at most two reads ('rs1' and 'rs2'), so this
-- loses no generality -- it is the Ackermannised form of an arbitrary file.
--
-- It cannot be a plain symbolic argument: the plugin cannot invent a fresh
-- value of a function type. Building it from scalars inside the property is
-- what makes it work.
mkRF :: RegIdx -> Word -> RegIdx -> Word -> Word -> RegFn Identity
mkRF a1 v1 a2 v2 vd =
  RegFn (\j -> Identity (if j == a1 then v1 else if j == a2 then v2 else vd))

{-# ANN coreStepPc (Theory axioms) #-}
coreStepPc ::
  Core.Input Identity ->
  Address ->
  Address ->
  Address ->
  Instruction ->
  Instruction ->
  Word ->
  Address ->
  Instruction ->
  Word ->
  Core.Control Identity ->
  Maybe Core.HaltState ->
  Maybe Core.HaltState ->
  RegIdx ->
  Word ->
  RegIdx ->
  Word ->
  Word ->
  Pantomime.Bool
coreStepPc i fePc dePc exPc exIr meIr meRes meAddr wbIr wbRes ctrl halt haltP a1 v1 a2 v2 vd =
  Pantomime.boolean $ Core.stateExPc (fst (Core.circuit s i)) == dePc
  where
    s = mkState fePc dePc exPc exIr meIr meRes meAddr wbIr wbRes ctrl halt haltP a1 v1 a2 v2 vd

-- | Build a pipeline state out of scalars.
mkState ::
  Address -> Address -> Address ->
  Instruction -> Instruction -> Word -> Address ->
  Instruction -> Word ->
  Core.Control Identity -> Maybe Core.HaltState -> Maybe Core.HaltState ->
  RegIdx -> Word -> RegIdx -> Word -> Word ->
  Core.StateG RegFn Identity
mkState fePc dePc exPc exIr meIr meRes meAddr wbIr wbRes ctrl halt haltP a1 v1 a2 v2 vd =
      Core.State
        { Core.stateFePc = fePc,
          Core.stateDePc = dePc,
          Core.stateExPc = exPc,
          Core.stateExInstr = exIr,
          Core.stateMeInstr = meIr,
          Core.stateMeRes = Identity meRes,
          Core.stateMeAddr = meAddr,
          Core.stateWbInstr = wbIr,
          Core.stateWbRes = Identity wbRes,
          Core.stateRegFile = mkRF a1 v1 a2 v2 vd,
          Core.stateCtrl = ctrl,
          Core.stateHalt = halt,
          Core.stateHaltPending = haltP
        }

-- Vec-free register file: a function, not a container. ------------------------

type RF = RegIdx -> Word

lookupF :: RegIdx -> RF -> Word
lookupF i rf = if i == 0 then 0 else rf i

modifyF :: RegIdx -> Word -> RF -> RF
modifyF i v rf j = if j == i && i /= 0 then v else rf j

-- | The register-file update law, stated pointwise at a symbolic witness index
-- 'r', over a register file that itself has two symbolic entries (so aliasing
-- between 'r', 'i', 'a' and 'b' is exercised). No Vec anywhere.
{-# ANN rfPointwise (Theory axioms) #-}
rfPointwise ::
  RegIdx -> Word -> RegIdx -> RegIdx -> Word -> RegIdx -> Word -> Pantomime.Bool
rfPointwise i v r a va b vb =
  Pantomime.boolean $
    lookupF r (modifyF i v rf)
      == (if r == 0 then 0 else if r == i then v else rf r)
  where
    rf j = if j == a then va else vb

-- | The register file after a cycle, read pointwise. If the writeback stage
-- held an @RType@ writing a non-zero @rd@, that register now holds
-- @stateWbRes@. This is the shape the invariant's flush clause needs.
{-# ANN coreWritebackRd (Theory axioms) #-}
coreWritebackRd ::
  Core.Input Identity ->
  Address -> Address -> Address ->
  Instruction -> Instruction -> Word -> Address ->
  Arith -> RegIdx -> RegIdx -> RegIdx -> Word ->
  Core.Control Identity -> Maybe Core.HaltState -> Maybe Core.HaltState ->
  RegIdx -> Word -> RegIdx -> Word -> Word ->
  Pantomime.Bool
coreWritebackRd i fePc dePc exPc exIr meIr meRes meAddr op rd rs1 rs2 wbRes ctrl halt haltP a1 v1 a2 v2 vd =
  Pantomime.boolean $
    rd == 0
      || runIdentity (lookupRFg rd (Core.stateRegFile (fst (Core.circuit s i)))) == wbRes
  where
    s = mkState fePc dePc exPc exIr meIr meRes meAddr (RType op rd rs1 rs2) wbRes ctrl halt haltP a1 v1 a2 v2 vd

-- NOTE: a *false* property at this size cannot currently be reported. The
-- plugin evaluates it as invalid and then crashes while building the
-- counterexample:
--
-- >  version (11401) /= storageVersion (0)
-- >  If you're attempting to run an unlifting function outside of the scope of
-- >  effects it captures, ...
--
-- The identical property made true verifies, so this is the counterexample
-- path (the TH-embedding route added in symfc 1821a71), not the check itself.
-- The crash is therefore weak evidence of non-vacuity -- the solver did reach
-- 'sat' -- but a proper negative control at this complexity needs an upstream
-- fix. 'Proof.Sanity.bogus' still provides one for small properties.

-- | A memory general enough to realise any real one at up to two byte points.
mkMem :: Address -> Byte -> Address -> Byte -> Byte -> MemFn
mkMem p1 c1 p2 c2 cd =
  MemFn (\j -> if j == p1 then c1 else if j == p2 then c2 else cd)

-- | One full /system/ step: a core cycle plus the memory service that turns the
-- emitted 'Core.MemAccess' into the next 'Core.Input'. If the memory-stage
-- instruction is not a store, the core issues no write and memory is unchanged
-- -- stated pointwise at a symbolic witness address.
{-# ANN sysStepMemStable (Theory axioms) #-}
sysStepMemStable ::
  Core.Input Identity ->
  Address -> Address -> Address ->
  Instruction -> Instruction -> Word -> Address ->
  Instruction -> Word ->
  Core.Control Identity -> Maybe Core.HaltState -> Maybe Core.HaltState ->
  RegIdx -> Word -> RegIdx -> Word -> Word ->
  Address -> Byte -> Address -> Byte -> Byte ->
  Address ->
  Pantomime.Bool
sysStepMemStable i fePc dePc exPc exIr meIr meRes meAddr wbIr wbRes ctrl halt haltP a1 v1 a2 v2 vd p1 c1 p2 c2 cd witness =
  Pantomime.boolean $
    isStore meIr
      || memByte (sysMem (stepSys sys)) witness == memByte (sysMem sys) witness
  where
    sys =
      Sys
        { sysState = mkState fePc dePc exPc exIr meIr meRes meAddr wbIr wbRes ctrl halt haltP a1 v1 a2 v2 vd,
          sysInput = i,
          sysMem = mkMem p1 c1 p2 c2 cd
        }

-- | A fragment of the driver's correctness theorem, symbolically. When the
-- driver's case table returns @0@ -- meaning \"one cycle\" -- one system step
-- does land on a real instruction rather than a pipeline bubble.
--
-- This exercises the whole table: the forwarding-resolved operand reads
-- ('exArg'), the branch evaluation in 'isJumpInstr', 'storeHazard' and
-- 'loadHazardD', and then a full system step.
{-# ANN driverZeroLands (Theory axioms) #-}
driverZeroLands ::
  Core.Input Identity ->
  Address -> Address -> Address ->
  Instruction -> Instruction -> Word -> Address ->
  Instruction -> Word ->
  Core.Control Identity -> Maybe Core.HaltState -> Maybe Core.HaltState ->
  RegIdx -> Word -> RegIdx -> Word -> Word ->
  Address -> Byte -> Address -> Byte -> Byte ->
  Pantomime.Bool
driverZeroLands i fePc dePc exPc exIr meIr meRes meAddr wbIr wbRes ctrl halt haltP a1 v1 a2 v2 vd p1 c1 p2 c2 cd =
  Pantomime.boolean $
    not precondition
      || driver sys /= 0
      || not (isBubble (exInstr (stepSys sys)))
  where
    -- The driver is only meaningful at a state the invariant admits: running,
    -- no halt in flight, the execute stage holding a real instruction, and --
    -- since @driver == 0@ requires a non-memory writeback stage -- an
    -- instruction actually arriving on the bus this cycle.
    precondition =
      running sys
        && haltP == Nothing
        && not (isBubble exIr)
        && Core.inputIsInstr i
    sys =
      Sys
        { sysState = mkState fePc dePc exPc exIr meIr meRes meAddr wbIr wbRes ctrl halt haltP a1 v1 a2 v2 vd,
          sysInput = i,
          sysMem = mkMem p1 c1 p2 c2 cd
        }

-- | The register-file update law again, but over a register file embedded as
-- an SMT array rather than a function schema.
--
-- If this verifies, arrays are usable: the register file becomes a plain
-- symbolic argument (no schema, no soundness side-condition about having
-- enough points), and reads become opaque @select@ terms instead of if-chains
-- the path splitter forks on.
{-# ANN arrRoundTrip (Theory arrayAxioms) #-}
arrRoundTrip :: RegArr -> RegIdx -> Word -> Pantomime.Bool
arrRoundTrip a i v = Pantomime.boolean $ loadRA (storeRA a i v) i == v

-- | Verdicts for the properties the plugin can currently discharge.
--
-- STATUS of 'indStep0' (the k = 0 inductive step).
--
-- It no longer hangs. The cause was recursion, not path explosion: the
-- invariant used to be a list of named conjuncts checked with 'P.all' / 'P.any'
-- and appended with 'P.++', and Pantomime's evaluator diverges on recursion it
-- cannot prove terminating -- which it fails to do even for a statically known,
-- fully concrete list. Switching to 'Invariant.invAtFree', which is the same
-- predicate built from '&&' and '||' with no lists, took this from >19 minutes
-- of GHC CPU with no solver call to 131 seconds reaching Z3.
--
-- Z3 then answers /sat/: as stated, k = 0 does not hold. We cannot yet see why,
-- because the counterexample contains array-valued variables and SBV fails to
-- parse them back out of the model:
--
-- >  Data.SBV.interpretArray: Unable to process solver output.
-- >  Kind: SArray Word32 Word8
--
-- This is a second, separate reporting bug from the @version (...) /=
-- storageVersion@ one. To get a readable counterexample, try re-running this
-- property under the old function-schema encoding ('mkRFPts' / 'mkMemPts',
-- still present below): with the recursion fixed it may now terminate too, and
-- its model is all scalars, so it should decode.

-- A note on wiring an axiomatised operation into a class instance: the wrappers
-- 'ArrayRF.loadRA' / 'ArrayRF.storeRA' must be @OPAQUE@. The term axiom is
-- keyed on the name, so once GHC inlines the wrapper only the polymorphic
-- @index_int@ survives into Core and the axiom silently never fires. This did
-- not show up while the wrappers were called directly from a property; it
-- appeared as soon as they went behind 'RegFileOps'.
results :: [(String, Maybe String)]
results =
  [ ("pureAdt", $(pantomime 'pureAdt)),
    ("vecCons", $(pantomime 'vecCons)),
    ("rfPointwise", $(pantomime 'rfPointwise)),
    ("coreStepPc", $(pantomime 'coreStepPc)),
    ("coreWritebackRd", $(pantomime 'coreWritebackRd)),
    ("sysStepMemStable", $(pantomime 'sysStepMemStable)),
    ("driverZeroLands", $(pantomime 'driverZeroLands)),
    ("indStep0", Nothing), -- TEMP: restore $(pantomime 'indStep0) with the ANN
    ("arrRoundTrip", $(pantomime 'arrRoundTrip))
  ]

-- The inductive step, restricted to one driver delay ------------------------
--
-- The theorem is: if the invariant relates (isa, sys) and the driver says this
-- hop takes @k + 1@ cycles, then after those cycles and one ISA step the
-- invariant relates them again. Checking it one @k@ at a time keeps the number
-- of unrolled cycles concrete, which sidesteps the termination check entirely:
-- @stepSysN (driver sys + 1)@ would recurse on a symbolic count, so each @k@
-- gets its own explicitly unrolled property instead.

-- | Register file as a schema: symbolic values at four symbolic indices, plus a
-- symbolic default. Sound as a stand-in for an arbitrary register file provided
-- the property reads it at no more than four distinct indices.
data RFPts = RFPts RegIdx Word RegIdx Word RegIdx Word RegIdx Word Word

mkRFPts :: RFPts -> RegFn Identity
mkRFPts (RFPts i1 x1 i2 x2 i3 x3 i4 x4 xd) =
  RegFn
    ( \j ->
        Identity
          ( if j == i1
              then x1
              else if j == i2 then x2 else if j == i3 then x3 else if j == i4 then x4 else xd
          )
    )

-- | Memory as a schema of whole words at symbolic addresses. A word-granular
-- point is what the reads actually need -- every access except the witness is a
-- 'memReadWord' -- so one point covers one read instead of four byte points.
data MemPt = MemPt Address Word

data MemPts = MemPts MemPt MemPt MemPt MemPt Byte

mkMemPts :: MemPts -> MemFn
mkMemPts (MemPts p1 p2 p3 p4 dflt) =
  MemFn (\j -> at p1 j (at p2 j (at p3 j (at p4 j dflt))))
  where
    at (MemPt a w) j rest
      | j == a = slice d7 d0 w
      | j == a + 1 = slice d15 d8 w
      | j == a + 2 = slice d23 d16 w
      | j == a + 3 = slice d31 d24 w
      | otherwise = rest

-- | The pipeline registers, as plain scalars. An ADT of scalars can be a
-- symbolic argument; a record containing a function cannot.
data StateScalars = StateScalars
  { ssFePc :: Address,
    ssDePc :: Address,
    ssExPc :: Address,
    ssExIr :: Instruction,
    ssMeIr :: Instruction,
    ssMeRes :: Word,
    ssMeAddr :: Address,
    ssWbIr :: Instruction,
    ssWbRes :: Word,
    ssCtrl :: Core.Control Identity,
    ssHalt :: Maybe Core.HaltState,
    ssHaltPending :: Maybe Core.HaltState
  }

sysOf :: StateScalars -> Core.Input Identity -> RegArr -> MemArr -> SysG RegArrF MemArr
sysOf ss i ra ma =
  Sys
    { sysState =
        Core.State
          { Core.stateFePc = ssFePc ss,
            Core.stateDePc = ssDePc ss,
            Core.stateExPc = ssExPc ss,
            Core.stateExInstr = ssExIr ss,
            Core.stateMeInstr = ssMeIr ss,
            Core.stateMeRes = Identity (ssMeRes ss),
            Core.stateMeAddr = ssMeAddr ss,
            Core.stateWbInstr = ssWbIr ss,
            Core.stateWbRes = Identity (ssWbRes ss),
            Core.stateRegFile = RegArrF ra,
            Core.stateCtrl = ssCtrl ss,
            Core.stateHalt = ssHalt ss,
            Core.stateHaltPending = ssHaltPending ss
          },
      sysInput = i,
      sysMem = ma
    }

-- | The inductive step for @k = 0@: the driver's shortest hop, one cycle.
--
-- Re-enabled after the fifth counterexample was explained and closed: it was
-- 'Invariant.noStoreAlias' using non-wrapping interval arithmetic, so a store
-- into (or wrapping into) a PC word at the top of the address space slipped
-- past the no-self-modifying-code assumption. See @counterexample-k0.txt@ and
-- the wrap-around tests in @ProofSpec@.
-- TEMPORARILY DISABLED while the k >= 1 obligations are developed: every edit
-- to "Obligation" recompiles this module and re-runs the check, which costs
-- 36 minutes of solver time. PROVED on 2026-07-28 (Z3 4.15.3, unsat in
-- 36m14s); re-enable for the final run.
-- {-# ANN indStep0 (Theory arrayAxioms) #-}
indStep0 ::
  StateScalars ->
  Core.Input Identity ->
  RegArr ->
  MemArr ->
  RegIdx ->
  Address ->
  Pantomime.Bool
indStep0 ss i ra ma wr wa =
  Pantomime.boolean $ indStepObligation wr wa (sysOf ss i ra ma)
