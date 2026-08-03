-- | The simulator: a machine that sees only the leakage and still reproduces
-- everything the attacker can see.
--
-- It is the unmodified 'Core.circuit', run on a censored state and fed
-- instruction words made up from the leakage. Per hop:
--
--   1. 'installLeak' puts @'Proof.Leakage.Model.invWord' l@ on the bus.
--   2. 'Proof.Driver.driver' picks the hop length from the censored state, and
--      the core runs that many cycles, every instruction fetch answered with
--      the same word.
--   3. 'scrub' normalises the state for the next hop.
--
-- The hop length is /derived/, not supplied. That is the point: the number of
-- cycles an instruction takes is exactly the timing an attacker sees, so a
-- simulator that were told it would be proving nothing.
--
-- 'proj' is the refinement relation the proof preserves. Its architectural half
-- is 'archOfLeak' and its simulator half is 'censor'.
module Proof.Leakage.Simulator
  ( -- * Simulator states
    SimSys,
    censor,
    censorEx,
    censorPast,
    exLeak,
    installJump,
    scrub,
    simEq,

    -- * The refinement relation
    proj,
    archOfLeak,
    isaNext,

    -- * The two machines
    implHop,
    simHop,
    leakSimHop,
    stepSimOut,
    installLeak,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Data.Monoid (getFirst)
import Instruction
import Proof.Driver (driver)
import Proof.Functional.Obligation (isStartupShape, isaOfHop)
import Proof.ISAStep
import Proof.Leakage.Model
import Proof.Machine
import RegFile
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- Censoring --------------------------------------------------------------------

-- | The simulator's state: a core state with the secrets removed.
--
-- The memory slot is @()@ -- the simulator has no memory, and answers
-- instruction fetches from the leakage and data reads with zero.
-- 'Proof.Driver.driver' is polymorphic in the memory type, so it runs on this
-- unchanged.
type SimSys r = SysG r ()

-- | Remove the secrets from a core state.
--
-- Kept: the three program counters (they are the observable fetch addresses),
-- the halt state, the 'Core.inputIsInstr' flag, and the /class/ of each
-- pipeline instruction.
--
-- Removed: the register file, both result registers, the memory-stage address,
-- and the instruction word on the bus. The control lines are reset rather than
-- copied, because 'Core.withCtrlReset' rewrites them before any stage reads
-- them and they carry nothing between cycles.
censor :: (RegFileOps r) => SysG r m -> SimSys r
censor sys@(Sys st inp _) =
  installJump (jumpSource =<< exLeak sys) $
    Sys
      { sysState =
          State
            { stateFePc = stateFePc st,
              stateDePc = stateDePc st,
              stateExPc = stateExPc st,
              stateExInstr = censorEx sys,
              stateMeInstr = censorPast (stateMeInstr st),
              stateMeRes = Identity 0,
              stateMeAddr = 0,
              stateWbInstr = censorPast (stateWbInstr st),
              stateWbRes = Identity 0,
              stateRegFile = initRFg,
              stateCtrl = initCtrl,
              stateHalt = stateHalt st,
              stateHaltPending = stateHaltPending st
            },
        sysInput = Input (inputIsInstr inp) (Identity 0),
        sysMem = ()
      }

-- | The leakage of the execute-stage instruction, or 'Nothing' when that stage
-- holds a stall @Nop@.
--
-- @Nop DecodeFail@ is not a stall: it is what an undecodable memory word
-- decodes to, so it is a real architectural instruction and is classified like
-- any other non-memory one. 'Proof.Machine.isBubble' draws the same line.
exLeak :: (RegFileOps r) => SysG r m -> Maybe L
exLeak sys = case exInstr sys of
  Nop DecodeFail -> Just (L CPlain (Nothing, Nothing))
  Nop _ -> Nothing
  ir -> Just (L (coreClass sys ir) (mkDeps ir))

-- | Censor the execute-stage instruction.
--
-- This is the only stage whose instruction still has decisions to make, so it
-- is replaced by the representative of its leakage, with the class resolved
-- against the /core/ state -- forwarded operands and all.
--
-- Stall @Nop@s pass through verbatim: 'Core.decode' reads the reason back off
-- @ctrlExInstr@ to decide the follow-on stall, so it is live here.
censorEx :: (RegFileOps r) => SysG r m -> Instruction
censorEx sys = case exLeak sys of
  Nothing -> exInstr sys
  Just l -> inv l

-- | Censor a memory- or writeback-stage instruction.
--
-- These have no decisions left. 'Core.memory' and 'Core.writeback' consult them
-- only to decide whether to issue a memory access and of what width, which
-- register to forward, and which register to write, so only the memory class
-- and width need survive.
--
-- The destination register is dropped, unlike in 'censorEx'. No hazard check
-- reads it here -- 'Core.decode', 'Proof.Driver.loadHazardD' and the
-- invariant's @me->ex@ conjunct all look at the execute stage -- and dropping
-- it keeps a censored writeback from clobbering the value 'installJump' parks
-- in the register file. With @rd == 0@ the write is a no-op and
-- 'Core.regWithFwd' ignores the forwarding line.
--
-- Idempotent on its own image, which is what lets 'scrub' apply it to
-- instructions the simulator produced itself.
censorPast :: Instruction -> Instruction
censorPast ir = case ir of
  Nop DecodeFail -> inv (L CPlain (Nothing, Nothing))
  Nop reason -> Nop reason
  IType (Load size _) _ _ _ -> inv (L (CLoad size 0) (mkDeps ir))
  SType size _ _ _ -> inv (L (CStore size) (mkDeps ir))
  _ -> inv (L CPlain (mkDeps ir))

-- | Park a leaked jump target in the register file, where 'Core.execute' will
-- read it back.
--
-- The register file is what 'Core.execute' reads, because in a censored state
-- neither forwarding line can fire: 'censorPast' gives every memory- and
-- writeback-stage instruction @rd == 0@, and 'Core.regWithFwd' ignores @x0@.
--
-- The value lives for exactly one hop -- the next 'censor' or 'scrub' rebuilds
-- the register file -- and during that hop the only register-reading
-- instruction in the execute stage is the @jalr@ that needs it.
installJump :: (RegFileOps r) => Maybe (RegIdx, Address) -> SimSys r -> SimSys r
installJump Nothing ss = ss
installJump (Just (src, target)) (Sys st inp m) =
  Sys
    st {stateRegFile = modifyRFg src (Identity (pack target)) (stateRegFile st)}
    inp
    m

-- | Normalise the simulator's state at a hop boundary.
--
-- Two jobs. First, discard dead state: the register file, both result
-- registers, the memory-stage address and the parked bus word. Every
-- instruction 'inv' emits reads only registers held at zero, writes either
-- @x0@ or a load result that is zero, and never lets a result value reach an
-- observation, so 'censor' can zero them on the implementation side and the
-- two still agree.
--
-- Second, re-censor the memory- and writeback-stage instructions. 'censorEx'
-- resolves an execute-stage branch to a taken or untaken representative, and
-- the simulator carries that form down the pipeline; 'censor', looking at the
-- implementation one hop later, sees the same instruction in the memory stage
-- and cannot recover the outcome, so it uses the plain representative. Running
-- 'censorPast' on both sides reconciles them.
scrub :: (RegFileOps r) => L -> SimSys r -> SimSys r
scrub l (Sys st inp _) =
  installJump parked $
    Sys
      { sysState =
          st
            { stateMeInstr = censorPast (stateMeInstr st),
              stateWbInstr = censorPast (stateWbInstr st),
              stateMeRes = Identity 0,
              stateWbRes = Identity 0,
              stateMeAddr = 0,
              stateRegFile = initRFg,
              stateCtrl = initCtrl
            },
        sysInput = Input (inputIsInstr inp) (Identity 0),
        sysMem = ()
      }
  where
    -- The instruction that just reached the execute stage is the one this hop
    -- injected, so its leakage is @l@ -- unless it was squashed or the core
    -- halted, in which case that stage holds a @Nop@ and nothing is parked.
    parked
      | stateExInstr st == inv l = jumpSource l
      | otherwise = Nothing

-- | Structural equality on simulator states, at one witness register.
--
-- 'SysG' has an 'Eq' instance, but it needs @'Eq' (r 'Identity')@, which the
-- SMT-array register file does not have; the register file is compared
-- pointwise instead, as 'Proof.Functional.Invariant.invAtFree' does. Quantifying
-- over the witness recovers full equality.
--
-- The control lines are not compared: 'Core.withCtrlReset' resets them at the
-- start of every cycle, so they carry nothing between hops.
simEq :: (RegFileOps r) => RegIdx -> SimSys r -> SimSys r -> Bool
simEq wr (Sys a ia _) (Sys b ib _) =
  stateFePc a == stateFePc b
    && stateDePc a == stateDePc b
    && stateExPc a == stateExPc b
    && stateExInstr a == stateExInstr b
    && stateMeInstr a == stateMeInstr b
    && stateMeAddr a == stateMeAddr b
    && stateWbInstr a == stateWbInstr b
    && runIdentity (stateMeRes a) == runIdentity (stateMeRes b)
    && runIdentity (stateWbRes a) == runIdentity (stateWbRes b)
    && stateHalt a == stateHalt b
    && stateHaltPending a == stateHaltPending b
    && inputIsInstr ia == inputIsInstr ib
    && runIdentity (lookupRFg wr (stateRegFile a)) == runIdentity (lookupRFg wr (stateRegFile b))
    && runIdentity (inputMem ia) == runIdentity (inputMem ib)

-- The refinement relation ------------------------------------------------------

-- | 'Proof.ISAStep.isaStep' made total: a halted ISA stands still.
isaNext :: (RegFileOps r, MemOps m) => IsaStateG r m -> IsaStateG r m
isaNext a = case isaStep a of
  Next a' -> a'
  IsaHalted -> a

-- | The architectural state a core state corresponds to, for the leakage proof.
--
-- Fetch-aligned: the instruction at its PC is the one the pipeline is /taking
-- in/, not the one in the execute stage. 'Proof.Functional.Obligation.isaOfHop'
-- is execute-aligned, one instruction behind, so a single
-- 'Proof.ISAStep.isaStep' converts between them.
--
-- The alignment is forced. The simulator's only channel into the core is the
-- instruction word on the bus, and the invariant pins that word to
-- @mem[dePc] == mem[isaPc + 4]@ -- the instruction after the one in execute. So
-- the leakage a hop consumes must describe that one.
--
-- At reset nothing is in flight and the PC comes off the fetch stage, exactly as
-- 'Proof.Functional.Obligation.isaOfHop' already does there.
archOfLeak :: (RegFileOps r, MemOps m) => SysG r m -> IsaStateG r m
archOfLeak sys
  | isStartupShape sys = isaOfHop sys
  | otherwise = isaNext (isaOfHop sys)

-- | The refinement relation: an architectural state paired with a censored core.
proj :: (RegFileOps r, MemOps m) => SysG r m -> (IsaStateG r m, SimSys r)
proj sys = (archOfLeak sys, censor sys)

-- The two machines -------------------------------------------------------------

-- | One simulator cycle.
--
-- The shape of 'Proof.Machine.stepSysOut', except that there is no memory to
-- service: an instruction fetch is answered with the leaked word, a data read
-- with zero, and a write with zero exactly as 'Proof.Machine.stepSys' does.
stepSimOut :: (RegFileOps r) => Word -> SimSys r -> (SimSys r, Output Identity)
stepSimOut w (Sys s i _) =
  let (s', o) = Core.circuit s i
      i' = case getFirst (outMem o) of
        Just (MemAccess isInstr _ _ Nothing) ->
          Input isInstr (Identity (if isInstr then w else 0))
        Just (MemAccess isInstr _ _ (Just _)) -> Input isInstr (Identity 0)
        Nothing -> Input False (Identity 0)
   in (Sys s' i' (), o)

-- | Put the leaked instruction word on the simulator's bus.
--
-- Only onto an instruction fetch. When 'Core.inputIsInstr' is 'False' the bus
-- carries the data for a load in the writeback stage, and 'Core.writeback'
-- would sign-extend the instruction word into a register. The censored value
-- there is zero and stays zero; the leaked word reaches 'Core.decode' one cycle
-- later through 'stepSimOut'.
installLeak :: Word -> SimSys r -> SimSys r
installLeak word (Sys s i m)
  | inputIsInstr i = Sys s (Input True (Identity word)) m
  | otherwise = Sys s i m

-- | The implementation, run for one driver hop, with the observation of each
-- cycle.
--
-- A four-way case rather than a loop: @driver sys@ is symbolic and Pantomime
-- cannot unroll a symbolic count.
implHop :: (RegFileOps r, MemOps m) => SysG r m -> (SysG r m, HopObs)
implHop sys = case driver sys of
  0 ->
    let (s1, o1) = stepSysOut sys
     in (s1, HopObs (Just (obsOf o1)) Nothing Nothing Nothing)
  1 ->
    let (s1, o1) = stepSysOut sys
        (s2, o2) = stepSysOut s1
     in (s2, HopObs (Just (obsOf o1)) (Just (obsOf o2)) Nothing Nothing)
  2 ->
    let (s1, o1) = stepSysOut sys
        (s2, o2) = stepSysOut s1
        (s3, o3) = stepSysOut s2
     in (s3, HopObs (Just (obsOf o1)) (Just (obsOf o2)) (Just (obsOf o3)) Nothing)
  _ ->
    let (s1, o1) = stepSysOut sys
        (s2, o2) = stepSysOut s1
        (s3, o3) = stepSysOut s2
        (s4, o4) = stepSysOut s3
     in (s4, HopObs (Just (obsOf o1)) (Just (obsOf o2)) (Just (obsOf o3)) (Just (obsOf o4)))

-- | The simulator, run for one hop.
--
-- The leaked word goes on the bus before the hop length is asked for, because
-- 'Proof.Driver.driver' reads it: a load-use hazard between the incoming
-- instruction and a load in the execute stage is one of the things that decides
-- how long the hop is.
simHop :: (RegFileOps r) => SimSys r -> L -> (SimSys r, HopObs)
simHop ss l = (scrub l ss', o)
  where
    w = invWord l
    ss0 = installLeak w ss
    (ss', o) = case driver ss0 of
      0 ->
        let (s1, o1) = stepSimOut w ss0
         in (s1, HopObs (Just (obsOf o1)) Nothing Nothing Nothing)
      1 ->
        let (s1, o1) = stepSimOut w ss0
            (s2, o2) = stepSimOut w s1
         in (s2, HopObs (Just (obsOf o1)) (Just (obsOf o2)) Nothing Nothing)
      2 ->
        let (s1, o1) = stepSimOut w ss0
            (s2, o2) = stepSimOut w s1
            (s3, o3) = stepSimOut w s2
         in (s3, HopObs (Just (obsOf o1)) (Just (obsOf o2)) (Just (obsOf o3)) Nothing)
      _ ->
        let (s1, o1) = stepSimOut w ss0
            (s2, o2) = stepSimOut w s1
            (s3, o3) = stepSimOut w s2
            (s4, o4) = stepSimOut w s3
         in (s4, HopObs (Just (obsOf o1)) (Just (obsOf o2)) (Just (obsOf o3)) (Just (obsOf o4)))

-- | Specification and simulator, composed.
--
-- The architectural state steps, and the simulator is handed the leakage of the
-- instruction that state is currently processing. Nothing here mentions the
-- pipeline: the leakage is @'leakOf' a@, a function of the architectural state
-- alone. That is the point of the construction -- an attacker model that can be
-- stated without the processor in it.
--
-- A closed machine, unlike its counterpart in the @highlevel-leakage@
-- development: AIMCore's ISA reads its instruction out of its own memory, so
-- the architectural state is all the input there is.
leakSimHop ::
  (RegFileOps r, MemOps m) =>
  (IsaStateG r m, SimSys r) ->
  ((IsaStateG r m, SimSys r), HopObs)
leakSimHop (a, ss) = ((isaNext a, ss'), o)
  where
    (ss', o) = simHop ss (leakOf a)
