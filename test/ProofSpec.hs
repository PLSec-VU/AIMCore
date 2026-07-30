{-# LANGUAGE PackageImports #-}

module ProofSpec (proofTests, progs, alignedWalk, invTrace, invReport, genProg) where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Core
import Data.Functor.Identity (Identity (..), runIdentity)
import Driver
import ISAStep
import Instruction
import Invariant
import Machine
import Obligation (indStepObligation, indStepObligation1, indStepObligation2, isStartupShape, isaOfG, isaOfHop)
import Memory.Types
import RegFile
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | A state the driver is meant to be evaluated in: the execute stage holds a
-- real instruction. The startup state (@Nop FirstCycle@) is the one bubble the
-- driver has an explicit case for. Note @Nop DecodeFail@ is /not/ a bubble --
-- see 'Machine.isBubble'.
aligned :: Sys -> Bool
aligned s = not (isBubble (exInstr s)) || exInstr s == Nop FirstCycle

-- | Walk the machine from aligned state to aligned state, taking
-- @driver s + 1@ cycles each hop.
alignedWalk :: Int -> Vec PROG_SIZE Word -> [(Int, Int, Sys)]
alignedWalk k prog = go k 0 (initSys prog)
  where
    go 0 _ _ = []
    go j i s =
      let n = driver s + 1
          s' = stepSysN n s
       in (i, n, s') : if running s' then go (j - 1) (i + n) s' else []

-- | Hops that do not land on an aligned state.
walkReport :: Int -> Vec PROG_SIZE Word -> String
walkReport k prog =
  unlines
    [ P.concat
        [ "  from cycle ",
          show i,
          " took ",
          show n,
          " cycles -> ex=",
          show (exInstr s'),
          " exPc=",
          show (stateExPc (sysState s'))
        ]
      | (i, n, s') <- alignedWalk k prog,
        not (aligned s'),
        running s'
    ]

-- | The architectural state @prog@ starts in: execution begins at 'initPc', the
-- register file is zeroed, and memory holds the program. The counterpart of
-- 'Machine.initSys' on the ISA side.
initIsa :: Vec PROG_SIZE Word -> IsaState
initIsa prog = IsaState initPc initRF (mkRAM @PROG_SIZE @RAM_SIZE_BYTES prog)

-- | The driver and the ISA walked in lockstep.
--
-- The first hop (out of the startup state) brings the first instruction into
-- the execute stage without the ISA having executed anything, so the ISA state
-- is unchanged across it. Every later hop advances the ISA by one step.
invTrace :: Int -> Vec PROG_SIZE Word -> [(Int, IsaState, Sys)]
invTrace k prog = (0, isa0, sys0) : go k 0 isa0 sys0 True
  where
    isa0 = initIsa prog
    sys0 = initSys prog

    go 0 _ _ _ _ = []
    go j c isa sys firstHop =
      let n = driver sys + 1
          c' = c + n
          sys' = stepSysN n sys
       in if firstHop
            then (c', isa, sys') : go (j - 1) c' isa sys' False
            else case isaStep isa of
              IsaHalted -> [(c', isa, sys')]
              Next isa' ->
                (c', isa', sys')
                  : if running sys' then go (j - 1) c' isa' sys' False else []

-- | The first point at which the invariant fails, with an explanation.
invReport :: Int -> Vec PROG_SIZE Word -> String
invReport k prog =
  case [x | x@(_, isa, sys) <- invTrace k prog, P.not (inv isa sys)] of
    [] -> ""
    ((c, isa, sys) : _) ->
      P.concat
        [ "\ninvariant fails at cycle ",
          show c,
          "\n  isaPc=",
          show (isaPc isa),
          " isaInstr=",
          show (isaInstrAt isa),
          "\n  ex=",
          show (exInstr sys),
          " exPc=",
          show (stateExPc (sysState sys)),
          " me=",
          show (stateMeInstr (sysState sys)),
          " wb=",
          show (stateWbInstr (sysState sys)),
          "\n  halt=",
          show (stateHalt (sysState sys)),
          "\n",
          explain isa sys
        ]

progs :: [(String, Vec PROG_SIZE Word)]
progs =
  [ ("prog1 (arith+store)", mkProg prog1),
    ("prog2 (load)", mkProg prog2),
    ("prog3 (branch)", mkProg prog3),
    ("sumTo 10 (loop)", mkProg (sumTo 10)),
    ("storeHazardEx (self-modifying)", mkProg storeHazardEx),
    ("storeHazardMe (self-modifying)", mkProg storeHazardMe)
  ]

-- | Store hazard raised by the store in the /execute/ stage: it writes to the
-- address currently sitting in decode. Code starts at 'initPc' = 200, so index
-- @k@ lives at @200 + 4k@. Storing @x0@ (always zero) keeps the overwritten
-- word harmless -- it decodes to @Nop DecodeFail@.
storeHazardEx :: Vec 4 Instruction
storeHazardEx =
  -- at 200: sw x0, 204(x0) -- targets index 1, which is what decode holds
  SType Word 204 0 0
    :> IType (Arith ADD) 1 0 1
    :> IType (Arith ADD) 2 0 2
    :> Instruction.break
    :> Nil

-- | Store hazard raised by the store in the /memory/ stage, one cycle later,
-- while a non-memory instruction is in execute.
storeHazardMe :: Vec 5 Instruction
storeHazardMe =
  IType (Arith ADD) 1 0 5
    -- at 204: sw x0, 212(x0) -- targets index 3, reached by decode a cycle later
    :> SType Word 212 0 0
    :> IType (Arith ADD) 2 0 2
    :> IType (Arith ADD) 3 0 3
    :> Instruction.break
    :> Nil

-- Random program generation -------------------------------------------------

-- | Registers the generated programs use. Kept small so that hazards and
-- forwarding paths are hit often.
genReg :: Gen RegIdx
genReg = chooseBoundedIntegral (0, 7)

-- | Only encodable instructions that survive a decode round-trip are usable:
-- 'encode' errors otherwise, and the machine executes whatever the memory word
-- decodes to. Anything else is replaced by a harmless @addi x0, x0, 0@.
roundTrips :: Instruction -> Instruction
roundTrips i =
  case encode' i of
    Just w | decode' w == i -> i
    _ -> IType (Arith ADD) 0 0 0

-- | A random program of @n@ instructions followed by @ebreak@ padding.
--
-- Two constraints keep generated programs inside the harness's 400-byte
-- memory: every load and store uses @x0@ as its base with a small immediate,
-- and every branch or jump targets an instruction index inside the program.
genProg :: Gen (Vec PROG_SIZE Word)
genProg = do
  n <- choose (1, 12)
  body <- P.mapM (genInstr n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Instruction.break
  pure (map encode (unsafeFromList instrs))

genInstr :: Int -> Int -> Gen Instruction
genInstr n i =
  oneof
    [ RType <$> genArith <*> genReg <*> genReg <*> genReg,
      IType <$> (Arith <$> genArith) <*> genReg <*> genReg <*> genSmallImm,
      -- Loads and stores are pinned to base x0 so addresses stay in the RAM
      -- region below the program.
      (\size sign rd off -> IType (Load size sign) rd 0 off)
        <$> genSize
        <*> elements [Signed, Unsigned]
        <*> genReg
        <*> genDataOff,
      (\size off rs2 -> SType size off 0 rs2) <$> genSize <*> genDataOff <*> genReg,
      (\cmp t rs1 rs2 -> BType cmp (fromIntegral ((t - i) * 4)) rs1 rs2)
        <$> elements [EQ, NE, LT, GE, LTU, GEU]
        <*> choose (0, n)
        <*> genReg
        <*> genReg,
      (\rd t -> JType rd (fromIntegral ((t - i) * 4))) <$> genReg <*> choose (0, n),
      UType <$> elements [PC, Zero] <*> genReg <*> (fromIntegral <$> choose (0 :: Int, 15))
    ]
  where
    genArith = elements [ADD, SUB, XOR, OR, AND, SLT, SLTU]
    genSize = elements [Byte, Half, Word]
    genSmallImm = fromIntegral <$> choose (0 :: Int, 15)
    -- Word-aligned offsets well inside the 200-byte RAM region.
    genDataOff = (\k -> fromIntegral (k * 4)) <$> choose (0 :: Int, 40)

-- | Both checks at once, so a counterexample reports whichever broke.
checkProg :: Int -> Vec PROG_SIZE Word -> String
checkProg k prog = walkReport k prog P.++ invReport k prog

proofTests :: TestTree
proofTests =
  testGroup
    "Driver and invariant"
    [ -- The base case, on the real 'Vec'-backed reset state. 'Induction.baseCase'
      -- proves this for an arbitrary memory, but has to substitute the register
      -- file (Clash's 'repeat' is opaque to the symbolic executor), so the
      -- concrete check is what pins that the state it describes is the one
      -- 'Machine.initSys' actually produces -- including 'Core.init''s reset
      -- register file and the loaded program.
      testCase "invariant holds at reset" $
        let bad =
              [ name
                | (name, prog) <- progs,
                  P.not (inv (initIsa prog) (initSys prog))
              ]
         in if P.null bad
              then pure ()
              else assertFailure (P.unlines [n P.++ ":\n" P.++ explain (initIsa prog) (initSys prog) | (n, prog) <- progs, P.elem n bad]),
      testProperty "invariant holds at reset for any program" $
        withMaxSuccess 2000 $
          forAll genProg $ \prog ->
            counterexample (explain (initIsa prog) (initSys prog)) $
              inv (initIsa prog) (initSys prog),
      testGroup
        "driver lands on aligned states"
        [ testCase name $
            let r = walkReport 40 prog
             in if P.null r then pure () else assertFailure ("\n" P.++ r)
          | (name, prog) <- progs
        ],
      testGroup
        "invariant holds along the driven walk"
        [ testCase name $
            let r = invReport 40 prog
             in if P.null r then pure () else assertFailure r
          | (name, prog) <- progs
        ],
      -- The driver's stated invariant, checked directly: from an aligned state,
      -- stepping @driver + 1@ cycles is exactly stepping until the execute
      -- stage stops being a bubble.
      --
      -- @ecall@ / @ebreak@ are excluded because they are terminal: the core
      -- halts and the execute stage then holds @Nop Halted@ forever, so there
      -- is no next non-bubble for the operational reading to find.
      testCase "driver agrees with its operational reading" $
        let bad =
              [ (driver sys, ref, exInstr sys)
                | prog <- allProgs 500,
                  (_, _, sys) <- invTrace 40 prog,
                  running sys,
                  aligned sys,
                  P.not (isEnvInstr (exInstr sys)),
                  let ref = driverRef 12 sys,
                  ref /= Just (driver sys + 1)
              ]
         in if P.null bad then pure () else assertFailure (show (P.take 10 bad)),
      testProperty "driver and invariant on random programs" $
        withMaxSuccess 2000 $
          forAll genProg $ \prog ->
            let r = checkProg 40 prog
             in counterexample (show prog P.++ "\n" P.++ r) (P.null r),
      testCase "coverage of the checked states" $
        let cov = coverage 2000
            missing = [k | k <- interesting, P.notElem k (P.map P.fst cov)]
         in if P.null missing
              then pure ()
              else
                assertFailure $
                  "\nnever reached: "
                    P.++ show missing
                    P.++ "\nreached:\n"
                    P.++ unlines ["  " P.++ k P.++ ": " P.++ show v | (k, v) <- cov],
      -- On reachable states, the driven step preserves the invariant.
      testCase "inductive on reachable states" $
        let bad = [e | sys <- sampleStates 200, Just e <- [inductiveStep sys]]
         in if P.null bad then pure () else assertFailure (P.head bad),
      -- DEVIATION 2 of "Invariant": a jump spliced into the memory stage. No
      -- reachable state has one -- a jump costs three cycles, so it has retired
      -- past writeback by the next aligned state -- and nothing else in the
      -- invariant rules it out, so this is the only thing constraining the
      -- @JType@ / @IType Jump@ clauses of 'Invariant.flushMeStage'. It fails if
      -- those clauses stop writing @rd@.
      testCase "inductive with a jump in the memory stage" $
        let bad = [e | sys <- sampleStates 200, Just e <- [inductiveStep (withJumpInMe sys)]]
         in if P.null bad then pure () else assertFailure (P.head bad),
      -- DEVIATION 3: the added @no halt pending@ conjunct is what excludes a
      -- halt in flight. Without it such a state satisfies the invariant, and
      -- 'Core.memory' then halts the core part-way through the hop.
      testCase "invariant rejects a halt in flight" $
        let admitted =
              [ ()
                | sys <- sampleStates 200,
                  let s = withPendingHalt sys,
                  P.any (P.all P.snd . caseConjuncts) (invCases (isaFromSys s) s)
              ]
         in if P.null admitted then pure () else assertFailure "invariant still admits a pending halt",
      -- The pointwise form is what symbolic execution uses, since function-backed
      -- register files and memories have no decidable equality. It must agree
      -- with the container form wherever the latter holds, or the witness
      -- threading is wrong.
      testCase "pointwise invariant agrees with the container form" $
        let bad =
              [ (c, wr, wa)
                | prog <- allProgs 25,
                  (c, isa, sys) <- invTrace 40 prog,
                  inv isa sys,
                  wr <- [0 .. 31],
                  wa <- [0, 16 .. 396],
                  P.not (invAt wr wa isa sys)
              ]
         in if P.null bad then pure () else assertFailure (show (P.take 5 bad)),
      -- The fold-free invariant is what the plugin runs on; it must agree with
      -- the named-conjunct version everywhere, or the two paths are checking
      -- different things.
      testCase "fold-free invariant agrees with the list version" $
        let bad =
              [ (c, wr, wa)
                | prog <- allProgs 25,
                  (c, isa, sys) <- invTrace 40 prog,
                  wr <- [0 .. 31],
                  wa <- [0, 16 .. 396],
                  invAt wr wa isa sys /= invAtFree wr wa isa sys
              ]
         in if P.null bad then pure () else assertFailure (show (P.take 5 bad)),
      -- The mechanism behind the fifth counterexample, concretely: a store
      -- whose byte range sits in (or wraps into) a PC word at the top of the
      -- address space. See the comment above 'wrapCESys'.
      testCase "wrap-around aliasing store breaks the un-guarded inductive step" $ do
        let sys = wrapCESys
            isa = isaFromSys sys
            sys' = stepSys sys
            isa' = case isaStep isa of Next x -> x; IsaHalted -> isa
            wr = 1
            wa = 0xFFFFFFFD
        assertBool "state satisfies the invariant" (invAtFree wr wa isa sys)
        assertBool "driver picks the one-cycle hop" (driver sys P.== 0)
        assertBool
          "the driven step breaks the invariant (mod the store-alias assumption)"
          (P.not (invAtFree wr wa isa' sys')),
      -- ...and the (wrap-corrected) assumption must therefore exclude it.
      testCase "wrap-around aliasing store is excluded by noStoreAlias" $ do
        let sys = wrapCESys
        assertBool "noStoreAlias rejects the wrapping store" (P.not (noStoreAlias sys))
        assertBool "obligation holds (vacuously)" (indStepObligation 1 0xFFFFFFFD sys),
      testProperty "inductive step on arbitrary pipeline states" $
        -- 1e6 has been run by hand (60s, no counterexample); 20k keeps the
        -- suite fast.
        -- 1e6 run by hand: 27s, zero discards, no counterexample.
        -- 1e6 re-run by hand after the wrap-around fix, with the generator
        -- extended to wrapping memory layouts, boundary-straddling PCs,
        -- near-PC store addresses, and branches/memory ops in decode: 25s,
        -- no counterexample.
        withMaxSuccess 20000 $
          forAllShow genArbSys (\_ -> "<state; see counterexample below>") $ \(sys, wr, wa) ->
            let isa = isaFromSys sys
                sys' = stepSys sys
                isa' = case isaStep isa of Next x -> x; IsaHalted -> isa
             in counterexample
                  ( "me=" P.++ show (stateMeInstr (sysState sys))
                      P.++ "\nwb=" P.++ show (stateWbInstr (sysState sys))
                      P.++ "\nex=" P.++ show (exInstr sys)
                      P.++ "\nexPc=" P.++ show (stateExPc (sysState sys))
                      P.++ " wr=" P.++ show wr P.++ " wa=" P.++ show wa
                      P.++ "\nfailing=" P.++ show (P.map P.fst (P.filter (P.not . P.snd) (P.concatMap caseConjuncts (invCasesAt wr wa isa' sys'))))
                  )
                  (indStepObligation wr wa sys),
      -- k = 1: the two-cycle hop. Both shapes that reach it are generated;
      -- 'coverage1' asserts each is actually sampled, since a premise this
      -- specific is easy to miss entirely and still see a green property.
      testProperty "k=1 inductive step on arbitrary pipeline states" $
        -- 1e6 run by hand: 23s, no counterexample. 20k keeps the suite fast.
        withMaxSuccess 20000 $
          forAllShow genArbSys1 (\_ -> "<state; see counterexample below>") $ \(sys, wr, wa) ->
            let isa = isaOfHop sys
                s2 = stepSysN 2 sys
                isa' = case isaStep isa of Next x -> x; IsaHalted -> isa
             in counterexample
                  ( "startupShape=" P.++ show (isStartupShape sys)
                      P.++ "\nme=" P.++ show (stateMeInstr (sysState sys))
                      P.++ "\nwb=" P.++ show (stateWbInstr (sysState sys))
                      P.++ "\nex=" P.++ show (exInstr sys)
                      P.++ "\nexPc=" P.++ show (stateExPc (sysState sys))
                      P.++ " fePc=" P.++ show (stateFePc (sysState sys))
                      P.++ " wr=" P.++ show wr P.++ " wa=" P.++ show wa
                      P.++ "\nfailing=" P.++ show (P.map P.fst (P.filter (P.not . P.snd) (P.concatMap caseConjuncts (invCasesAt wr wa isa' s2))))
                  )
                  (indStepObligation1 wr wa sys),
      testCase "k=1 generator reaches both hop shapes" $
        let sample = [s | (s, _, _) <- unGen (vectorOf 4000 genArbSys1) (mkQCGen 7) 30]
            admitted p =
              [ ()
                | s <- sample,
                  p s,
                  driver s P.== 1,
                  invAtFree 1 0 (isaOfHop s) s
              ]
            startups = admitted isStartupShape
            steadies = admitted (P.not . isStartupShape)
         in do
              assertBool "no startup state satisfied the k=1 premise" (P.not (P.null startups))
              assertBool "no steady state satisfied the k=1 premise" (P.not (P.null steadies)),
      -- Why 'isaOfHop' exists: deriving the architectural PC from the execute
      -- stage, as 'isaOfG' does, leaves a startup state satisfying no case of
      -- the invariant at all -- so the k=1 obligation would hold vacuously
      -- there rather than saying anything.
      testCase "startup states need the fetch-stage PC to be admitted" $
        let sample = [s | (s, _, _) <- unGen (vectorOf 2000 genArbSys1) (mkQCGen 11) 30, isStartupShape s]
            admits f s = invAtFree 1 0 (f s) s
         in do
              assertBool "generator produced no startup states" (P.not (P.null sample))
              assertBool
                "isaOfHop should admit some startup state"
                (P.any (admits isaOfHop) sample)
              assertBool
                "isaOfG should admit no startup state (that is why isaOfHop exists)"
                (P.not (P.any (admits isaOfG) sample)),
      -- k = 2: jumps and the all-memory steady shape return to a running
      -- invariant case after three core cycles; ecall/ebreak return to one of
      -- the two halted cases.
      testProperty "k=2 inductive step on arbitrary pipeline states" $
        withMaxSuccess 20000 $
          forAllShow genArbSys2 (\_ -> "<k=2 state; see counterexample below>") $ \(label, sys, wr, wa) ->
            counterexample
              ( "case=" P.++ label
                  P.++ " driverCase=" P.++ driverCaseName sys
                  P.++ "\nme=" P.++ show (stateMeInstr (sysState sys))
                  P.++ "\nwb=" P.++ show (stateWbInstr (sysState sys))
                  P.++ "\nex=" P.++ show (exInstr sys)
                  P.++ "\nexPc=" P.++ show (stateExPc (sysState sys))
                  P.++ " wr=" P.++ show wr P.++ " wa=" P.++ show wa
              )
              (indStepObligation2 wr wa sys),
      testCase "k=2 generator reaches env, jump, and steady cases" $
        let sample =
              [ (label, driverCaseName s)
                | (label, s, wr, wa) <-
                    unGen (vectorOf 4000 genArbSys2) (mkQCGen 29) 30,
                  driver s P.== 2,
                  invAtFree wr wa (isaOfG s) s
              ]
            has label = P.any ((P.== label) . P.fst) sample
         in do
              assertBool "no env k=2 state satisfied the premise" (has "env")
              assertBool "no jump k=2 state satisfied the premise" (has "jump")
              assertBool "no steady k=2 state satisfied the premise" (has "steady"),
      testCase "jumps never occupy me/wb at a checked state" $
        let bad =
              [ (shape (stateMeInstr (sysState sys)), shape (stateWbInstr (sysState sys)))
                | prog <- allProgs 2000,
                  (_, _, sys) <- invTrace 40 prog,
                  isJumpShape (stateMeInstr (sysState sys)) || isJumpShape (stateWbInstr (sysState sys))
              ]
         in if P.null bad then pure () else assertFailure (show (P.take 5 bad))
    ]

-- Inductiveness probes -------------------------------------------------------
--
-- Being *inductive* means: for every (isa, sys) satisfying the invariant, the
-- driven step preserves it. That quantifier ranges over all states the
-- invariant admits, not just those reachable from startup. So a state that
-- cannot actually occur still has to be handled -- and that is where the
-- invariant as written comes apart.

-- | The architectural state that makes the flush conjunct hold at @sys@ by
-- construction. If @sys@ is an aligned running state, this is the ISA state the
-- invariant claims it corresponds to.
-- Shared with 'Induction.indStep0' via "Obligation", so the two cannot drift.
isaFromSys :: (RegFileOps r, MemOps m) => SysG r m -> IsaStateG r m
isaFromSys = isaOfG

-- | Take one driven step and report whether the invariant survived. Returns
-- 'Nothing' when @sys@ does not satisfy the invariant to begin with (such a
-- state is not a counterexample to inductiveness).
inductiveStep :: Sys -> Maybe String
inductiveStep sys
  | P.not (P.any holdsCase (invCases isa sys)) = Nothing
  | inv' isa' sys' = Nothing
  | otherwise = Just (explain isa' sys')
  where
    isa = isaFromSys sys
    sys' = stepSysN (driver sys + 1) sys
    isa' = case isaStep isa of
      Next i -> i
      IsaHalted -> isa
    inv' a s = P.any holdsCase (invCases a s)
    holdsCase c = P.all P.snd (caseConjuncts c)

-- | Aligned, running states drawn from the programs under test.
sampleStates :: Int -> [Sys]
sampleStates n =
  [ sys
    | prog <- allProgs n,
      (_, _, sys) <- invTrace 40 prog,
      running sys,
      aligned sys,
      P.not (isEnvInstr (exInstr sys))
  ]

-- | Put a jump in the memory stage. The invariant permits this -- a jump is not
-- a memory instruction, so @isMemInstr me == False@ is satisfied -- but it can
-- never actually occur, so nothing else in the invariant rules it out.
withJumpInMe :: Sys -> Sys
withJumpInMe s =
  s {sysState = (sysState s) {stateMeInstr = JType 5 0, stateMeRes = pure 0x1234}}

-- | Put a halt in flight. Without the @no halt pending@ conjunct the invariant
-- admits this, and 'Core.memory' will then halt the core part-way through the
-- driver's hop.
withPendingHalt :: Sys -> Sys
withPendingHalt s =
  s {sysState = (sysState s) {stateHaltPending = Just (EBreak 0)}}

-- The fifth counterexample, explained: address wraparound ---------------------
--
-- 'noStoreAlias' as originally written checked
--
-- >  clashes p = a < p + 4 && p < a + n
--
-- in 'Unsigned 32' arithmetic. Both @p + 4@ and @a + n@ wrap: with a PC word
-- at the top of the address space (@p = 0xFFFFFFFC@), @a < p + 4@ is @a < 0@,
-- which is false for EVERY store address -- so a store into that word passes
-- the assumption. Likewise a word store at @0xFFFFFFFE@ writes bytes
-- @0xFFFFFFFE..0x00000001@, wrapping into a low PC word without ever
-- satisfying @p < a + n@.
--
-- This state realises the mechanism concretely: the pipeline sits astride the
-- wrap (exPc = 0xFFFFFFF8, dePc = 0xFFFFFFFC, fePc = 0), and the memory-stage
-- store writes one byte into the middle of the decode-stage instruction word.
-- The invariant holds, the driver picks the one-cycle hop, and after the step
-- @ex == decode (mem[isaPc])@ is gone: the core executes the instruction it
-- latched before the store, while the architectural memory now holds the
-- stored-over word.
wrapExWord, wrapDeWord :: Word
wrapExWord = encode (IType (Arith ADD) 1 0 16) -- addi x1, x0, 16
wrapDeWord = encode (IType (Arith ADD) 2 0 1) -- addi x2, x0, 1

wrapMem :: MemFn
wrapMem = MemFn bytes
  where
    bytes a
      | a - 0xFFFFFFF8 P.< 4 = byteAt wrapExWord (a - 0xFFFFFFF8)
      | a - 0xFFFFFFFC P.< 4 = byteAt wrapDeWord (a - 0xFFFFFFFC)
      | P.otherwise = 0
    byteAt w k = case k of
      0 -> slice d7 d0 w
      1 -> slice d15 d8 w
      2 -> slice d23 d16 w
      _ -> slice d31 d24 w

wrapCESys :: SysG RegFn MemFn
wrapCESys =
  Sys
    ( (sysState (initSys (mkProg prog1)))
        { stateFePc = 0x00000000,
          stateDePc = 0xFFFFFFFC,
          stateExPc = 0xFFFFFFF8,
          stateExInstr = decode' wrapExWord,
          -- The store's address and value live in stateMeAddr / stateMeRes;
          -- its operand fields are already spent by the time it reaches the
          -- memory stage.
          stateMeInstr = SType Types.Byte 0 0 0,
          stateWbInstr = Nop MemoryBusBusy,
          stateMeRes = pure 0, -- the byte written: 0x00, over 0x01
          stateWbRes = pure 0,
          stateMeAddr = 0xFFFFFFFD, -- second byte of the decode-stage word
          stateRegFile = RegFn (const (pure 0)),
          stateCtrl = initCtrl,
          stateHalt = Nothing,
          stateHaltPending = Nothing
        }
    )
    (Input True (pure wrapDeWord))
    wrapMem

-- Arbitrary-state search ------------------------------------------------------
--
-- The counterexamples symbolic execution finds are all UNREACHABLE states, so
-- walking well-formed programs will never produce them. Generating arbitrary
-- pipeline states directly does, and gives a shrinkable executable example
-- instead of a model to hand-decode.
--
-- States are built to satisfy the invariant's structural conjuncts by
-- construction (PCs three words apart, ex latched from mem[exPc], inputMem
-- from mem[dePc]) so the premise is not discarded; everything else is random.
--
-- 'stateCtrl' is left at 'initCtrl' soundly: 'Core.pipe' is wrapped in
-- 'Core.withCtrlReset', which overwrites it before any stage reads it, so the
-- incoming value cannot affect 'Machine.stepSys'.

-- | Instruction generators that build in the premise's constraints, rather than
-- generating freely and filtering. Filtering discarded ~6 examples per hit,
-- which wastes work and -- worse -- can bias the sample rather than thin it.

gr3 :: Gen RegIdx
gr3 = chooseBoundedIntegral (0, 3)

gi15 :: Gen Imm
gi15 = fromIntegral <$> choose (0 :: Int, 15)

grAvoiding :: [RegIdx] -> Gen RegIdx
grAvoiding bad = chooseBoundedIntegral (0, 3) `suchThat` (\r -> P.notElem r bad)

-- | Non-memory instructions: what @driver == 0@ requires of the writeback
-- stage (@isMemInstr wb == False@).
genNonMem :: Gen Instruction
genNonMem =
  oneof
    [ RType <$> elements [ADD, SUB, XOR, AND, SLT] <*> gr3 <*> gr3 <*> gr3,
      (\op rd rs i -> IType (Arith op) rd rs i) <$> elements [ADD, XOR] <*> gr3 <*> gr3 <*> gi15,
      UType <$> elements [Zero, PC] <*> gr3 <*> (fromIntegral <$> choose (0 :: Int, 15)),
      JType <$> gr3 <*> (fromIntegral <$> choose (0 :: Int, 15)),
      (\rd rs i -> IType Jump rd rs i) <$> gr3 <*> gr3 <*> gi15,
      P.pure (Nop MemoryBusBusy)
    ]

-- | The decode-stage instruction. Unlike the writeback stage, nothing under
-- @driver == 0@ restricts it, so loads, stores and branches belong in the
-- sample too; a load-use hazard against an execute-stage load is what the
-- caller filters, and anything else the premise discards is a few wasted
-- samples, not a soundness issue.
genDeInstr :: Gen Instruction
genDeInstr =
  frequency
    [ (3, genNonMem),
      ( 1,
        (\sz sg rd rs i -> IType (Load sz sg) rd rs i)
          <$> elements [Types.Byte, Types.Half, Types.Word]
          <*> elements [Signed, Unsigned]
          <*> gr3
          <*> gr3
          <*> gi15
      ),
      ( 1,
        (\sz i r1 r2 -> SType sz i r1 r2)
          <$> elements [Types.Byte, Types.Half, Types.Word]
          <*> gi15
          <*> gr3
          <*> gr3
      ),
      ( 1,
        (\cmp t r1 r2 -> BType cmp (fromIntegral (2 * t :: Int)) r1 r2)
          <$> elements [EQ, NE, LT, GE, LTU, GEU]
          <*> choose (0, 7)
          <*> gr3
          <*> gr3
      )
    ]

-- | The execute stage under @driver == 0@: no environment instruction and no
-- jump, both of which the driver routes to a longer hop. Excluding them is
-- exact for k = 0, not a coverage compromise.
genExInstr :: Gen Instruction
genExInstr =
  oneof
    [ RType <$> elements [ADD, SUB, XOR, AND, SLT] <*> gr3 <*> gr3 <*> gr3,
      (\op rd rs i -> IType (Arith op) rd rs i) <$> elements [ADD, XOR] <*> gr3 <*> gr3 <*> gi15,
      (\sz sg rd rs i -> IType (Load sz sg) rd rs i)
        <$> elements [Types.Byte, Types.Half, Types.Word]
        <*> elements [Signed, Unsigned] <*> gr3 <*> gr3 <*> gi15,
      (\sz i r1 r2 -> SType sz i r1 r2)
        <$> elements [Types.Byte, Types.Half, Types.Word] <*> gi15 <*> gr3 <*> gr3,
      UType <$> elements [Zero, PC] <*> gr3 <*> (fromIntegral <$> choose (0 :: Int, 15)),
      -- Branches are admissible under @driver == 0@ when not taken; taken ones
      -- are discarded by the premise, which just costs a few samples. The
      -- immediate is kept even so 'roundTrips' preserves the shape.
      (\cmp t r1 r2 -> BType cmp (fromIntegral (2 * t :: Int)) r1 r2)
        <$> elements [EQ, NE, LT, GE, LTU, GEU] <*> choose (0, 7) <*> gr3 <*> gr3,
      P.pure (Nop MemoryBusBusy)
    ]

-- | The memory stage. A load's destination avoids the execute stage's sources,
-- so the load-use hazard the invariant forbids is excluded by construction.
genMeInstr :: Instruction -> Gen Instruction
genMeInstr exI =
  oneof
    [ genNonMem,
      (\sz sg rd rs i -> IType (Load sz sg) rd rs i)
        <$> elements [Types.Byte, Types.Half, Types.Word]
        <*> elements [Signed, Unsigned]
        <*> grAvoiding srcs <*> gr3 <*> gi15,
      (\sz i r1 r2 -> SType sz i r1 r2)
        <$> elements [Types.Byte, Types.Half, Types.Word] <*> gi15 <*> gr3 <*> gr3
    ]
  where
    srcs = P.concat [getRs1 exI, getRs2 exI]

genW :: Gen Word
genW = fromInteger <$> choose (0, 2 P.^ (32 :: Int) - 1)

genFn :: (P.Eq k) => Gen k -> Gen v -> Gen v -> Gen (k -> v)
genFn gk gv gd = do
  n <- choose (0 :: Int, 10)
  ps <- vectorOf n ((,) <$> gk <*> gv)
  d <- gd
  P.pure (\k -> P.maybe d P.id (P.lookup k ps))

-- | A pipeline state satisfying the premise of the k = 0 obligation by
-- construction: PCs three words apart, ex latched from mem[exPc], inputMem
-- from mem[dePc], writeback non-memory, execute neither environment nor jump,
-- no load-use hazard, no halt in flight.
--
-- 'stateCtrl' is left at 'initCtrl' soundly: 'Core.pipe' is wrapped in
-- 'Core.withCtrlReset', which overwrites it before any stage reads it.
genArbSys :: Gen (SysG RegFn MemFn, RegIdx, Address)
genArbSys = do
  -- Occasionally sit the pipeline astride the top of the address space: the
  -- fifth symbolic counterexample lived there, and the non-wrapping memory
  -- layout this generator previously used made the premise unsatisfiable in
  -- exactly that region, hiding it from the search.
  base <-
    frequency
      [ (7, unpack <$> genW),
        (1, elements [0xFFFFFFF4, 0xFFFFFFF8, 0xFFFFFFFC])
      ]
  exI <- genExInstr
  meI <- genMeInstr exI
  nextI <- case exI of
    IType (Load _ _) rd _ _ ->
      suchThat genDeInstr (\i -> P.notElem rd (P.concat [getRs1 i, getRs2 i]))
    _ -> genDeInstr
  wbI <- genNonMem
  let w0 = P.maybe 0 P.id (encode' (roundTrips exI))
      w1 = P.maybe 0 P.id (encode' (roundTrips nextI))
  w2 <- genW
  extra <- genFn (unpack <$> genW) (fromIntegral <$> choose (0 :: Int, 255)) (P.pure 0)
  let byteOf w k = case k of
        0 -> slice d7 d0 w
        1 -> slice d15 d8 w
        2 -> slice d23 d16 w
        _ -> slice d31 d24 w
      -- Wrapping comparisons: @a - base < 4@ is the wrap-correct form of
      -- @base <= a && a < base + 4@, so the three-instruction window stays
      -- intact when it straddles 0xFFFFFFFF -> 0.
      memf a
        | a - base P.< 4 = byteOf w0 (a - base)
        | a - base P.< 8 = byteOf w1 (a - base - 4)
        | a - base P.< 12 = byteOf w2 (a - base - 8)
        | P.otherwise = extra a
  rfF <- genFn (chooseBoundedIntegral (0, 3)) genW genW
  mr <- genW
  wbr <- genW
  -- Store addresses near the PC window (including partial overlaps and, when
  -- the window straddles the wrap, wrapped byte ranges) are where the
  -- store-alias corner cases live; a uniform address almost never lands there.
  ma <-
    frequency
      [ (1, unpack <$> genW),
        (1, (\d -> base + fromIntegral (d :: Int) - 8) <$> choose (0, 24))
      ]
  wr <- frequency [(3, chooseBoundedIntegral (0, 3)), (1, chooseBoundedIntegral (0, 31))]
  wa <- unpack <$> genW
  let sys =
        Sys
          ((sysState (initSys (mkProg prog1)))
             { stateFePc = base + 8,
               stateDePc = base + 4,
               stateExPc = base,
               stateExInstr = decode' w0,
               stateMeInstr = meI,
               stateWbInstr = wbI,
               stateMeRes = pure mr,
               stateWbRes = pure wbr,
               stateMeAddr = ma,
               stateRegFile = RegFn (P.fmap Identity rfF),
               stateCtrl = initCtrl,
               stateHalt = Nothing,
               stateHaltPending = Nothing
             })
          (Input True (pure w1))
          (MemFn memf)
  P.pure (sys, wr, wa)

-- | States the driver sends on a two-cycle hop, i.e. @driver == 1@. Two shapes
-- reach it, and they are generated separately because their invariant cases
-- constrain the fetch path differently:
--
--   * startup -- every stage holds @Nop FirstCycle@ and nothing is on the bus;
--   * steady with a memory instruction in writeback -- it occupied the bus
--     last cycle, so no instruction was fetched and @fePc == exPc + 4@.
genArbSys1 :: Gen (SysG RegFn MemFn, RegIdx, Address)
genArbSys1 = oneof [genStartup1, genSteady1]

-- | Arbitrary running states sent on a three-cycle hop. The three satisfiable
-- driver branches are generated separately:
--
--   * an environment instruction;
--   * a taken jump (JAL is unconditionally taken);
--   * memory instructions in both writeback and memory, with a non-memory
--     execute instruction.
--
-- The driver's @storeHazard/nomem@ branch is incompatible with the theorem's
-- pre-state 'noStoreAlias' assumption: it requires a memory-stage store at
-- exactly @dePc@, while the assumption rejects even a one-byte overlap with
-- that word. It is therefore an empty proof case rather than a generator case.
genArbSys2 :: Gen (String, SysG RegFn MemFn, RegIdx, Address)
genArbSys2 =
  oneof
    [ genRunning2 "env" $
        elements
          [ IType (Env Call) 0 0 0,
            IType (Env Break) 0 0 0
          ],
      genRunning2 "jump" $
        JType <$> gr3 <*> (fromIntegral . (2 *) <$> choose (0 :: Int, 7)),
      genSteady2
    ]

-- | Environment and jump states permit arbitrary earlier pipeline shapes.
-- Fetch-input shape follows writeback exactly as the invariant requires.
genRunning2 ::
  String ->
  Gen Instruction ->
  Gen (String, SysG RegFn MemFn, RegIdx, Address)
genRunning2 label genEx = do
  base <- genBase
  ex0 <- genEx
  let exI = roundTrips ex0
  meI <- genMeInstr exI
  wbI <- oneof [genNonMem, genMemInstr]
  nextI <- genDeInstr
  let w0 = P.maybe 0 P.id (encode' exI)
      w1 = P.maybe 0 P.id (encode' (roundTrips nextI))
  w2 <- genW
  (memf, _) <- genMemWindow base w0 w1 w2
  rfF <- genFn (chooseBoundedIntegral (0, 3)) genW genW
  mr <- genW
  wbr <- genW
  loaded <- genW
  -- Keep memory-stage stores away from all three PC words so the non-alias
  -- premise is exercised rather than making those samples vacuous.
  let ma = base + 64
      wbMem = isMemInstr wbI
      inp = if wbMem then Input False (pure loaded) else Input True (pure w1)
      fePc = if wbMem then base + 4 else base + 8
  wr <- genWitnessReg
  wa <- unpack <$> genW
  let sys =
        Sys
          ( (sysState (initSys (mkProg prog1)))
              { stateFePc = fePc,
                stateDePc = base + 4,
                stateExPc = base,
                stateExInstr = decode' w0,
                stateMeInstr = meI,
                stateWbInstr = wbI,
                stateMeRes = pure mr,
                stateWbRes = pure wbr,
                stateMeAddr = ma,
                stateRegFile = RegFn (P.fmap Identity rfF),
                stateCtrl = initCtrl,
                stateHalt = Nothing,
                stateHaltPending = Nothing
              }
          )
          inp
          (MemFn memf)
  P.pure (label, sys, wr, wa)

-- | The steady @wb=memory, me=memory, ex=non-memory@ branch.
genSteady2 :: Gen (String, SysG RegFn MemFn, RegIdx, Address)
genSteady2 = do
  base <- genBase
  exI <-
    oneof
      [ RType <$> elements [ADD, SUB, XOR, AND, SLT] <*> gr3 <*> gr3 <*> gr3,
        (\op rd rs i -> IType (Arith op) rd rs i)
          <$> elements [ADD, XOR] <*> gr3 <*> gr3 <*> gi15,
        UType <$> elements [Zero, PC] <*> gr3 <*> (fromIntegral <$> choose (0 :: Int, 15)),
        P.pure (Nop MemoryBusBusy)
      ]
  meI <-
    oneof
      [ (\sz sg rd rs i -> IType (Load sz sg) rd rs i)
          <$> elements [Types.Byte, Types.Half, Types.Word]
          <*> elements [Signed, Unsigned]
          <*> grAvoiding (P.concat [getRs1 exI, getRs2 exI])
          <*> gr3
          <*> gi15,
        (\sz i r1 r2 -> SType sz i r1 r2)
          <$> elements [Types.Byte, Types.Half, Types.Word]
          <*> gi15
          <*> gr3
          <*> gr3
      ]
  wbI <- genMemInstr
  nextI <- genDeInstr
  let w0 = P.maybe 0 P.id (encode' (roundTrips exI))
      w1 = P.maybe 0 P.id (encode' (roundTrips nextI))
  w2 <- genW
  (memf, _) <- genMemWindow base w0 w1 w2
  rfF <- genFn (chooseBoundedIntegral (0, 3)) genW genW
  mr <- genW
  wbr <- genW
  loaded <- genW
  wr <- genWitnessReg
  wa <- unpack <$> genW
  let sys =
        Sys
          ( (sysState (initSys (mkProg prog1)))
              { stateFePc = base + 4,
                stateDePc = base + 4,
                stateExPc = base,
                stateExInstr = decode' w0,
                stateMeInstr = meI,
                stateWbInstr = wbI,
                stateMeRes = pure mr,
                stateWbRes = pure wbr,
                stateMeAddr = base + 64,
                stateRegFile = RegFn (P.fmap Identity rfF),
                stateCtrl = initCtrl,
                stateHalt = Nothing,
                stateHaltPending = Nothing
              }
          )
          (Input False (pure loaded))
          (MemFn memf)
  P.pure ("steady", sys, wr, wa)

-- | The startup shape. @fePc@ is where the ISA's PC sits, and the pipeline is
-- empty, so no memory word is pinned to any stage.
genStartup1 :: Gen (SysG RegFn MemFn, RegIdx, Address)
genStartup1 = do
  fePc <- genBase
  firstI <- genNonMem
  nextI <- genDeInstr
  let w0 = P.maybe 0 P.id (encode' (roundTrips firstI))
      w1 = P.maybe 0 P.id (encode' (roundTrips nextI))
  w2 <- genW
  (memf, _) <- genMemWindow fePc w0 w1 w2
  rfF <- genFn (chooseBoundedIntegral (0, 3)) genW genW
  wr <- genWitnessReg
  wa <- unpack <$> genW
  let sys =
        Sys
          ( (sysState (initSys (mkProg prog1)))
              { stateFePc = fePc,
                -- The other PCs are unconstrained by the startup case; give
                -- them junk so nothing accidentally relies on them.
                stateDePc = 0,
                stateExPc = 0,
                stateExInstr = Nop FirstCycle,
                stateMeInstr = Nop FirstCycle,
                stateWbInstr = Nop FirstCycle,
                stateMeRes = pure 0,
                stateWbRes = pure 0,
                stateMeAddr = 0,
                stateRegFile = RegFn (P.fmap Identity rfF),
                stateCtrl = initCtrl,
                stateHalt = Nothing,
                stateHaltPending = Nothing
              }
          )
          (Input False (pure 0))
          (MemFn memf)
  P.pure (sys, wr, wa)

-- | The steady two-cycle shape: a load or store in writeback, a non-memory
-- instruction in the memory stage. Nothing is on the bus this cycle, so
-- @inputMem@ is the value the writeback-stage load reads, not an instruction.
genSteady1 :: Gen (SysG RegFn MemFn, RegIdx, Address)
genSteady1 = do
  base <- genBase
  exI <- genExInstr
  meI <- suchThat genNonMem (\i -> P.not (loadHazard exI i))
  wbI <- genMemInstr
  nextI <- genDeInstr
  let w0 = P.maybe 0 P.id (encode' (roundTrips exI))
      w1 = P.maybe 0 P.id (encode' (roundTrips nextI))
  w2 <- genW
  (memf, _) <- genMemWindow base w0 w1 w2
  rfF <- genFn (chooseBoundedIntegral (0, 3)) genW genW
  mr <- genW
  wbr <- genW
  -- The word the writeback-stage load takes its value from.
  loaded <- genW
  ma <- genStoreAddr base
  wr <- genWitnessReg
  wa <- unpack <$> genW
  let sys =
        Sys
          ( (sysState (initSys (mkProg prog1)))
              { stateFePc = base + 4,
                stateDePc = base + 4,
                stateExPc = base,
                stateExInstr = decode' w0,
                stateMeInstr = meI,
                stateWbInstr = wbI,
                stateMeRes = pure mr,
                stateWbRes = pure wbr,
                stateMeAddr = ma,
                stateRegFile = RegFn (P.fmap Identity rfF),
                stateCtrl = initCtrl,
                stateHalt = Nothing,
                stateHaltPending = Nothing
              }
          )
          (Input False (pure loaded))
          (MemFn memf)
  P.pure (sys, wr, wa)

-- | Loads and stores, for the writeback stage of a @k = 1@ steady state.
genMemInstr :: Gen Instruction
genMemInstr =
  oneof
    [ (\sz sg rd rs i -> IType (Load sz sg) rd rs i)
        <$> elements [Types.Byte, Types.Half, Types.Word]
        <*> elements [Signed, Unsigned]
        <*> gr3
        <*> gr3
        <*> gi15,
      (\sz i r1 r2 -> SType sz i r1 r2)
        <$> elements [Types.Byte, Types.Half, Types.Word]
        <*> gi15
        <*> gr3
        <*> gr3
    ]

-- | A PC base, biased towards the top of the address space so hops that
-- straddle the 0xFFFFFFFF -> 0 wrap are sampled. See the wrap-around note on
-- 'genArbSys'.
genBase :: Gen Address
genBase =
  frequency
    [ (7, unpack <$> genW),
      (1, elements [0xFFFFFFF4, 0xFFFFFFF8, 0xFFFFFFFC])
    ]

-- | Three instruction words at @base@, @base+4@, @base+8@ over arbitrary
-- background bytes, with wrap-correct bounds.
genMemWindow :: Address -> Word -> Word -> Word -> Gen (Address -> Byte, ())
genMemWindow base w0 w1 w2 = do
  extra <- genFn (unpack <$> genW) (fromIntegral <$> choose (0 :: Int, 255)) (P.pure 0)
  let byteOf w k = case k of
        0 -> slice d7 d0 w
        1 -> slice d15 d8 w
        2 -> slice d23 d16 w
        _ -> slice d31 d24 w
      memf a
        | a - base P.< 4 = byteOf w0 (a - base)
        | a - base P.< 8 = byteOf w1 (a - base - 4)
        | a - base P.< 12 = byteOf w2 (a - base - 8)
        | P.otherwise = extra a
  P.pure (memf, ())

-- | Store addresses biased into the PC window, where the aliasing corner cases
-- live.
genStoreAddr :: Address -> Gen Address
genStoreAddr base =
  frequency
    [ (1, unpack <$> genW),
      (1, (\d -> base + fromIntegral (d :: Int) - 8) <$> choose (0, 24))
    ]

genWitnessReg :: Gen RegIdx
genWitnessReg =
  frequency [(3, chooseBoundedIntegral (0, 3)), (1, chooseBoundedIntegral (0, 31))]

isJumpShape :: Instruction -> Bool
isJumpShape (JType _ _) = True
isJumpShape (IType Jump _ _ _) = True
isJumpShape _ = False

-- | Every program the tests check the invariant on: the hand-written ones plus
-- a deterministic sample of the generator.
allProgs :: Int -> [Vec PROG_SIZE Word]
allProgs n =
  P.map P.snd progs
    P.++ [unGen genProg (mkQCGen k) 30 | k <- [1 .. n]]

-- | Which driver cases, and which memory-/writeback-stage instruction shapes,
-- the tests actually exercise at the states where the invariant is checked.
coverage :: Int -> [(String, Int)]
coverage n = tally (P.concatMap observations (allProgs n))
  where
    observations prog =
      P.concat
        [ [ "driver:" P.++ driverCaseName sys,
            "me:" P.++ shape (stateMeInstr (sysState sys)),
            "wb:" P.++ shape (stateWbInstr (sysState sys))
          ]
          | (_, _, sys) <- invTrace 40 prog
        ]
    tally xs = P.foldr bump [] xs
    bump x [] = [(x, 1)]
    bump x ((y, c) : rest)
      | x == y = (y, c + 1) : rest
      | otherwise = (y, c) : bump x rest

-- | A coarse name for an instruction, for coverage purposes.
shape :: Instruction -> String
shape = \case
  RType {} -> "RType"
  IType (Arith _) _ _ _ -> "IType/Arith"
  IType (Load _ _) _ _ _ -> "IType/Load"
  IType Jump _ _ _ -> "IType/Jump"
  IType (Env _) _ _ _ -> "IType/Env"
  SType {} -> "SType"
  BType {} -> "BType"
  UType {} -> "UType"
  JType {} -> "JType"
  Nop _ -> "Nop"

-- | Cases we want the random programs to actually reach. If any of these is
-- never hit, the passing property above says less than it appears to.
interesting :: [String]
interesting =
  [ "driver:firstCycle",
    "driver:env",
    "driver:jump",
    "driver:storeHazard/mem",
    "driver:storeHazard/nomem",
    "driver:loadHazard",
    "driver:steady/wb-nomem",
    "driver:steady/me-nomem",
    "driver:steady/ex-nomem",
    "driver:steady/all-mem",
    "me:IType/Load",
    "me:SType",
    "wb:IType/Load",
    "wb:SType"
  ]

prog1 :: Vec 3 Instruction
prog1 =
  IType (Arith ADD) 2 0 5
    :> SType Word 0 0 2
    :> Instruction.break
    :> Nil

prog2 :: Vec 6 Instruction
prog2 =
  IType (Arith ADD) 2 0 5
    :> SType Word 0 0 2
    :> IType (Load Word Signed) 3 0 0
    :> RType ADD 4 0 3
    :> SType Word 4 0 4
    :> Instruction.break
    :> Nil

prog3 :: Vec 6 Instruction
prog3 =
  IType (Arith ADD) 2 0 3
    :> RType ADD 3 0 2
    :> BType EQ 8 2 3
    :> SType Word 0 0 2
    :> SType Word 4 0 2
    :> Instruction.break
    :> Nil

sumTo :: Int -> Vec 8 Instruction
sumTo n =
  unsafeFromList
    [ IType (Arith ADD) 1 0 (fromIntegral n),
      IType (Arith ADD) 2 0 0,
      BType EQ 16 1 0,
      RType ADD 2 2 1,
      IType (Arith ADD) 1 1 (-1),
      JType 0 (-12),
      SType Word 0 0 2,
      Instruction.break
    ]
