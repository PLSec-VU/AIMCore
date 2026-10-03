{-# LANGUAGE PackageImports #-}

-- | The core behind a cache, checked against the core behind ideal memory.
--
-- The constant-time leakage proof ("Proof.Leakage.Induction") is about the core
-- attached to memory that answers every request in the next cycle. Behind a
-- cache ('Proof.Machine.stepCachedOut') a load that misses is answered only
-- after the miss penalty, and in the meantime the core stalls. The tests here
-- check the two facts that let the ideal-memory result carry over:
--
--   * a stall cycle makes no request and changes nothing in the core but the
--     decode-stage PC, and every other cycle is a cycle of the ideal run -- so
--     the cached run makes the ideal run's requests, with stall cycles
--     inserted;
--   * end to end, a program whose control flow and addresses do not depend on
--     the data it loads makes the same requests at the same times behind a
--     cache, whatever the data.
module CompositionSpec
  ( compositionTests,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Core
import Data.Functor.Identity (Identity (..))
import ISA (IsaState, StepG (..), isaStep)
import Instruction
import LeakageSpec (genCTProg)
import Memory.Cache
import Memory.Types
import Proof.Leakage.Model
import Proof.Leakage.Simulator (archOfLeak)
import Proof.Machine
import RegFile (RegFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | The cache the tests use: four one-word lines, so lines collide often.
type TestCache = DirectMapped 4

-- | Two systems equal up to what a stall may change and nobody observes.
--
-- The control lines never matter: 'Core.withCtrlReset' overwrites them before
-- any stage reads them. The decode-stage PC and the PC of a bubble in execute
-- may differ too. A stall freezes decode and execute but not fetch, which keeps
-- copying the fetch PC into the decode PC; when the cycle that issued the load
-- also redirected fetch (a taken jump, a hazard's refetch), that changes the
-- decode PC. The cycle after the stall is then the second cycle of the
-- redirect, whose decode emits a bubble regardless, so the only trace left is
-- the PC of that bubble, overwritten one cycle later.
sameUpToStall :: Sys -> Sys -> Bool
sameUpToStall (Sys a ia ma) (Sys b ib mb) =
  norm a == norm b
    P.&& inputIsInstr ia == inputIsInstr ib
    P.&& runIdentity (inputMem ia) == runIdentity (inputMem ib)
    P.&& inputMemReady ia == inputMemReady ib
    P.&& ma == mb
  where
    norm st =
      st
        { stateCtrl = initCtrl,
          stateDePc = 0,
          stateExPc = case stateExInstr st of
            Nop _ -> 0
            _ -> stateExPc st
        }

-- | The fields, other than the control lines, on which two systems differ, as
-- @field(first | second)@.
coreDiff :: Sys -> Sys -> [String]
coreDiff (Sys a ia ma) (Sys b ib mb) =
  P.concat
    [ f "fePc" stateFePc,
      f "dePc" stateDePc,
      f "exPc" stateExPc,
      f "ex" stateExInstr,
      f "me" stateMeInstr,
      f "meAddr" stateMeAddr,
      f "meRes" (runIdentity . stateMeRes),
      f "wb" stateWbInstr,
      f "wbRes" (runIdentity . stateWbRes),
      f "halt" stateHalt,
      f "haltNextPc" stateHaltNextPc,
      f "loadInFlight" stateLoadInFlight,
      g "isInstr" inputIsInstr,
      g "bus" (runIdentity . inputMem),
      g "ready" inputMemReady,
      ["registers" | stateRegFile a /= stateRegFile b],
      ["memory" | ma /= mb]
    ]
  where
    f :: (Eq v, Show v) => String -> (State Identity -> v) -> [String]
    f name sel
      | sel a == sel b = []
      | otherwise = [name P.++ "(" P.++ show (sel a) P.++ " | " P.++ show (sel b) P.++ ")"]
    g :: (Eq v, Show v) => String -> (Input Identity -> v) -> [String]
    g name sel
      | sel ia == sel ib = []
      | otherwise = [name P.++ "(" P.++ show (sel ia) P.++ " | " P.++ show (sel ib) P.++ ")"]

-- | Walk the cached system and the ideal one together for @n@ cycles of the
-- cached one, skipping the cached run's stall cycles -- those whose bus input is
-- not ready -- in the ideal one. Returns @\"\"@ if every stall cycle makes no
-- request and leaves the core alone up to 'sameUpToStall', and every other
-- cycle makes the ideal run's request from the ideal run's state, up to
-- 'sameUpToStall'.
stutterReport :: Int -> Int -> Vec PROG_SIZE Word -> String
stutterReport penalty n prog = go n (0 :: Int) (initCacheSys sys0 (mkDirectMapped :: TestCache)) sys0
  where
    sys0 = initSys prog

    go :: Int -> Int -> CacheSys RegFile MemBytes TestCache -> Sys -> String
    go 0 _ _ _ = ""
    go j c cs ideal
      | P.not (running core) = ""
      | P.not ready =
          if obsOf oC /= NoAccess
            then failAt "a stall cycle made a request: " (show (obsOf oC))
            else
              -- The bus input is the memory's answer, which may arrive now.
              if P.not (sameUpToStall (withInput (cacheSysCore cs')) core)
                then failAt "a stall cycle changed the core: " (P.unwords (coreDiff core (cacheSysCore cs')))
                else go (j - 1) (c + 1) cs' ideal
      | P.not (sameUpToStall core ideal) = failAt "the cached core left the ideal run: " (P.unwords (coreDiff ideal core))
      | obsOf oC /= obsOf oI = failAt "different requests: " (show (obsOf oC, obsOf oI))
      | otherwise = go (j - 1) (c + 1) cs' ideal'
      where
        core = cacheSysCore cs
        ready = inputMemReady (sysInput core)
        withInput (Sys st _ m) = Sys st (sysInput core) m
        (cs', oC) = stepCachedOut penalty cs
        (ideal', oI) = stepSysOut ideal
        failAt what detail = "\ncycle " P.++ show c P.++ ": " P.++ what P.++ detail P.++ "\n"

-- | The ISA-level constant-time leakage trace of the first @n@ instructions.
ctTrace :: Int -> IsaState -> [L]
ctTrace 0 _ = []
ctTrace n a = leakOf a : case isaStep a of
  Next a' -> ctTrace (n - 1) a'
  IsaHalted -> []

-- | The requests the core makes behind a cache, cycle by cycle, until it halts.
cachedObs :: Int -> Int -> Sys -> [Obs]
cachedObs penalty n sys = go n (initCacheSys sys (mkDirectMapped :: TestCache))
  where
    go :: Int -> CacheSys RegFile MemBytes TestCache -> [Obs]
    go 0 _ = []
    go j cs
      | P.not (running (cacheSysCore cs)) = []
      | otherwise = let (cs', o) = stepCachedOut penalty cs in obsOf o : go (j - 1) cs'

-- | Load a data image (word-aligned addresses below the program) into a system.
withData :: [(Address, Word)] -> Sys -> Sys
withData ws (Sys s i m) = Sys s i (P.foldr (\(a, w) -> memWriteWord Types.Word a w) m ws)

genData :: Gen [(Address, Word)]
genData =
  P.mapM (\k -> (\w -> (4 * k, w)) <$> elements [0, 1, 4, 0x10, 0x40, 0xDEADBEEF]) [0 .. 49]

-- | Programs whose control flow and addresses never depend on loaded data.
--
-- As 'LeakageSpec.genCTProg', with @x5@ and @x6@ the base registers, set only
-- from constants; loads write @x1@ to @x4@ and @x7@, and branches compare only
-- @x0@, @x5@ and @x6@, so no loaded value reaches a branch or an address. Stores
-- store @x0@, so no loaded value reaches memory either.
genSecretFreeProg :: Gen (Vec PROG_SIZE Word)
genSecretFreeProg = do
  n <- choose (2, 12)
  body <- P.mapM (genOne n) [0 .. n - 1]
  let instrs = P.map roundTrips body P.++ P.replicate (50 - n) Instruction.break
  pure (map encode (unsafeFromList instrs))
  where
    roundTrips i = case encode' i of
      Just w | decode' w == i -> i
      _ -> IType (Arith ADD) 0 0 0

    genOne n i =
      frequency
        [ (2, RType <$> elements [ADD, SUB, XOR, OR, AND] <*> genDst <*> genAny <*> genAny),
          (3, (\b a -> IType (Arith ADD) b 0 (fromIntegral a)) <$> genBase <*> genAddr n),
          ( 3,
            (\size sign rd b off -> IType (Load size sign) rd b off)
              <$> elements [Byte, Half, Types.Word]
              <*> elements [Signed, Unsigned]
              <*> genDst
              <*> genBase
              <*> genOff
          ),
          (2, (\size off b -> SType size off b 0) <$> elements [Byte, Half, Types.Word] <*> genOff <*> genBase),
          ( 2,
            (\cmp t rs1 rs2 -> BType cmp (fromIntegral ((t - i) * 4)) rs1 rs2)
              <$> elements [EQ, NE, LT, GE, LTU, GEU]
              <*> choose (0, n)
              <*> genCtl
              <*> genCtl
          ),
          (1, (\rd t -> JType rd (fromIntegral ((t - i) * 4))) <$> genDst <*> choose (0, n))
        ]

    genBase = elements [5, 6]
    genCtl = elements [0, 5, 6]
    genDst = elements [1, 2, 3, 4, 7]
    genAny = elements [0, 1, 2, 3, 4, 5, 6, 7]
    genOff = (\k -> fromIntegral (k * 4)) <$> choose (0 :: Int, 2)
    genAddr n =
      oneof
        [ (\k -> 4 * k) <$> choose (0 :: Int, 40),
          (\t -> fromIntegral initPc + 4 * t) <$> choose (0, n)
        ]

compositionTests :: TestTree
compositionTests =
  testGroup
    "Core behind a cache"
    [ testProperty "the cached run is the ideal run with stall cycles inserted" $
        withMaxSuccess 1000 $
          forAll genCTProg $ \prog ->
            forAll (choose (1, 4)) $ \penalty ->
              let r = stutterReport penalty 300 prog
               in counterexample r (P.null r),
      testProperty "a secret-free program leaks nothing about its data, behind a cache" $
        withMaxSuccess 1000 $
          forAll genSecretFreeProg $ \prog ->
            forAll genData $ \da ->
              forAll genData $ \db ->
                let s1 = withData da (initSys prog)
                    s2 = withData db (initSys prog)
                 in ctTrace 100 (archOfLeak s1) == ctTrace 100 (archOfLeak s2)
                      P.&& cachedObs 3 300 s1 == cachedObs 3 300 s2,
      testProperty "a load through a loaded pointer leaks it, even to a timing attacker" $
        -- The control: @lw x5, 0(x0); lw x1, 0(x5); lw x2, 0(x0); ebreak@. The
        -- second load's address is data. With the pointer at 0 the second and
        -- third loads hit the line the first one filled; with the pointer at 16,
        -- which maps to the same line, both miss. So even an attacker who sees
        -- only in which cycles a request is made tells the two apart.
        let prog =
              map encode $
                unsafeFromList $
                  [ IType (Load Types.Word Signed) 5 0 0,
                    IType (Load Types.Word Signed) 1 5 0,
                    IType (Load Types.Word Signed) 2 0 0
                  ]
                    P.++ P.replicate 47 Instruction.break
            timing p = P.map (/= NoAccess) (cachedObs 3 100 (withData [(0, p)] (initSys prog)))
         in once (timing 0 /= timing 16)
    ]
