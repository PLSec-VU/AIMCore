{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Access
import BenchmarkSpec (benchmarkTests)
import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import Core
import Data.Maybe (fromJust, isJust)
import Instruction
import InstructionSpec (instructionTests)
import LeakageSpec (leakageTests)
import qualified Leak.MonitorPC.PC as Leak.MonitorPC
import qualified Leak.PC.PC as Leak.PC
import qualified Leak.SecretPC.PC as Leak.SecretPC
import RegFile
import Simulate
import Memory.Types
import Memory.Vec
import qualified Proof.SMT.Sanity as Sanity
import qualified Prelude
import qualified Proof.Functional.Induction
import qualified Proof.Leakage.Induction
import ProofSpec (proofTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
import TheoremSpec (nonInterferenceTheorem, simulatorTheorem)
import "aimcore" Types
import "aimcore" Util
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (!!), (&&), (++), (||))


main :: IO ()
main = defaultMain tests

data CPUTest = CPUTest
  { testProg :: Vec PROG_SIZE Word,
    testExpected :: [(Int, Word)]
  }
  deriving (Show, Eq)

mkPureTest :: String -> CPUTest -> TestTree
mkPureTest s (CPUTest prog expected) =
  testCase s $
    let ram = simResult @RAM_SIZE_BYTES prog
     in forM_ expected $ \(loc, res) ->
          readWord (fromIntegral loc) ram @?= res

mkPCLeakTest :: String -> Vec PROG_SIZE Word -> TestTree
mkPCLeakTest s prog =
  testCase s $
    assertBool "" $
      Leak.PC.pcsEqual prog

mkSecretPCLeakTest :: String -> Vec PROG_SIZE Word -> TestTree
mkSecretPCLeakTest s prog =
  testCase s $
    assertBool "" $
      Leak.SecretPC.pcsEqual prog

-- | The compile-time symbolic checks, read back out.
--
-- The first three establish that the Pantomime plugin is wired in and actually
-- discharging properties -- since pantomime 1821a71 an invalid property no
-- longer fails the build, so the negative control has to be asserted here.
-- The rest are the refinement proof itself; see "Proof.Functional.Induction".
sanityTests :: TestTree
sanityTests =
  testGroup
    "Symbolic proof results"
    [ testCase "plugin sanity: deMorgan is valid" $ verdict "deMorgan" @?= Nothing,
      testCase "plugin sanity: doubling is valid" $ verdict "doubling" @?= Nothing,
      testCase "plugin sanity: negative control yields a counterexample" $
        assertBool "expected a counterexample for 'bogus'" $
          isJust (verdict "bogus"),
      testCase "array embedding round-trips" $
        lookup "arrRoundTrip" Proof.Functional.Induction.results @?= Just Nothing,
      testCase "shift embeddings are sane" $
        lookup "shiftsSane" Proof.Functional.Induction.results @?= Just Nothing,
      testCase "base case: invariant holds at reset" $
        lookup "baseCase" Proof.Functional.Induction.results @?= Just Nothing,
      testCase "k = 0 inductive step is valid" $
        lookup "indStep0" Proof.Functional.Induction.results @?= Just Nothing,
      testCase "k = 1 inductive step is valid" $
        lookup "indStep1" Proof.Functional.Induction.results @?= Just Nothing,
      testCase "k = 2 inductive step is valid" $
        lookup "indStep2" Proof.Functional.Induction.results @?= Just Nothing,
      testCase "k = 3 inductive step is valid" $
        lookup "indStep3" Proof.Functional.Induction.results @?= Just Nothing,
      testCase "k = 0 leakage step is valid" $
        lookup "leakStep0" Proof.Leakage.Induction.results @?= Just Nothing,
      testCase "k = 1 leakage step is valid" $
        lookup "leakStep1" Proof.Leakage.Induction.results @?= Just Nothing,
      testCase "k = 2 leakage step is valid" $
        lookup "leakStep2" Proof.Leakage.Induction.results @?= Just Nothing,
      testCase "k = 3 leakage step is valid" $
        lookup "leakStep3" Proof.Leakage.Induction.results @?= Just Nothing
    ]
  where
    verdict :: String -> Maybe String
    verdict name = case lookup name Sanity.results of
      Just v -> v
      Nothing -> error "Proof.SMT.Sanity.results is missing an expected entry"

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ sanityTests,
      proofTests,
      leakageTests,
      instructionTests,
      testGroup
        "Haskell simulation tests"
        [ testGroup
            "Basic programs"
            [ mkPureTest
                "test 1"
                CPUTest
                  { testProg = mkProg prog1,
                    testExpected = [(0, 5)]
                  },
              mkPureTest
                "test 2"
                CPUTest
                  { testProg = mkProg prog2,
                    testExpected = [(0, 5), (4, 5)]
                  },
              mkPureTest
                "test 3"
                CPUTest
                  { testProg = mkProg prog3,
                    testExpected = [(0, 0), (4, 3)]
                  },
              mkPureTest
                "sumTo 10"
                CPUTest
                  { testProg = mkProg $ sumTo 10,
                    testExpected = [(0, sum [0 .. 10])]
                  }
            ]
          -- testGroup
          --   "PC leak"
          --   [ mkPCLeakTest "test 1" $ mkProg prog1,
          --     mkPCLeakTest "test 2" $ mkProg prog1,
          --     mkPCLeakTest "test 3" $ mkProg prog1
          --     {-
          --     , testProperty "PC Simulator" $
          --       withMaxSuccess 500000 $
          --         simulatorTheorem
          --           Leak.PC.proj
          --           Leak.PC.leak
          --           Leak.PC.sim
          --           Core.circuit
          --           Leak.PC.obs,
          --     testProperty "Non-interference" $
          --       withMaxSuccess 500000 $
          --         nonInterferenceTheorem Leak.PC.proj Leak.PC.leak Core.circuit Leak.PC.obs
          --     -}
          --   ],
          -- testGroup
          --   "SecretPC leak"
          --   [ mkSecretPCLeakTest "test 1" $ mkProg prog1,
          --     mkSecretPCLeakTest "test 1" $ mkProg prog1,
          --     mkSecretPCLeakTest "test 2" $ mkProg prog1,
          --     mkSecretPCLeakTest "test 3" $ mkProg prog1,
          --     mkSecretPCLeakTest "sumTo 10" $ mkProg $ sumTo 10
          --     {-
          --     , testProperty "SecretPC Simulator" $
          --       withMaxSuccess 500000 $
          --         simulatorTheorem
          --           Leak.SecretPC.proj
          --           Leak.SecretPC.leak
          --           Leak.SecretPC.sim
          --           Core.circuit
          --           Leak.SecretPC.obs,
          --     testProperty "MonitorPC Non-interference" $
          --       withMaxSuccess 500000 $
          --         nonInterferenceTheorem
          --           Leak.MonitorPC.proj
          --           Leak.MonitorPC.leak
          --           Core.circuit
          --           Leak.MonitorPC.obs
          --     -}
          --   ]
        ],
      benchmarkTests
    ]

prog1 :: Vec 3 Instruction
prog1 =
  -- r2 := r0 + 5
  IType (Arith ADD) 2 0 5
    :>
    -- mem[0 + r0] := r2
    SType Word 0 0 2
    :> break
    :> Nil

prog2 :: Vec 6 Instruction
prog2 =
  -- r2 := r0 + 5
  IType (Arith ADD) 2 0 5
    :>
    -- mem[0 + r0] := r2
    SType Word 0 0 2
    :>
    -- r3 := mem[r0 + 0],
    IType (Load Word Signed) 3 0 0
    :>
    -- r4 := r0 + r3
    RType ADD 4 0 3
    :>
    -- mem[1 + r0] := r4
    SType Word 4 0 4
    :> break
    :> Nil

prog3 :: Vec 6 Instruction
prog3 =
  -- r2 := r0 + 3
  IType (Arith ADD) 2 0 3
    :>
    -- r3 := r0 + r2
    RType ADD 3 0 2
    :>
    -- r2 == r3 ? jump pc + 8
    BType EQ 8 2 3
    :>
    -- mem[0 + r0] := r2
    SType Word 0 0 2
    :>
    -- mem[1 + r0] := r2
    SType Word 4 0 2
    :> break
    :> Nil

sumTo :: Int -> Vec 8 Instruction
sumTo n =
  unsafeFromList
    [ -- r1 := r0 + n
      IType (Arith ADD) 1 0 $ fromIntegral n,
      -- r2 := 0 (res = 0)
      IType (Arith ADD) 2 0 0,
      -- r1 == r0 ? jump pc + 16
      BType EQ 16 1 0,
      -- r2 := r2 + r1 (res += n)
      RType ADD 2 2 1,
      -- r1 := r1 - 1 (n -= 1)
      IType (Arith ADD) 1 1 (-1),
      -- jump back to the branch
      JType 0 (-12),
      -- mem[0] := r2
      SType Word 0 0 2,
      break
    ]

genEnumBound :: (Enum a, Bounded a) => Gen a
genEnumBound = chooseEnum (minBound, maxBound)

instance Arbitrary Arith where
  arbitrary = genEnumBound

instance Arbitrary Comparison where
  arbitrary = genEnumBound

instance Arbitrary Size where
  arbitrary = genEnumBound

instance Arbitrary Sign where
  arbitrary = genEnumBound

instance Arbitrary UBase where
  arbitrary = genEnumBound

instance Arbitrary Env where
  arbitrary = genEnumBound

instance Arbitrary IOperation where
  arbitrary =
    oneof
      [ Arith
          <$> elements
            [ ADD,
              XOR,
              OR,
              AND,
              SLL,
              SRL,
              SRA,
              SLT,
              SLTU
            ],
        (uncurry Load)
          <$> elements
            [ (Byte, Signed),
              (Half, Signed),
              (Word, Signed),
              (Byte, Unsigned),
              (Half, Unsigned)
            ],
        pure Jump
      ]

instance Arbitrary Instruction where
  arbitrary =
    resize 10 $
      oneof
        [ RType <$> arbitrary <*> regIdxGen <*> regIdxGen <*> regIdxGen,
          (IType <$> arbitrary <*> regIdxGen <*> regIdxGen <*> immGen)
            `suchThat` ( \instr -> case instr of
                           IType (Arith arith) _ _ imm ->
                             case arith of
                               SLL -> slice d11 d5 imm == 0
                               SRL -> slice d11 d5 imm == 0
                               SRA -> slice d11 d5 imm == 0
                               _ -> True
                           _ -> True
                       ),
          SType <$> arbitrary <*> immGen <*> regIdxGen <*> regIdxGen,
          BType <$> arbitrary <*> bImmGen <*> regIdxGen <*> regIdxGen,
          UType <$> arbitrary <*> regIdxGen <*> uImmGen,
          JType <$> regIdxGen <*> jImmGen
        ]
    where
      regIdxGen = chooseBoundedIntegral (0, 31)
      immGen = chooseBoundedIntegral (0, 5)
      uImmGen = chooseBoundedIntegral (0, 5)
      bImmGen = chooseBoundedIntegral (0, 5)
      jImmGen = chooseBoundedIntegral (0, 5)

instance {-# OVERLAPPING #-} (Access f) => Arbitrary (Control f) where
  arbitrary =
    Control
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMaybeRegFwd
      <*> genMaybeRegFwd
    where
      genAccessWord = do
        isSecret <- arbitrary
        word <- arbitrary
        pure $ conditionalSecret isSecret word
      genMaybeRegFwd = do
        hasFwd <- arbitrary
        if hasFwd
          then do
            regIdx <- arbitrary
            accessWord <- genAccessWord
            pure $ Just (regIdx, accessWord)
          else pure Nothing

instance (Arbitrary a) => Arbitrary (PubSec a) where
  arbitrary = oneof [Public <$> arbitrary, Secret <$> arbitrary]

instance Arbitrary Core.HaltState where
  arbitrary = oneof
    [ Core.EBreak <$> arbitrary
    , Core.Syscall <$> arbitrary
    , pure Core.SecurityViolation
    ]

instance (Access f, Arbitrary (f Word)) => Arbitrary (RegFile f) where
  arbitrary = do
    vals <- vectorOf 31 arbitrary
    pure $ RegFile $ unsafeFromList vals

instance {-# OVERLAPPING #-} (Access f, Arbitrary (f Word)) => Arbitrary (Core.State f) where
  arbitrary =
    Core.State
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance {-# OVERLAPPING #-} (Access f) => Arbitrary (Input f) where
  arbitrary = do
    isInstr <- arbitrary
    mem <-
      if isInstr
        then fromJust <$> ((encode' <$> arbitrary) `suchThat` isJust)
        else arbitrary
    isSecretMem <- arbitrary
    pure $
      Input
        isInstr
        (conditionalSecret isSecretMem mem)
