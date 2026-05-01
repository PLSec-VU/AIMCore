{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Access
import BenchmarkSpec (benchmarkTests)
import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import Control.Monad.Identity (Identity(..))
import Core
import Data.Maybe (fromJust, isJust)
import Instruction
import InstructionSpec (instructionTests)
import qualified Leak.PC.PC as Leak.PC
import qualified Leak.SecretPC.PC as Leak.SecretPC
import Simulate
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
import TheoremSpec (nonInterferenceTheorem, simulatorTheorem)
import Types
import Util
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

-- mkCmpTest :: String -> Vec n Word -> TestTree
-- mkCmpTest s prog =
--  testCase s $
--    let pure_mem0 = simToHalt' $ mkRAM prog
--        clash_mem0 = HardwareSim.simToHalt prog
--     in pure_mem0 @?= clash_mem0

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ instructionTests,
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
            ],
          testGroup
            "PC leak"
            [ mkPCLeakTest "test 1" $ mkProg prog1,
              mkPCLeakTest "test 2" $ mkProg prog1,
              mkPCLeakTest "test 3" $ mkProg prog3,
              mkPCLeakTest "sumTo 10" $
                mkProg $
                  sumTo
                    10,
              testProperty
                "LeakPC Simulator"
                $ withMaxSuccess 500000
                $ simulatorTheorem Leak.PC.proj Leak.PC.leak Leak.PC.sim Core.circuit Leak.PC.obs,
              testProperty "Non-interference" $
                withMaxSuccess 500000 $
                  nonInterferenceTheorem Leak.PC.proj Leak.PC.leak Core.circuit Leak.PC.obs
            ],
          testGroup
            "SecretPC leak"
            [ mkSecretPCLeakTest "test 1" $ mkProg prog1,
              mkSecretPCLeakTest "test 1" $ mkProg prog1,
              mkSecretPCLeakTest "test 2" $ mkProg prog1,
              mkSecretPCLeakTest "test 3" $ mkProg prog1,
              mkSecretPCLeakTest "sumTo 10" $ mkProg $ sumTo 10,
              testProperty "SecretPC Simulator" $
                withMaxSuccess 500000 $
                  simulatorTheorem
                    Leak.SecretPC.proj
                    Leak.SecretPC.leak
                    Leak.SecretPC.sim
                    Core.circuit
                    Leak.SecretPC.obs,
              testProperty "SecretPC Non-interference" $
                withMaxSuccess 5000000 $
                  nonInterferenceTheorem Leak.SecretPC.proj Leak.SecretPC.leak Leak.SecretPC.implementation Leak.SecretPC.obs
            ]
            -- testGroup
            --  "Pure and clash simulations should agree."
            --  [ mkCmpTest "test 1" prog1,
            --    mkCmpTest "test 2" prog2,
            --    mkCmpTest "test 3" prog3,
            --    mkCmpTest "sumTo 10" $ sumTo 10
            --  ]
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
      <*> genMaybeRegFwd
      <*> genMaybeRegFwd
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
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

instance Arbitrary Core.HaltState where
  arbitrary = elements [Core.Running, Core.EBreak, Core.SecurityViolation]

instance {-# OVERLAPPING #-} (Access f) => Arbitrary (Core.State f) where
  arbitrary =
    Core.State
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genAccessWord
      <*> genAccessWord
      <*> arbitrary
      <*> genAccessWord
      <*> arbitrary
      <*> arbitrary
    where
      genAccessWord = do
        isSecret <- arbitrary
        word <- arbitrary
        pure $ conditionalSecret isSecret word

instance {-# OVERLAPPING #-} (Access f) => Arbitrary (Input f) where
  arbitrary = do
    isInstr <- arbitrary
    mem <-
      if isInstr
        then fromJust <$> ((encode' <$> arbitrary) `suchThat` isJust)
        else arbitrary
    r1 <- arbitrary
    r2 <- arbitrary
    isSecretMem <- arbitrary
    isSecretR1 <- arbitrary
    isSecretR2 <- arbitrary
    pure $
      Input
        isInstr
        (conditionalSecret isSecretMem mem)
        (conditionalSecret isSecretR1 r1)
        (conditionalSecret isSecretR2 r2)

