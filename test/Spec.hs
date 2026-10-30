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
import RegFile
import Simulate
import Memory.Types
import Memory.Vec
import qualified Prelude
import IsaSpec (isaConformanceTests)
import ProofSpec (proofTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
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


tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ proofTests,
      isaConformanceTests,
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
      <*> arbitrary

-- | Half the time a valid instruction encoding, half the time an arbitrary data
-- word. Whether the core treats it as an instruction is up to the state's
-- 'Core.stateDeExpInstr'.
instance {-# OVERLAPPING #-} (Access f) => Arbitrary (Input f) where
  arbitrary = do
    mem <-
      oneof
        [ fromJust <$> ((encode' <$> arbitrary) `suchThat` isJust),
          arbitrary
        ]
    isSecretMem <- arbitrary
    pure $ Input (conditionalSecret isSecretMem mem)
