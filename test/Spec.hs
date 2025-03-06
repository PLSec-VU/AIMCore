module Main where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import qualified HardwareSim
import Instruction
import qualified Leak.PC
import Pipe
import Regfile
import Simulate
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, undefined, (!!), (&&), (++), (||))
import qualified Prelude

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
    let ram = simResult prog
     in forM_ expected $ \(loc, res) ->
          readWord (fromIntegral loc) ram @?= res

mkPCLeakTest :: String -> Vec PROG_SIZE Word -> TestTree
mkPCLeakTest s prog =
  testCase s $
    assertBool "" $
      Leak.PC.pcsEqual prog

-- mkCmpTest :: String -> Vec n Word -> TestTree
-- mkCmpTest s prog =
--  testCase s $
--    let pure_mem0 = simToHalt' $ mkRAM prog
--        clash_mem0 = HardwareSim.simToHalt prog
--     in pure_mem0 @?= clash_mem0

tests :: TestTree
tests =
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
          mkPCLeakTest "test 3" $ mkProg prog1,
          mkPCLeakTest "sumTo 10" $ mkProg $ sumTo 10,
          testProperty "QuickCheck" $ Leak.PC.pcsEqual
        ]
        -- testGroup
        --  "Pure and clash simulations should agree."
        --  [ mkCmpTest "test 1" prog1,
        --    mkCmpTest "test 2" prog2,
        --    mkCmpTest "test 3" prog3,
        --    mkCmpTest "sumTo 10" $ sumTo 10
        --  ]
    ]

prog1 =
  -- r2 := r0 + 5
  IType (Arith ADD) 2 0 5
    :>
    -- mem[0 + r0] := r2
    SType Word 0 0 2
    :> halt
    :> Nil

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
    :> halt
    :> Nil

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
    :> halt
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
      halt
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
      [ Arith <$> arbitrary,
        Load <$> arbitrary <*> arbitrary,
        Env <$> arbitrary,
        pure Jump
      ]

instance Arbitrary Instruction where
  arbitrary =
    oneof
      [ RType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        IType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        SType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        BType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        UType <$> arbitrary <*> arbitrary <*> arbitrary,
        JType <$> arbitrary <*> arbitrary,
        pure Invalid,
        pure EBREAK -- (fix, and probably make less common and/or just enforce it comes at the end)
      ]
