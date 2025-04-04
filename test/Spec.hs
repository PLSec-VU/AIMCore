module Main where

import Clash.Prelude hiding (Log, Ordering (..), Word, break, def, init, lift, log, resize)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Monoid
import Debug.Trace
import qualified HardwareSim
import Instruction
import qualified Leak.PC
import qualified Leak.PC.Sim
import qualified Leak.PC.Time
import Pipe
import Regfile
import Simulate
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
import Types
import Util
import Prelude hiding (Ordering (..), Word, break, init, log, map, not, repeat, undefined, (!!), (&&), (++), (||))
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
          testProperty "QuickCheck" $ withMaxSuccess 5000000 theorem
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
    :> break
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
    :> break
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
          IType <$> arbitrary <*> regIdxGen <*> regIdxGen <*> immGen,
          SType <$> arbitrary <*> immGen <*> regIdxGen <*> regIdxGen,
          BType <$> arbitrary <*> immGen <*> regIdxGen <*> regIdxGen,
          UType <$> arbitrary <*> regIdxGen <*> uImmGen,
          JType <$> regIdxGen <*> uImmGen
        ]
    where
      regIdxGen = chooseBoundedIntegral (0, 31)
      immGen = chooseBoundedIntegral (0, 5)
      uImmGen = chooseBoundedIntegral (0, 5)

instance Arbitrary Control where
  arbitrary =
    Control
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Pipe where
  arbitrary =
    Pipe
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

instance Arbitrary Input where
  arbitrary =
    Input
      <$> arbitrary
      <*> oneof
        [ encode <$> arbitrary,
          arbitrary
        ]
      <*> arbitrary
      <*> arbitrary

theorem :: Gen Property
theorem = do
  input <- arbitrary
  pipe <- arbitrary
  let state = Leak.PC.proj $ pipe
      (state_final, sim_pc) = circuit_sim input state
      (pipe_final, pipe_output) = circuit_pipe pipe input
      pipe_pc = obs pipe_output
  pure $
    flip counterexample (sim_pc == pipe_pc) $
      unlines
        [ "input: ",
          "-------------------------------",
          show input,
          "",
          "pipe:",
          "-------------------------------",
          show pipe,
          "",
          "pipe_final:",
          "-------------------------------",
          show pipe_final,
          "",
          "state:",
          "-------------------------------",
          show state,
          "",
          "state_final:",
          "-------------------------------",
          show state_final,
          "",
          "sim_pc:",
          "-------------------------------",
          show sim_pc,
          "",
          "pipe_pc:",
          "-------------------------------",
          show pipe_pc,
          "",
          "pipe_output:",
          "-------------------------------",
          show pipe_output
        ]
  where
    circuit_sim :: Input -> (Leak.PC.Time.State, Leak.PC.Sim.State) -> ((Leak.PC.Time.State, Leak.PC.Sim.State), Maybe Address)
    circuit_sim = Leak.PC.circuit
    circuit_pipe :: Pipe -> Input -> (Pipe, Output)
    circuit_pipe = Pipe.pipe

    obs :: Output -> Maybe Address
    obs sim_pc = do
      mem <- getFirst $ outMem sim_pc
      guard $ memIsInstr mem
      pure $ memAddress mem
