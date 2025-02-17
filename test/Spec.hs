module Main where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import qualified HardwareSim
import Instruction
import Pipe
import Regfile
import Simulate
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, repeat, undefined, (!!), (&&), (++), (||))
import qualified Prelude

main :: IO ()
main = defaultMain tests

data CPUTest n = CPUTest
  { testProg :: Vec n Word,
    testExpected :: [(Int, Word)]
  }
  deriving (Show, Eq)

mkPureTest :: (KnownNat (RAM_SIZE + (n + 20))) => String -> CPUTest n -> TestTree
mkPureTest s (CPUTest prog expected) =
  testCase s $
    let ram = memRAM $ simToHalt $ mkRAM prog
     in forM_ expected $ \(loc, res) ->
          ram !! loc @?= res

mkCmpTest :: (KnownNat (RAM_SIZE + (n + 20))) => String -> Vec n Word -> TestTree
mkCmpTest s prog =
  testCase s $
    let pure_mem0 = simToHalt' $ mkRAM prog
        clash_mem0 = HardwareSim.simToHalt prog
     in pure_mem0 @?= clash_mem0

tests :: TestTree
tests =
  testGroup
    "Haskell simulation tests"
    [ testGroup
        "Basic programs"
        [ mkPureTest
            "test 1"
            CPUTest
              { testProg = prog1,
                testExpected = [(0, 5)]
              },
          mkPureTest
            "test 2"
            CPUTest
              { testProg = prog2,
                testExpected = [(0, 5), (1, 5)]
              },
          mkPureTest
            "test 3"
            CPUTest
              { testProg = prog3,
                testExpected = [(0, 0), (1, 3)]
              },
          mkPureTest
            "sumTo 10"
            CPUTest
              { testProg = sumTo 10,
                testExpected = [(0, sum [0 .. 10])]
              }
        ],
      testGroup
        "Pure and clash simulations should agree."
        [ mkCmpTest "test 1" prog1,
          mkCmpTest "test 2" prog2,
          mkCmpTest "test 3" prog3,
          mkCmpTest "sumTo 10" $ sumTo 10
        ]
    ]

prog1 =
  map encode $
    -- r2 := r0 + 5
    IType (Arith ADD) 2 0 5
      :>
      -- mem[0 + r0] := r2
      SType Word 0 0 2
      :> halt
      :> Nil

prog2 =
  map encode $
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
      SType Word 1 0 4
      :> halt
      :> Nil

prog3 =
  map encode $
    -- r2 := r0 + 3
    IType (Arith ADD) 2 0 3
      :>
      -- r3 := r0 + r2
      RType ADD 3 0 2
      :>
      -- r2 == r3 ? jump pc + 2
      BType EQ 2 2 3
      :>
      -- mem[0 + r0] := r2
      SType Word 0 0 2
      :>
      -- mem[1 + r0] := r2
      SType Word 1 0 2
      :> halt
      :> Nil

sumTo :: Int -> Vec 8 Word
sumTo n =
  map encode $
    unsafeFromList
      [ -- r1 := r0 + n
        IType (Arith ADD) 1 0 $ fromIntegral n,
        -- r2 := 0 (res = 0)
        IType (Arith ADD) 2 0 0,
        -- r1 == r0 ? jump pc + 4
        BType EQ 4 1 0,
        -- r2 := r2 + r1 (res += n)
        RType ADD 2 2 1,
        -- r1 := r1 - 1 (n -= 1)
        IType (Arith ADD) 1 1 (-1),
        -- jump back to the branch
        JType 0 (-3),
        -- mem[0] := r2
        SType Word 0 0 2,
        halt
      ]
