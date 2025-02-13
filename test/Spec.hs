module Main where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
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

mkTest :: (KnownNat (RAM_SIZE + (n + 20))) => String -> CPUTest n -> TestTree
mkTest s (CPUTest prog expected) =
  testCase s $
    let ram = memRAM $ simToHalt $ mkRAM prog
     in forM_ expected $ \(loc, res) ->
          ram !! loc @?= res

tests :: TestTree
tests =
  testGroup
    "Haskell simulation tests"
    [ testGroup
        "Basic programs"
        [ mkTest
            "test 1"
            CPUTest
              { testProg =
                  map encode $
                    -- r2 := r0 + 5
                    IType (Arith ADD) 2 0 5
                      :>
                      -- mem[0 + r0] := r2
                      SType Word 0 0 2
                      :> halt
                      :> Nil,
                testExpected = [(0, 5)]
              },
          mkTest
            "test 2"
            CPUTest
              { testProg =
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
                      :> Nil,
                testExpected = [(0, 5), (1, 5)]
              },
          mkTest
            "test 3"
            CPUTest
              { testProg =
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
                      :> Nil,
                testExpected = [(0, 0), (1, 3)]
              },
          mkTest
            "sumTo 10"
            CPUTest
              { testProg = sumTo 10,
                testExpected = [(0, sum [0 .. 10])]
              }
        ]
    ]

sumTo :: Int -> Vec 8 Word
sumTo n =
  map encode $
    unsafeFromList
      [ -- r1 := r0 + n
        IType (Arith ADD) 1 0 $ fromIntegral n,
        -- r2 := 0 (res = 0)
        IType (Arith ADD) 2 0 0,
        -- r1 == r0 ? jump pc + 5
        BType EQ 4 1 0,
        -- r2 := r2 + r1 (res += n)
        RType ADD 2 2 3,
        -- r1 := r1 - 1 (n -= 1)
        IType (Arith ADD) 1 1 (-1),
        -- jump back to the branch
        JType 0 (-3),
        -- store result in mem[0]
        SType Word 0 0 2,
        halt
      ]
