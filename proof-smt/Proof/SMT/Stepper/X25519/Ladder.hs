{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.X25519.Ladder
  ( x25519LadderFirstStepOutOfFuel
  , x25519LadderSecondStepLeaks
  , x25519LadderPublicScalarTwoStepsOutOfFuel
  , x25519LadderDecodeAgrees
  ) where

import Proof.SMT.Stepper.X25519.Call
import Stepper (Fuel)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.ElfCode (elfCode, expectDecodeAgreement)
import Stepper.RelState (RelState)
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_x25519"
    [ ( "ladder"
      , [ "crypto_scalarmult_curve25519_ref10.part.0"
        , "memcpy"
        , "fe25519_frombytes"
        , "fe25519_cswap"
        , "fe25519_mul"
        , "fe25519_sq"
        , "__muldi3"
        , "__mulsi3"
        ]
      )
    ])

sharedPoint, scalarIn, publicPoint :: Address
sharedPoint = 0x1000
scalarIn = 0x1100
publicPoint = 0x1200

scalarWords :: Int
scalarWords = 8

basePointU :: [Word]
basePointU = [9, 0, 0, 0, 0, 0, 0, 0]

untilSecondStepSwap, throughSecondStepSwap :: Fuel
untilSecondStepSwap = decimalSteps [2, 9, 7, 8, 7]
throughSecondStepSwap = decimalSteps [2, 9, 7, 8, 8]

publicScalar :: [Word]
publicScalar = [0x5a5a5a5a, 0x5a5a5a5a, 0x5a5a5a5a, 0x5a5a5a5a, 0x5a5a5a5a, 0x5a5a5a5a, 0x5a5a5a5a, 0x5a5a5a5a]

untilThirdStep :: Fuel
untilThirdStep = decimalSteps [8, 3, 6, 7, 0]

firstSwapBranch :: Address
firstSwapBranch = 0x1e158

ladderCall :: Fuel -> SymFC.Sym Verdict
ladderCall = ladderCallWith (withSecretWords scalarIn scalarWords)

ladderCallWith :: (RelState -> SymFC.Sym RelState) -> Fuel -> SymFC.Sym Verdict
ladderCallWith withScalar fuel = do
  start <- callFrom ladderCode ladderEntry (pointer sharedPoint) (pointer scalarIn) (pointer publicPoint)
  ready <-
    withScalar start
      >>= withPublicWords publicPoint basePointU
      >>= withStackGuardPointer
  relRun fuel ready

{-# ANN x25519LadderFirstStepOutOfFuel (SymFC ['stepperEmbeddings]) #-}
x25519LadderFirstStepOutOfFuel :: SymFC.Sym ()
x25519LadderFirstStepOutOfFuel = expectOutOfFuel $ ladderCall untilSecondStepSwap

{-# ANN x25519LadderSecondStepLeaks (SymFC ['stepperEmbeddings]) #-}
x25519LadderSecondStepLeaks :: SymFC.Sym ()
x25519LadderSecondStepLeaks = expectLeakAt firstSwapBranch LeakBranch $ ladderCall throughSecondStepSwap

{-# ANN x25519LadderPublicScalarTwoStepsOutOfFuel (SymFC ['stepperEmbeddings]) #-}
x25519LadderPublicScalarTwoStepsOutOfFuel :: SymFC.Sym ()
x25519LadderPublicScalarTwoStepsOutOfFuel =
  expectOutOfFuel $ ladderCallWith (withPublicWords scalarIn publicScalar) untilThirdStep

{-# ANN x25519LadderDecodeAgrees (SymFC ['stepperEmbeddings]) #-}
x25519LadderDecodeAgrees :: SymFC.Sym ()
x25519LadderDecodeAgrees = expectDecodeAgreement [ladderDecoded]
