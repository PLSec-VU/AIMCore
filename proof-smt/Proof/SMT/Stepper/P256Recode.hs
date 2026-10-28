{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.P256Recode
  ( p256RecodeUnoptimisedLeaks
  , p256RecodeOptimisedLeaks
  , p256GetPointUnoptimisedNoLeak
  , p256GetPointUnoptimisedOneStepShortOutOfFuel
  , p256RecodeUnoptimisedDecodeAgrees
  , p256RecodeOptimisedDecodeAgrees
  ) where

import Proof.SMT.Stepper.P256Call
import Stepper (Fuel)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.ElfCode (elfCode, expectDecodeAgreement)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_ecdsa_sha256_O0"
    [ ("recodeUnoptimised", ["sp_256_ecc_recode_6_9"])
    , ("getPointUnoptimised", ["sp_256_get_point_33_9"])
    ])

$(elfCode "benchmark/bench_ecdsa_sha256_O3"
    [ ("recodeOptimised", ["sp_256_ecc_recode_6_9"])
    ])

scalarAddr, digitsAddr, pointAddr, tableAddr :: Address
scalarAddr = 0x1000
digitsAddr = 0x2000
pointAddr = 0x3000
tableAddr = 0x10000

scalarLimbs :: Int
scalarLimbs = 9

unoptimisedRecodeSteps, optimisedRecodeSteps :: Fuel
unoptimisedRecodeSteps = decimalSteps [2, 8, 4, 9]
optimisedRecodeSteps = decimalSteps [7, 7, 8]

getPointSteps, getPointShort :: Fuel
getPointSteps = decimalSteps [1, 6, 0, 4, 2]
getPointShort = decimalSteps [1, 6, 0, 4, 1]

unoptimisedRecodeIndexLoad, optimisedRecodeIndexLoad :: Address
unoptimisedRecodeIndexLoad = 0x268c8
optimisedRecodeIndexLoad = 0x18cb4

recodeSecretScalar :: Code -> Address -> Fuel -> SymFC.Sym Verdict
recodeSecretScalar code entry fuel = do
  start <- callAt code entry [pointer scalarAddr, pointer digitsAddr]
  withScalar <- withSecretWords scalarAddr scalarLimbs start
  relRun fuel withScalar

getPointSecretIndex :: Fuel -> SymFC.Sym Verdict
getPointSecretIndex fuel = do
  indexA <- SymFC.fresh @Word
  indexB <- SymFC.fresh @Word
  start <- callAt getPointUnoptimisedCode getPointUnoptimisedEntry [pointer pointAddr, pointer tableAddr, Pair indexA indexB]
  relRun fuel start

{-# ANN p256RecodeUnoptimisedLeaks (SymFC stepperEmbeddings) #-}
p256RecodeUnoptimisedLeaks :: SymFC.Sym ()
p256RecodeUnoptimisedLeaks =
  expectLeakAt unoptimisedRecodeIndexLoad LeakLoadAddress $
    recodeSecretScalar recodeUnoptimisedCode recodeUnoptimisedEntry unoptimisedRecodeSteps

{-# ANN p256RecodeOptimisedLeaks (SymFC stepperEmbeddings) #-}
p256RecodeOptimisedLeaks :: SymFC.Sym ()
p256RecodeOptimisedLeaks =
  expectLeakAt optimisedRecodeIndexLoad LeakLoadAddress $
    recodeSecretScalar recodeOptimisedCode recodeOptimisedEntry optimisedRecodeSteps

{-# ANN p256GetPointUnoptimisedNoLeak (SymFC stepperEmbeddings) #-}
p256GetPointUnoptimisedNoLeak :: SymFC.Sym ()
p256GetPointUnoptimisedNoLeak = expectNoLeak $ getPointSecretIndex getPointSteps

{-# ANN p256GetPointUnoptimisedOneStepShortOutOfFuel (SymFC stepperEmbeddings) #-}
p256GetPointUnoptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256GetPointUnoptimisedOneStepShortOutOfFuel = expectOutOfFuel $ getPointSecretIndex getPointShort

{-# ANN p256RecodeUnoptimisedDecodeAgrees (SymFC stepperEmbeddings) #-}
p256RecodeUnoptimisedDecodeAgrees :: SymFC.Sym ()
p256RecodeUnoptimisedDecodeAgrees = expectDecodeAgreement [recodeUnoptimisedDecoded, getPointUnoptimisedDecoded]

{-# ANN p256RecodeOptimisedDecodeAgrees (SymFC stepperEmbeddings) #-}
p256RecodeOptimisedDecodeAgrees :: SymFC.Sym ()
p256RecodeOptimisedDecodeAgrees = expectDecodeAgreement [recodeOptimisedDecoded]
