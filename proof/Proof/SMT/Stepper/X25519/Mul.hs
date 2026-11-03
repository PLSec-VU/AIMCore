{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.X25519.Mul
  ( x25519MulPublicNoLeak
  , x25519MulPublicOneStepShort
  , x25519MulSecretLeaks
  , x25519ToBytesSecretNoLeak
  , x25519ToBytesSecretOneStepShort
  , x25519MulSymbolicThreeBits
  , x25519MulSymbolicFiveBits
  , x25519MulDecodeAgrees
  , x25519MulFetchesByAddress
  ) where

import Proof.SMT.Stepper.X25519.Call
import Stepper (Fuel)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.ElfCode (elfCode, expectDecodeAgreement, expectFetchByAddress)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_x25519"
    [ ("multiply", ["fe25519_mul", "__muldi3", "__mulsi3"])
    , ("toBytes", ["fe25519_tobytes", "fe25519_reduce"])
    ])

limbCount :: Int
limbCount = 10

productElement, multiplicand, multiplier :: Address
productElement = 0x1000
multiplicand = 0x1100
multiplier = 0x1200

publicMultiplicand, publicMultiplier :: [Word]
publicMultiplicand = [0x1234567, 0x1244668, 0x1254769, 0x126486a, 0x127496b, 0x1284a6c, 0x1294b6d, 0x12a4c6e, 0x12b4d6f, 0x12c4e70]
publicMultiplier = [0x0abcdef, 0x0abee0f, 0x0ac0e2f, 0x0ac2e4f, 0x0ac4e6f, 0x0ac6e8f, 0x0ac8eaf, 0x0acaecf, 0x0acceef, 0x0acef0f]

multiplySteps, multiplyOneShort, pastFirstLimbProduct :: Fuel
multiplySteps = decimalSteps [2, 8, 1, 7, 6]
multiplyOneShort = decimalSteps [2, 8, 1, 7, 5]
pastFirstLimbProduct = decimalSteps [3, 0, 0]

firstMultiplierBitBranch :: Address
firstMultiplierBitBranch = 0x42680

bytesOut, elementIn :: Address
bytesOut = 0x1000
elementIn = 0x1100

toBytesSteps, toBytesOneShort :: Fuel
toBytesSteps = decimalSteps [1, 9, 0]
toBytesOneShort = decimalSteps [1, 8, 9]

multiplyCall :: (RelState -> SymFC.Sym RelState) -> Fuel -> SymFC.Sym Verdict
multiplyCall withFactors fuel = do
  start <- callFrom multiplyCode multiplyEntry (pointer productElement) (pointer multiplicand) (pointer multiplier)
  relRun fuel =<< withFactors start

withPublicFactors :: RelState -> SymFC.Sym RelState
withPublicFactors st = withPublicWords multiplicand publicMultiplicand st >>= withPublicWords multiplier publicMultiplier

withSecretFactors :: RelState -> SymFC.Sym RelState
withSecretFactors st = withSecretWords multiplicand limbCount st >>= withSecretWords multiplier limbCount

{-# ANN x25519MulPublicNoLeak (SymFC ['stepperEmbeddings]) #-}
x25519MulPublicNoLeak :: SymFC.Sym ()
x25519MulPublicNoLeak = expectNoLeak $ multiplyCall withPublicFactors multiplySteps

{-# ANN x25519MulPublicOneStepShort (SymFC ['stepperEmbeddings]) #-}
x25519MulPublicOneStepShort :: SymFC.Sym ()
x25519MulPublicOneStepShort = expectOutOfFuel $ multiplyCall withPublicFactors multiplyOneShort

{-# ANN x25519MulSecretLeaks (SymFC ['stepperEmbeddings]) #-}
x25519MulSecretLeaks :: SymFC.Sym ()
x25519MulSecretLeaks = expectLeakAt firstMultiplierBitBranch $ multiplyCall withSecretFactors pastFirstLimbProduct

toBytesCall :: Fuel -> SymFC.Sym Verdict
toBytesCall fuel = do
  start <- callFrom toBytesCode toBytesEntry (pointer bytesOut) (pointer elementIn) (Same 0)
  relRun fuel =<< withSecretWords elementIn limbCount start

{-# ANN x25519ToBytesSecretNoLeak (SymFC ['stepperEmbeddings]) #-}
x25519ToBytesSecretNoLeak :: SymFC.Sym ()
x25519ToBytesSecretNoLeak = expectNoLeak $ toBytesCall toBytesSteps

{-# ANN x25519ToBytesSecretOneStepShort (SymFC ['stepperEmbeddings]) #-}
x25519ToBytesSecretOneStepShort :: SymFC.Sym ()
x25519ToBytesSecretOneStepShort = expectOutOfFuel $ toBytesCall toBytesOneShort

withPublicSymbolicWords :: Address -> Int -> RelState -> SymFC.Sym RelState
withPublicSymbolicWords _ 0 st = pure st
withPublicSymbolicWords addr remaining st = do
  value <- SymFC.fresh @Word
  withPublicWord addr value st >>= withPublicSymbolicWords (addr + 4) (remaining - 1)

withSymbolicFactors :: RelState -> SymFC.Sym RelState
withSymbolicFactors st = withPublicSymbolicWords multiplicand limbCount st >>= withPublicSymbolicWords multiplier limbCount

{-# ANN x25519MulSymbolicThreeBits (SymFC ['stepperEmbeddings]) #-}
x25519MulSymbolicThreeBits :: SymFC.Sym ()
x25519MulSymbolicThreeBits = expectOutOfFuel $ multiplyCall withSymbolicFactors (decimalSteps [2, 3, 3])

{-# ANN x25519MulSymbolicFiveBits (SymFC ['stepperEmbeddings]) #-}
x25519MulSymbolicFiveBits :: SymFC.Sym ()
x25519MulSymbolicFiveBits = expectOutOfFuel $ multiplyCall withSymbolicFactors (decimalSteps [2, 5, 7])

{-# ANN x25519MulDecodeAgrees (SymFC ['stepperEmbeddings]) #-}
x25519MulDecodeAgrees :: SymFC.Sym ()
x25519MulDecodeAgrees = expectDecodeAgreement [multiplyDecoded, toBytesDecoded]

{-# ANN x25519MulFetchesByAddress (SymFC ['stepperEmbeddings]) #-}
x25519MulFetchesByAddress :: SymFC.Sym ()
x25519MulFetchesByAddress = expectFetchByAddress [multiplyCode, toBytesCode]
