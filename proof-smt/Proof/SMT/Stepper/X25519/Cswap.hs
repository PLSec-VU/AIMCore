{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.X25519.Cswap
  ( x25519CswapPublicBitNoLeak
  , x25519CswapPublicBitOneStepShort
  , x25519CswapSecretBitLeaks
  , x25519CmovSecretBitNoLeak
  , x25519CmovSecretBitOneStepShort
  , x25519CswapDecodeAgrees
  , x25519DecodeRejectsWrongInstruction
  ) where

import Proof.SMT.Stepper.X25519.Call
import Stepper (Fuel)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.ElfCode (elfCode, expectDecodeAgreement, expectDecodeDisagreement, withWrongFirstInstruction)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_x25519"
    [ ("cswap", ["fe25519_cswap"])
    , ("cmov", ["fe25519_cmov"])
    ])

limbCount :: Int
limbCount = 10

firstElement, secondElement :: Address
firstElement = 0x1000
secondElement = 0x1100

publicElement :: [Word]
publicElement = [0x1234567, 0x1244668, 0x1254769, 0x126486a, 0x127496b, 0x1284a6c, 0x1294b6d, 0x12a4c6e, 0x12b4d6f, 0x12c4e70]

swapWithBitSteps, swapWithBitOneShort :: Fuel
swapWithBitSteps = decimalSteps [1, 1, 2]
swapWithBitOneShort = decimalSteps [1, 1, 1]

moveSteps, moveOneShort :: Fuel
moveSteps = decimalSteps [6, 3]
moveOneShort = decimalSteps [6, 2]

firstSwapBranch :: Address
firstSwapBranch = 0x1e158

withSecretElements :: RelState -> SymFC.Sym RelState
withSecretElements st = withSecretWords firstElement limbCount st >>= withSecretWords secondElement limbCount

withPublicElements :: RelState -> SymFC.Sym RelState
withPublicElements st = withPublicWords firstElement publicElement st >>= withPublicWords secondElement publicElement

secretBit :: SymFC.Sym (Rel Word)
secretBit = Pair <$> SymFC.fresh @Word <*> SymFC.fresh @Word

conditionalCall :: Code -> Address -> (RelState -> SymFC.Sym RelState) -> Fuel -> Rel Word -> SymFC.Sym Verdict
conditionalCall code entry withElements fuel bit = do
  start <- callFrom code entry (pointer firstElement) (pointer secondElement) bit
  relRun fuel =<< withElements start

swapSecrets :: Fuel -> SymFC.Sym Verdict
swapSecrets fuel = conditionalCall cswapCode cswapEntry withSecretElements fuel (Same 1)

{-# ANN x25519CswapPublicBitNoLeak (SymFCWith 'stepperEmbeddings) #-}
x25519CswapPublicBitNoLeak :: SymFC.Sym ()
x25519CswapPublicBitNoLeak = expectNoLeak $ swapSecrets swapWithBitSteps

{-# ANN x25519CswapPublicBitOneStepShort (SymFCWith 'stepperEmbeddings) #-}
x25519CswapPublicBitOneStepShort :: SymFC.Sym ()
x25519CswapPublicBitOneStepShort = expectOutOfFuel $ swapSecrets swapWithBitOneShort

{-# ANN x25519CswapSecretBitLeaks (SymFCWith 'stepperEmbeddings) #-}
x25519CswapSecretBitLeaks :: SymFC.Sym ()
x25519CswapSecretBitLeaks =
  expectLeakAt firstSwapBranch LeakBranch $
    conditionalCall cswapCode cswapEntry withPublicElements swapWithBitSteps =<< secretBit

moveUnderSecretBit :: Fuel -> SymFC.Sym Verdict
moveUnderSecretBit fuel = conditionalCall cmovCode cmovEntry withSecretElements fuel =<< secretBit

{-# ANN x25519CmovSecretBitNoLeak (SymFCWith 'stepperEmbeddings) #-}
x25519CmovSecretBitNoLeak :: SymFC.Sym ()
x25519CmovSecretBitNoLeak = expectNoLeak $ moveUnderSecretBit moveSteps

{-# ANN x25519CmovSecretBitOneStepShort (SymFCWith 'stepperEmbeddings) #-}
x25519CmovSecretBitOneStepShort :: SymFC.Sym ()
x25519CmovSecretBitOneStepShort = expectOutOfFuel $ moveUnderSecretBit moveOneShort

{-# ANN x25519CswapDecodeAgrees (SymFCWith 'stepperEmbeddings) #-}
x25519CswapDecodeAgrees :: SymFC.Sym ()
x25519CswapDecodeAgrees = expectDecodeAgreement [cswapDecoded, cmovDecoded]

{-# ANN x25519DecodeRejectsWrongInstruction (SymFCWith 'stepperEmbeddings) #-}
x25519DecodeRejectsWrongInstruction :: SymFC.Sym ()
x25519DecodeRejectsWrongInstruction = expectDecodeDisagreement [withWrongFirstInstruction cswapDecoded, cmovDecoded]
