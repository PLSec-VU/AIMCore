{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.P256Field
  ( p256CompareUnoptimisedNoLeak
  , p256CompareUnoptimisedOneStepShortOutOfFuel
  , p256CompareOptimisedNoLeak
  , p256CompareOptimisedOneStepShortOutOfFuel
  , p256CondSubUnoptimisedNoLeak
  , p256CondSubUnoptimisedOneStepShortOutOfFuel
  , p256CondCopyUnoptimisedNoLeak
  , p256CondCopyUnoptimisedOneStepShortOutOfFuel
  , p256MontReduceUnoptimisedNoLeak
  , p256MontReduceUnoptimisedOneStepShortOutOfFuel
  , p256MontReduceOptimisedNoLeak
  , p256MontReduceOptimisedOneStepShortOutOfFuel
  , p256FieldUnoptimisedDecodeAgrees
  , p256FieldOptimisedDecodeAgrees
  , p256UnoptimisedDecodeRejectsWrongInstruction
  , p256OptimisedDecodeRejectsWrongInstruction
  ) where

import Proof.SMT.Stepper.P256Call
import Stepper (Fuel)
import Stepper.ElfCode (elfCode, expectDecodeAgreement, expectDecodeDisagreement, withWrongFirstInstruction)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import Stepper.Concrete (stepperEmbeddings)
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_ecdsa_sha256_O0"
    [ ("compareUnoptimised", ["sp_256_cmp_9"])
    , ("condSubUnoptimised", ["sp_256_cond_sub_9"])
    , ("condCopyUnoptimised", ["sp_256_cond_copy_9"])
    , ("montReduceUnoptimised", ["sp_256_mont_reduce_9"])
    ])

$(elfCode "benchmark/bench_ecdsa_sha256_O3"
    [ ("compareOptimised", ["sp_256_cmp_9"])
    , ("montReduceOptimised", ["sp_256_mont_reduce_9.constprop.0"])
    ])

leftAddr, rightAddr, resultAddr :: Address
leftAddr = 0x1000
rightAddr = 0x2000
resultAddr = 0x3000

fieldLimbs, productLimbs :: Int
fieldLimbs = 9
productLimbs = 18

unoptimisedModulus :: Address
unoptimisedModulus = 0x45e34

montgomeryFactorOfModulus :: Word
montgomeryFactorOfModulus = 1

secretWord :: SymFC.Sym (Rel Word)
secretWord = Pair <$> SymFC.fresh @Word <*> SymFC.fresh @Word

compareCall :: Code -> Address -> Fuel -> SymFC.Sym Verdict
compareCall code entry fuel = do
  start <- callAt code entry [pointer leftAddr, pointer rightAddr]
  withLeft <- withSecretWords leftAddr fieldLimbs start
  withRight <- withSecretWords rightAddr fieldLimbs withLeft
  relRun fuel withRight

condSubCall :: Fuel -> SymFC.Sym Verdict
condSubCall fuel = do
  mask <- secretWord
  start <- callAt condSubUnoptimisedCode condSubUnoptimisedEntry [pointer resultAddr, pointer leftAddr, pointer rightAddr, mask]
  withLeft <- withSecretWords leftAddr fieldLimbs start
  withRight <- withSecretWords rightAddr fieldLimbs withLeft
  relRun fuel withRight

condCopyCall :: Fuel -> SymFC.Sym Verdict
condCopyCall fuel = do
  mask <- secretWord
  start <- callAt condCopyUnoptimisedCode condCopyUnoptimisedEntry [pointer resultAddr, pointer leftAddr, mask]
  withSource <- withSecretWords leftAddr fieldLimbs start
  withTarget <- withSecretWords resultAddr fieldLimbs withSource
  relRun fuel withTarget

montReduceUnoptimisedCall :: Fuel -> SymFC.Sym Verdict
montReduceUnoptimisedCall fuel = do
  start <-
    callAt
      montReduceUnoptimisedCode
      montReduceUnoptimisedEntry
      [pointer leftAddr, pointer unoptimisedModulus, Same montgomeryFactorOfModulus]
  withProduct <- withSecretWords leftAddr productLimbs start
  relRun fuel withProduct

montReduceOptimisedCall :: Fuel -> SymFC.Sym Verdict
montReduceOptimisedCall fuel = do
  start <- callAt montReduceOptimisedCode montReduceOptimisedEntry [pointer leftAddr]
  withProduct <- withSecretWords leftAddr productLimbs start
  relRun fuel withProduct

{-# ANN p256CompareUnoptimisedNoLeak (SymFC ['stepperEmbeddings]) #-}
p256CompareUnoptimisedNoLeak :: SymFC.Sym ()
p256CompareUnoptimisedNoLeak =
  expectNoLeak $ compareCall compareUnoptimisedCode compareUnoptimisedEntry (decimalSteps [1, 4, 2])

{-# ANN p256CompareUnoptimisedOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256CompareUnoptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256CompareUnoptimisedOneStepShortOutOfFuel =
  expectOutOfFuel $ compareCall compareUnoptimisedCode compareUnoptimisedEntry (decimalSteps [1, 4, 1])

{-# ANN p256CompareOptimisedNoLeak (SymFC ['stepperEmbeddings]) #-}
p256CompareOptimisedNoLeak :: SymFC.Sym ()
p256CompareOptimisedNoLeak =
  expectNoLeak $ compareCall compareOptimisedCode compareOptimisedEntry (decimalSteps [6, 9])

{-# ANN p256CompareOptimisedOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256CompareOptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256CompareOptimisedOneStepShortOutOfFuel =
  expectOutOfFuel $ compareCall compareOptimisedCode compareOptimisedEntry (decimalSteps [6, 8])

{-# ANN p256CondSubUnoptimisedNoLeak (SymFC ['stepperEmbeddings]) #-}
p256CondSubUnoptimisedNoLeak :: SymFC.Sym ()
p256CondSubUnoptimisedNoLeak = expectNoLeak $ condSubCall (decimalSteps [1, 1, 9])

{-# ANN p256CondSubUnoptimisedOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256CondSubUnoptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256CondSubUnoptimisedOneStepShortOutOfFuel = expectOutOfFuel $ condSubCall (decimalSteps [1, 1, 8])

{-# ANN p256CondCopyUnoptimisedNoLeak (SymFC ['stepperEmbeddings]) #-}
p256CondCopyUnoptimisedNoLeak :: SymFC.Sym ()
p256CondCopyUnoptimisedNoLeak = expectNoLeak $ condCopyCall (decimalSteps [1, 7, 1])

{-# ANN p256CondCopyUnoptimisedOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256CondCopyUnoptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256CondCopyUnoptimisedOneStepShortOutOfFuel = expectOutOfFuel $ condCopyCall (decimalSteps [1, 7, 0])

{-# ANN p256MontReduceUnoptimisedNoLeak (SymFC ['stepperEmbeddings]) #-}
p256MontReduceUnoptimisedNoLeak :: SymFC.Sym ()
p256MontReduceUnoptimisedNoLeak = expectNoLeak $ montReduceUnoptimisedCall (decimalSteps [1, 7, 5, 7])

{-# ANN p256MontReduceUnoptimisedOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256MontReduceUnoptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256MontReduceUnoptimisedOneStepShortOutOfFuel =
  expectOutOfFuel $ montReduceUnoptimisedCall (decimalSteps [1, 7, 5, 6])

{-# ANN p256MontReduceOptimisedNoLeak (SymFC ['stepperEmbeddings]) #-}
p256MontReduceOptimisedNoLeak :: SymFC.Sym ()
p256MontReduceOptimisedNoLeak = expectNoLeak $ montReduceOptimisedCall (decimalSteps [4, 6, 9])

{-# ANN p256MontReduceOptimisedOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256MontReduceOptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256MontReduceOptimisedOneStepShortOutOfFuel =
  expectOutOfFuel $ montReduceOptimisedCall (decimalSteps [4, 6, 8])

{-# ANN p256FieldUnoptimisedDecodeAgrees (SymFC ['stepperEmbeddings]) #-}
p256FieldUnoptimisedDecodeAgrees :: SymFC.Sym ()
p256FieldUnoptimisedDecodeAgrees = expectDecodeAgreement [compareUnoptimisedDecoded, condSubUnoptimisedDecoded, condCopyUnoptimisedDecoded, montReduceUnoptimisedDecoded]

{-# ANN p256FieldOptimisedDecodeAgrees (SymFC ['stepperEmbeddings]) #-}
p256FieldOptimisedDecodeAgrees :: SymFC.Sym ()
p256FieldOptimisedDecodeAgrees = expectDecodeAgreement [compareOptimisedDecoded, montReduceOptimisedDecoded]

{-# ANN p256UnoptimisedDecodeRejectsWrongInstruction (SymFC ['stepperEmbeddings]) #-}
p256UnoptimisedDecodeRejectsWrongInstruction :: SymFC.Sym ()
p256UnoptimisedDecodeRejectsWrongInstruction = expectDecodeDisagreement [withWrongFirstInstruction compareUnoptimisedDecoded, condSubUnoptimisedDecoded, condCopyUnoptimisedDecoded, montReduceUnoptimisedDecoded]

{-# ANN p256OptimisedDecodeRejectsWrongInstruction (SymFC ['stepperEmbeddings]) #-}
p256OptimisedDecodeRejectsWrongInstruction :: SymFC.Sym ()
p256OptimisedDecodeRejectsWrongInstruction = expectDecodeDisagreement [withWrongFirstInstruction compareOptimisedDecoded, montReduceOptimisedDecoded]
