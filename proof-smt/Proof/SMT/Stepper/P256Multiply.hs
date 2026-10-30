{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.P256Multiply
  ( p256MulUnoptimisedSecretRightLeaks
  , p256MulUnoptimisedSecretLeftNoLeak
  , p256MulUnoptimisedSecretLeftOneStepShortOutOfFuel
  , p256MulOptimisedSecretRightLeaks
  , p256MulOptimisedSecretLeftLeaks
  , p256SqrUnoptimisedLeaks
  , p256SqrOptimisedLeaks
  , p256MontReduceOrderUnoptimisedLeaks
  , p256MontReduceOrderOptimisedNoLeak
  , p256MontReduceOrderOptimisedOneStepShortOutOfFuel
  , p256MultiplyUnoptimisedDecodeAgrees
  , p256MultiplyOptimisedDecodeAgrees
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
    [ ("mulUnoptimised", ["sp_256_mul_9", "__muldi3", "memcpy"])
    , ("sqrUnoptimised", ["sp_256_sqr_9", "__muldi3", "memcpy"])
    , ( "montReduceOrderUnoptimised"
      , [ "sp_256_mont_reduce_order_9"
        , "__mulsi3"
        , "sp_256_cond_sub_9"
        , "sp_256_mont_shift_9"
        , "memset"
        , "sp_256_mul_add_9"
        , "__muldi3"
        , "sp_256_norm_9"
        ]
      )
    ])

$(elfCode "benchmark/bench_ecdsa_sha256_O3"
    [ ("mulOptimised", ["sp_256_mul_9", "__muldi3", "memcpy"])
    , ("sqrOptimised", ["sp_256_sqr_9", "__muldi3", "memcpy"])
    , ( "montReduceOrderOptimised"
      , [ "sp_256_mont_reduce_order_9.constprop.0"
        , "sp_256_mul_add_9.constprop.0"
        , "__muldi3"
        , "sp_256_norm_9"
        ]
      )
    ])

leftAddr, rightAddr, resultAddr :: Address
leftAddr = 0x1000
rightAddr = 0x2000
resultAddr = 0x3000

fieldLimbs, productLimbs :: Int
fieldLimbs = 9
productLimbs = 18

curveConstantB :: [Word]
curveConstantB =
  [0x7d2604b, 0x1e71e1f1, 0x14ec3d8e, 0x1a0d6198, 0x86bc651, 0x1eaabb4c, 0xf9ecfae, 0x1b154752, 0x5ac635]

unoptimisedOrder :: Address
unoptimisedOrder = 0x45e7c

montgomeryFactorOfOrder :: Word
montgomeryFactorOfOrder = 0xe00bc4f

unoptimisedMulSteps, unoptimisedSqrSteps, unoptimisedReduceOrderSteps :: Fuel
unoptimisedMulSteps = decimalSteps [5, 2, 1, 7, 7]
unoptimisedSqrSteps = decimalSteps [2, 9, 2, 8, 7]
unoptimisedReduceOrderSteps = decimalSteps [5, 8, 3, 0, 3]

optimisedMulSteps, optimisedSqrSteps, optimisedReduceOrderSteps :: Fuel
optimisedMulSteps = decimalSteps [1, 7, 7, 6, 8]
optimisedSqrSteps = decimalSteps [1, 0, 0, 2, 3]
optimisedReduceOrderSteps = decimalSteps [1, 1, 8, 8, 9]

unoptimisedMulSecretLeftShort, optimisedReduceOrderShort :: Fuel
unoptimisedMulSecretLeftShort = decimalSteps [5, 2, 1, 7, 6]
optimisedReduceOrderShort = decimalSteps [1, 1, 8, 8, 8]

firstUnoptimisedTableLoad, firstOptimisedTableLoad :: Address
firstUnoptimisedTableLoad = 0x1b2d4
firstOptimisedTableLoad = 0x21ca8

mulSecretRight :: Code -> Address -> Fuel -> SymFC.Sym Verdict
mulSecretRight code entry fuel = do
  start <- callAt code entry [pointer resultAddr, pointer leftAddr, pointer rightAddr]
  withLeft <- withPublicWords leftAddr fieldLimbs start
  withRight <- withSecretWords rightAddr fieldLimbs withLeft
  relRun fuel withRight

mulSecretLeftByCurveConstant :: Code -> Address -> Fuel -> SymFC.Sym Verdict
mulSecretLeftByCurveConstant code entry fuel = do
  start <- callAt code entry [pointer resultAddr, pointer leftAddr, pointer rightAddr]
  withLeft <- withSecretWords leftAddr fieldLimbs start
  withRight <- withKnownWords rightAddr curveConstantB withLeft
  relRun fuel withRight

sqrSecret :: Code -> Address -> Fuel -> SymFC.Sym Verdict
sqrSecret code entry fuel = do
  start <- callAt code entry [pointer resultAddr, pointer leftAddr]
  withInput <- withSecretWords leftAddr fieldLimbs start
  relRun fuel withInput

montReduceOrderUnoptimisedCall :: Fuel -> SymFC.Sym Verdict
montReduceOrderUnoptimisedCall fuel = do
  start <-
    callAt
      montReduceOrderUnoptimisedCode
      montReduceOrderUnoptimisedEntry
      [pointer leftAddr, pointer unoptimisedOrder, Same montgomeryFactorOfOrder]
  withProduct <- withSecretWords leftAddr productLimbs start
  relRun fuel withProduct

montReduceOrderOptimisedCall :: Fuel -> SymFC.Sym Verdict
montReduceOrderOptimisedCall fuel = do
  start <- callAt montReduceOrderOptimisedCode montReduceOrderOptimisedEntry [pointer leftAddr]
  withProduct <- withSecretWords leftAddr productLimbs start
  relRun fuel withProduct

{-# ANN p256MulUnoptimisedSecretRightLeaks (SymFC ['stepperEmbeddings]) #-}
p256MulUnoptimisedSecretRightLeaks :: SymFC.Sym ()
p256MulUnoptimisedSecretRightLeaks =
  expectLeakAt firstUnoptimisedTableLoad LeakLoadAddress $
    mulSecretRight mulUnoptimisedCode mulUnoptimisedEntry unoptimisedMulSteps

{-# ANN p256MulUnoptimisedSecretLeftNoLeak (SymFC ['stepperEmbeddings]) #-}
p256MulUnoptimisedSecretLeftNoLeak :: SymFC.Sym ()
p256MulUnoptimisedSecretLeftNoLeak =
  expectNoLeak $ mulSecretLeftByCurveConstant mulUnoptimisedCode mulUnoptimisedEntry unoptimisedMulSteps

{-# ANN p256MulUnoptimisedSecretLeftOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256MulUnoptimisedSecretLeftOneStepShortOutOfFuel :: SymFC.Sym ()
p256MulUnoptimisedSecretLeftOneStepShortOutOfFuel =
  expectOutOfFuel $ mulSecretLeftByCurveConstant mulUnoptimisedCode mulUnoptimisedEntry unoptimisedMulSecretLeftShort

{-# ANN p256MulOptimisedSecretRightLeaks (SymFC ['stepperEmbeddings]) #-}
p256MulOptimisedSecretRightLeaks :: SymFC.Sym ()
p256MulOptimisedSecretRightLeaks =
  expectLeakAt firstOptimisedTableLoad LeakLoadAddress $
    mulSecretRight mulOptimisedCode mulOptimisedEntry optimisedMulSteps

{-# ANN p256MulOptimisedSecretLeftLeaks (SymFC ['stepperEmbeddings]) #-}
p256MulOptimisedSecretLeftLeaks :: SymFC.Sym ()
p256MulOptimisedSecretLeftLeaks =
  expectLeakAt firstOptimisedTableLoad LeakLoadAddress $
    mulSecretLeftByCurveConstant mulOptimisedCode mulOptimisedEntry optimisedMulSteps

{-# ANN p256SqrUnoptimisedLeaks (SymFC ['stepperEmbeddings]) #-}
p256SqrUnoptimisedLeaks :: SymFC.Sym ()
p256SqrUnoptimisedLeaks =
  expectLeakAt firstUnoptimisedTableLoad LeakLoadAddress $
    sqrSecret sqrUnoptimisedCode sqrUnoptimisedEntry unoptimisedSqrSteps

{-# ANN p256SqrOptimisedLeaks (SymFC ['stepperEmbeddings]) #-}
p256SqrOptimisedLeaks :: SymFC.Sym ()
p256SqrOptimisedLeaks =
  expectLeakAt firstOptimisedTableLoad LeakLoadAddress $
    sqrSecret sqrOptimisedCode sqrOptimisedEntry optimisedSqrSteps

{-# ANN p256MontReduceOrderUnoptimisedLeaks (SymFC ['stepperEmbeddings]) #-}
p256MontReduceOrderUnoptimisedLeaks :: SymFC.Sym ()
p256MontReduceOrderUnoptimisedLeaks =
  expectLeakAt firstUnoptimisedTableLoad LeakLoadAddress $
    montReduceOrderUnoptimisedCall unoptimisedReduceOrderSteps

{-# ANN p256MontReduceOrderOptimisedNoLeak (SymFC ['stepperEmbeddings]) #-}
p256MontReduceOrderOptimisedNoLeak :: SymFC.Sym ()
p256MontReduceOrderOptimisedNoLeak = expectNoLeak $ montReduceOrderOptimisedCall optimisedReduceOrderSteps

{-# ANN p256MontReduceOrderOptimisedOneStepShortOutOfFuel (SymFC ['stepperEmbeddings]) #-}
p256MontReduceOrderOptimisedOneStepShortOutOfFuel :: SymFC.Sym ()
p256MontReduceOrderOptimisedOneStepShortOutOfFuel =
  expectOutOfFuel $ montReduceOrderOptimisedCall optimisedReduceOrderShort

{-# ANN p256MultiplyUnoptimisedDecodeAgrees (SymFC ['stepperEmbeddings]) #-}
p256MultiplyUnoptimisedDecodeAgrees :: SymFC.Sym ()
p256MultiplyUnoptimisedDecodeAgrees = expectDecodeAgreement [mulUnoptimisedDecoded, sqrUnoptimisedDecoded, montReduceOrderUnoptimisedDecoded]

{-# ANN p256MultiplyOptimisedDecodeAgrees (SymFC ['stepperEmbeddings]) #-}
p256MultiplyOptimisedDecodeAgrees :: SymFC.Sym ()
p256MultiplyOptimisedDecodeAgrees = expectDecodeAgreement [mulOptimisedDecoded, sqrOptimisedDecoded, montReduceOrderOptimisedDecoded]
