{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Concrete
  ( literalAddressIsKnown
  , literalAddressWrongValueFails
  , literalArithmeticIsKnown
  , freshAddressIsUnknown
  , mergedAddressIsUnknown
  , literalBoolIsKnown
  , literalBoolWrongValueFails
  , literalComparisonIsKnown
  , freshBoolIsUnknown
  , mergedBoolIsUnknown
  , xorWithSelfIsZero
  , closureOverConstantIsKnown
  , closureOverConstantWrongValueFails
  , closureOverFreshIsUnknown
  , closureOverForcedFreshIsUnknown
  , closureIgnoringFreshIsKnown
  , sectionOverConstantIsKnown
  , sectionOverFreshIsUnknown
  , branchingClosureIsUnknown
  , closureFromConstantBranchIsKnown
  , closureOverUnforcedDifferenceIsUnknown
  , closureOverForcedXorIsKnown
  , recursiveClosureOverConstantIsKnown
  , recursiveClosureOverFreshIsUnknown
  , justConstantIsKnown
  , justConstantWrongValueFails
  , justFreshIsUnknown
  , pairWithIdentityIsKnown
  ) where

import Data.Bits (xor)
import Data.Maybe (isNothing)
import Stepper.Concrete (concreteAddress, knownBool, stepperEmbeddings)
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address)

expectKnownAs :: Address -> Address -> SymFC.Sym ()
expectKnownAs expected address = do
  known <- concreteAddress address
  if known == Just expected then pure () else SymFC.halt

expectNotKnownAs :: Address -> Address -> SymFC.Sym ()
expectNotKnownAs expected address = do
  known <- concreteAddress address
  if known == Just expected then SymFC.halt else pure ()

expectUnknown :: Address -> SymFC.Sym ()
expectUnknown address = do
  known <- concreteAddress address
  if isNothing known then pure () else SymFC.halt

textBase :: Address
textBase = 0x1000

wordSize :: Address
wordSize = 4

{-# ANN literalAddressIsKnown (SymFCWith 'stepperEmbeddings) #-}
literalAddressIsKnown :: SymFC.Sym ()
literalAddressIsKnown = expectKnownAs 0x1000 textBase

{-# ANN literalAddressWrongValueFails (SymFCWith 'stepperEmbeddings) #-}
literalAddressWrongValueFails :: SymFC.Sym ()
literalAddressWrongValueFails = expectNotKnownAs 0x1004 textBase

{-# ANN literalArithmeticIsKnown (SymFCWith 'stepperEmbeddings) #-}
literalArithmeticIsKnown :: SymFC.Sym ()
literalArithmeticIsKnown = expectKnownAs 0x1004 (textBase + wordSize)

{-# ANN freshAddressIsUnknown (SymFCWith 'stepperEmbeddings) #-}
freshAddressIsUnknown :: SymFC.Sym ()
freshAddressIsUnknown = SymFC.fresh @Address >>= expectUnknown

{-# ANN mergedAddressIsUnknown (SymFCWith 'stepperEmbeddings) #-}
mergedAddressIsUnknown :: SymFC.Sym ()
mergedAddressIsUnknown = do
  b <- SymFC.fresh @Bool
  expectUnknown (if b then 1 else 2)

expectBoolKnownAs :: Bool -> Bool -> SymFC.Sym ()
expectBoolKnownAs expected value = do
  known <- knownBool value
  if known == Just expected then pure () else SymFC.halt

expectBoolNotKnownAs :: Bool -> Bool -> SymFC.Sym ()
expectBoolNotKnownAs expected value = do
  known <- knownBool value
  if known == Just expected then SymFC.halt else pure ()

expectBoolUnknown :: Bool -> SymFC.Sym ()
expectBoolUnknown value = do
  known <- knownBool value
  if isNothing known then pure () else SymFC.halt

{-# ANN literalBoolIsKnown (SymFCWith 'stepperEmbeddings) #-}
literalBoolIsKnown :: SymFC.Sym ()
literalBoolIsKnown = expectBoolKnownAs True True

{-# ANN literalBoolWrongValueFails (SymFCWith 'stepperEmbeddings) #-}
literalBoolWrongValueFails :: SymFC.Sym ()
literalBoolWrongValueFails = expectBoolNotKnownAs False True

{-# ANN literalComparisonIsKnown (SymFCWith 'stepperEmbeddings) #-}
literalComparisonIsKnown :: SymFC.Sym ()
literalComparisonIsKnown = expectBoolKnownAs True (textBase + wordSize == 0x1004)

{-# ANN freshBoolIsUnknown (SymFCWith 'stepperEmbeddings) #-}
freshBoolIsUnknown :: SymFC.Sym ()
freshBoolIsUnknown = SymFC.fresh @Bool >>= expectBoolUnknown

{-# ANN mergedBoolIsUnknown (SymFCWith 'stepperEmbeddings) #-}
mergedBoolIsUnknown :: SymFC.Sym ()
mergedBoolIsUnknown = do
  b <- SymFC.fresh @Bool
  expectBoolUnknown ((if b then 1 else 2 :: Address) == 1)

{-# ANN xorWithSelfIsZero (SymFCWith 'stepperEmbeddings) #-}
xorWithSelfIsZero :: SymFC.Sym ()
xorWithSelfIsZero = do
  word <- SymFC.fresh @Address
  expectBoolKnownAs True (word `xor` word == 0)

knownAnd :: (a -> Bool) -> a -> SymFC.Sym Bool
knownAnd property value = do
  known <- SymFC.concrete value
  pure $ case known of
    SymFC.Known value' -> property value'
    SymFC.NotKnown -> False

expectKnownWith :: (a -> Bool) -> a -> SymFC.Sym ()
expectKnownWith property value = do
  holds <- knownAnd property value
  if holds then pure () else SymFC.halt

refuteKnownWith :: (a -> Bool) -> a -> SymFC.Sym ()
refuteKnownWith property value = do
  holds <- knownAnd property value
  if holds then SymFC.halt else pure ()

expectNotKnown :: a -> SymFC.Sym ()
expectNotKnown = refuteKnownWith (const True)

{-# OPAQUE addTo #-}
addTo :: Word -> Word -> Word
addTo k = \y -> y + k

{-# OPAQUE pickAdder #-}
pickAdder :: Bool -> Word -> Word
pickAdder b = if b then (+ 1) else (+ 2)

{-# OPAQUE subtractUntilBelow #-}
subtractUntilBelow :: Word -> Word -> Word
subtractUntilBelow k = go
  where
    go n = if n < k then n else go (n - k)

{-# ANN closureOverConstantIsKnown (SymFCWith 'stepperEmbeddings) #-}
closureOverConstantIsKnown :: SymFC.Sym ()
closureOverConstantIsKnown = expectKnownWith (\f -> f 1 == 6) (addTo 5)

{-# ANN closureOverConstantWrongValueFails (SymFCWith 'stepperEmbeddings) #-}
closureOverConstantWrongValueFails :: SymFC.Sym ()
closureOverConstantWrongValueFails = refuteKnownWith (\f -> f 1 == 7) (addTo 5)

{-# ANN closureOverFreshIsUnknown (SymFCWith 'stepperEmbeddings) #-}
closureOverFreshIsUnknown :: SymFC.Sym ()
closureOverFreshIsUnknown = do
  x <- SymFC.fresh @Word
  expectNotKnown (addTo x)

{-# ANN closureOverForcedFreshIsUnknown (SymFCWith 'stepperEmbeddings) #-}
closureOverForcedFreshIsUnknown :: SymFC.Sym ()
closureOverForcedFreshIsUnknown = do
  x <- SymFC.fresh @Word
  x `seq` expectNotKnown (addTo x)

{-# ANN closureIgnoringFreshIsKnown (SymFCWith 'stepperEmbeddings) #-}
closureIgnoringFreshIsKnown :: SymFC.Sym ()
closureIgnoringFreshIsKnown = do
  x <- SymFC.fresh @Word
  let k = 5 :: Word
  x `seq` expectKnownWith (\f -> f 1 == 6) (\y -> y + k)

{-# ANN sectionOverConstantIsKnown (SymFCWith 'stepperEmbeddings) #-}
sectionOverConstantIsKnown :: SymFC.Sym ()
sectionOverConstantIsKnown = expectKnownWith (\f -> f 1 == 4) (+ (3 :: Word))

{-# ANN sectionOverFreshIsUnknown (SymFCWith 'stepperEmbeddings) #-}
sectionOverFreshIsUnknown :: SymFC.Sym ()
sectionOverFreshIsUnknown = do
  x <- SymFC.fresh @Word
  x `seq` expectNotKnown (+ x)

{-# ANN branchingClosureIsUnknown (SymFCWith 'stepperEmbeddings) #-}
branchingClosureIsUnknown :: SymFC.Sym ()
branchingClosureIsUnknown = do
  b <- SymFC.fresh @Bool
  expectNotKnown (if b then (+ 1) else (+ (2 :: Word)))

{-# ANN closureFromConstantBranchIsKnown (SymFCWith 'stepperEmbeddings) #-}
closureFromConstantBranchIsKnown :: SymFC.Sym ()
closureFromConstantBranchIsKnown = expectKnownWith (\f -> f 1 == 2) (pickAdder True)

{-# ANN closureOverUnforcedDifferenceIsUnknown (SymFCWith 'stepperEmbeddings) #-}
closureOverUnforcedDifferenceIsUnknown :: SymFC.Sym ()
closureOverUnforcedDifferenceIsUnknown = do
  x <- SymFC.fresh @Word
  let z = x - x
  x `seq` expectNotKnown (\y -> y + z)

{-# ANN closureOverForcedXorIsKnown (SymFCWith 'stepperEmbeddings) #-}
closureOverForcedXorIsKnown :: SymFC.Sym ()
closureOverForcedXorIsKnown = do
  x <- SymFC.fresh @Word
  let isZero = x `xor` x == 0
  isZero `seq` expectKnownWith (\f -> f 1 == 1) (\y -> if isZero then y else y + 1)

{-# ANN recursiveClosureOverConstantIsKnown (SymFCWith 'stepperEmbeddings) #-}
recursiveClosureOverConstantIsKnown :: SymFC.Sym ()
recursiveClosureOverConstantIsKnown = expectKnownWith (\f -> f 12 == 2) (subtractUntilBelow 5)

{-# ANN recursiveClosureOverFreshIsUnknown (SymFCWith 'stepperEmbeddings) #-}
recursiveClosureOverFreshIsUnknown :: SymFC.Sym ()
recursiveClosureOverFreshIsUnknown = do
  x <- SymFC.fresh @Word
  x `seq` expectNotKnown (subtractUntilBelow x)

{-# ANN justConstantIsKnown (SymFCWith 'stepperEmbeddings) #-}
justConstantIsKnown :: SymFC.Sym ()
justConstantIsKnown = expectKnownWith (== Just 5) (Just (5 :: Word))

{-# ANN justConstantWrongValueFails (SymFCWith 'stepperEmbeddings) #-}
justConstantWrongValueFails :: SymFC.Sym ()
justConstantWrongValueFails = refuteKnownWith (== Just 6) (Just (5 :: Word))

{-# ANN justFreshIsUnknown (SymFCWith 'stepperEmbeddings) #-}
justFreshIsUnknown :: SymFC.Sym ()
justFreshIsUnknown = do
  x <- SymFC.fresh @Word
  x `seq` expectNotKnown (Just x)

{-# ANN pairWithIdentityIsKnown (SymFCWith 'stepperEmbeddings) #-}
pairWithIdentityIsKnown :: SymFC.Sym ()
pairWithIdentityIsKnown = expectKnownWith (\(n, f) -> f n == 5) (5 :: Word, \y -> y)
