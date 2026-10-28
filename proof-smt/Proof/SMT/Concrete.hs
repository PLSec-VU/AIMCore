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
  ) where

import Data.Maybe (isNothing)
import Stepper.Concrete (concreteAddress, concreteEmbeddings, knownBool)
import SymFC (SymFC (..))
import SymFC.Base qualified as Base
import SymFC.BuiltIn qualified as SymFC
import SymFC.Clash qualified as Clash
import Types (Address)

expectKnownAs :: Address -> Address -> SymFC.Sym ()
expectKnownAs expected address
  | concreteAddress address == Just expected = pure ()
  | otherwise = SymFC.halt

expectNotKnownAs :: Address -> Address -> SymFC.Sym ()
expectNotKnownAs expected address
  | concreteAddress address == Just expected = SymFC.halt
  | otherwise = pure ()

expectUnknown :: Address -> SymFC.Sym ()
expectUnknown address
  | isNothing (concreteAddress address) = pure ()
  | otherwise = SymFC.halt

textBase :: Address
textBase = 0x1000

wordSize :: Address
wordSize = 4

{-# ANN literalAddressIsKnown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
literalAddressIsKnown :: SymFC.Sym ()
literalAddressIsKnown = expectKnownAs 0x1000 textBase

{-# ANN literalAddressWrongValueFails (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
literalAddressWrongValueFails :: SymFC.Sym ()
literalAddressWrongValueFails = expectNotKnownAs 0x1004 textBase

{-# ANN literalArithmeticIsKnown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
literalArithmeticIsKnown :: SymFC.Sym ()
literalArithmeticIsKnown = expectKnownAs 0x1004 (textBase + wordSize)

{-# ANN freshAddressIsUnknown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
freshAddressIsUnknown :: SymFC.Sym ()
freshAddressIsUnknown = SymFC.fresh @Address >>= expectUnknown

{-# ANN mergedAddressIsUnknown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
mergedAddressIsUnknown :: SymFC.Sym ()
mergedAddressIsUnknown = do
  b <- SymFC.fresh @Bool
  expectUnknown (if b then 1 else 2)

expectBoolKnownAs :: Bool -> Bool -> SymFC.Sym ()
expectBoolKnownAs expected value
  | knownBool value == Just expected = pure ()
  | otherwise = SymFC.halt

expectBoolNotKnownAs :: Bool -> Bool -> SymFC.Sym ()
expectBoolNotKnownAs expected value
  | knownBool value == Just expected = SymFC.halt
  | otherwise = pure ()

expectBoolUnknown :: Bool -> SymFC.Sym ()
expectBoolUnknown value
  | isNothing (knownBool value) = pure ()
  | otherwise = SymFC.halt

{-# ANN literalBoolIsKnown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
literalBoolIsKnown :: SymFC.Sym ()
literalBoolIsKnown = expectBoolKnownAs True True

{-# ANN literalBoolWrongValueFails (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
literalBoolWrongValueFails :: SymFC.Sym ()
literalBoolWrongValueFails = expectBoolNotKnownAs False True

{-# ANN literalComparisonIsKnown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
literalComparisonIsKnown :: SymFC.Sym ()
literalComparisonIsKnown = expectBoolKnownAs True (textBase + wordSize == 0x1004)

{-# ANN freshBoolIsUnknown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
freshBoolIsUnknown :: SymFC.Sym ()
freshBoolIsUnknown = SymFC.fresh @Bool >>= expectBoolUnknown

{-# ANN mergedBoolIsUnknown (SymFC $ Base.embeddings <> Clash.embeddings <> concreteEmbeddings) #-}
mergedBoolIsUnknown :: SymFC.Sym ()
mergedBoolIsUnknown = do
  b <- SymFC.fresh @Bool
  expectBoolUnknown (if b then True else False)
