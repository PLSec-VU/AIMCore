{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Concrete
  ( literalAddressIsKnown
  , literalAddressWrongValueFails
  , literalArithmeticIsKnown
  , freshAddressIsUnknown
  , mergedAddressIsUnknown
  ) where

import Data.Maybe (isNothing)
import Stepper.Concrete (concreteAddress, concreteEmbeddings)
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
