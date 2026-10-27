{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Stepper.Concrete
  ( concreteAddress
  , concreteAddressE
  , concreteEmbeddings
  , stepperEmbeddings
  ) where

import Clash.Prelude (Nat, Type)
import Data.Coerce (coerce)
import SymFC (Embeddings (..))
import SymFC.Base qualified as Base
import SymFC.BuiltIn (Embeddable, Embedding (..), embedding)
import SymFC.BuiltIn qualified as SymFC
import SymFC.Clash qualified as Clash
import Types (Address)

{-# OPAQUE concreteAddress #-}
concreteAddress :: Address -> Maybe Address
concreteAddress = Just

concreteAddressE
  :: forall (bvA :: Nat -> Type)
   . Embeddable bvA Clash.BitVec
  => bvA 32
  -> Maybe (bvA 32)
concreteAddressE
  | Embedding <- embedding @bvA = coerce go
  where
    go :: Clash.BitVec 32 -> Maybe (Clash.BitVec 32)
    go (Clash.BitVecP address) = Clash.BitVecP <$> SymFC.concrete address

concreteEmbeddings :: Embeddings
concreteEmbeddings =
  Embeddings
    { typeEmbeddings = []
    , termEmbeddings = [('concreteAddress, 'concreteAddressE)]
    }

stepperEmbeddings :: Embeddings
stepperEmbeddings = Base.embeddings <> Clash.embeddings <> concreteEmbeddings
