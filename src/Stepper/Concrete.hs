{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Stepper.Concrete
  ( concreteAddress
  , concreteAddressE
  , concreteRegIdx
  , concreteRegIdxE
  , concreteEmbeddings
  , knownBool
  , knownBoolE
  , stepperEmbeddings
  ) where

import Clash.Prelude (Nat, Type)
import Data.Coerce (coerce)
import SymFC (Embeddings (..))
import SymFC.Base qualified as Base
import SymFC.BuiltIn (Embeddable, Embedding (..), embedding)
import SymFC.BuiltIn qualified as SymFC
import SymFC.Clash qualified as Clash
import Types (Address, RegIdx)

{-# OPAQUE concreteAddress #-}
concreteAddress :: Address -> Maybe Address
concreteAddress = Just

{-# OPAQUE concreteRegIdx #-}
concreteRegIdx :: RegIdx -> Maybe RegIdx
concreteRegIdx = Just

{-# OPAQUE knownBool #-}
knownBool :: Bool -> Maybe Bool
knownBool = Just

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

concreteRegIdxE
  :: forall (bvA :: Nat -> Type)
   . Embeddable bvA Clash.BitVec
  => bvA 5
  -> Maybe (bvA 5)
concreteRegIdxE
  | Embedding <- embedding @bvA = coerce go
  where
    go :: Clash.BitVec 5 -> Maybe (Clash.BitVec 5)
    go (Clash.BitVecP idx) = Clash.BitVecP <$> SymFC.concrete idx

knownBoolE :: Bool -> Maybe Bool
knownBoolE value = case SymFC.concreteBool (SymFC.boolean value) of
  Nothing -> Nothing
  Just known -> Just (SymFC.ite known True False)

concreteEmbeddings :: Embeddings
concreteEmbeddings =
  Embeddings
    { typeEmbeddings = []
    , termEmbeddings =
        [ ('concreteAddress, 'concreteAddressE)
        , ('concreteRegIdx, 'concreteRegIdxE)
        , ('knownBool, 'knownBoolE)
        ]
    }

stepperEmbeddings :: Embeddings
stepperEmbeddings = Base.embeddings <> Clash.embeddings <> concreteEmbeddings
