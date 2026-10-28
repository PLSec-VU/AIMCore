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
concreteAddress :: Address -> SymFC.Sym (Maybe Address)
concreteAddress = pure . Just

{-# OPAQUE concreteRegIdx #-}
concreteRegIdx :: RegIdx -> SymFC.Sym (Maybe RegIdx)
concreteRegIdx = pure . Just

{-# OPAQUE knownBool #-}
knownBool :: Bool -> SymFC.Sym (Maybe Bool)
knownBool = pure . Just

fromKnown :: SymFC.Known a -> Maybe a
fromKnown (SymFC.Known value) = Just value
fromKnown SymFC.NotKnown = Nothing

concreteAddressE
  :: forall (bvA :: Nat -> Type)
   . Embeddable bvA Clash.BitVec
  => bvA 32
  -> SymFC.Sym (Maybe (bvA 32))
concreteAddressE
  | Embedding <- embedding @bvA = coerce go
  where
    go :: Clash.BitVec 32 -> SymFC.Sym (Maybe (Clash.BitVec 32))
    go (Clash.BitVecP address) = fmap Clash.BitVecP . fromKnown <$> SymFC.concrete address

concreteRegIdxE
  :: forall (bvA :: Nat -> Type)
   . Embeddable bvA Clash.BitVec
  => bvA 5
  -> SymFC.Sym (Maybe (bvA 5))
concreteRegIdxE
  | Embedding <- embedding @bvA = coerce go
  where
    go :: Clash.BitVec 5 -> SymFC.Sym (Maybe (Clash.BitVec 5))
    go (Clash.BitVecP idx) = fmap Clash.BitVecP . fromKnown <$> SymFC.concrete idx

knownBoolE :: Bool -> SymFC.Sym (Maybe Bool)
knownBoolE value = fromKnown <$> SymFC.concrete value

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
