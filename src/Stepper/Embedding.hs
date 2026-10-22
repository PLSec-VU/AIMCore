{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoScopedTypeVariables #-}

module Stepper.Embedding
  ( -- embeddings
  ) where

import Prelude hiding (Word, read)
import Stepper.Memory qualified as Stepper (Memory)
import Stepper.Memory qualified as Memory
import SymFC.Base qualified as Base
import SymFC.Clash qualified as Clash
import SymFC (Embeddings (..))
import SymFC.BuiltIn (Embeddable, Embedding (..), embedding)
import SymFC.BuiltIn qualified as SymFC
import Data.Coerce (coerce)
import GHC.TypeNats (type (<=))

embeddings :: Embeddings
embeddings = Base.embeddings <> Clash.embeddings <> embeddings'

embeddings' :: Embeddings
embeddings' = Embeddings
  { typeEmbeddings =
    [ (''Stepper.Memory, ''Memory)
    -- , (''StepperRegFile, ''RegFile)
    ]
  , termEmbeddings =
    [ ('Memory.empty, 'emptyM)
    -- , ('Memory.read, 'readM)
    -- , ('Memory.write, 'writeM)
    -- , ('Memory.eq, 'eqM)
    -- , ('RegFile.initRF, 'emptyR)
    -- , ('RegFile.modifyRF, undefined)
    -- , ('RegFile.modifyRF, undefined)
    -- , ('RegFile.eqRF, 'eqR)
    ]
  }

-- type StepperRegFile = Stepper.RegFile Identity

newtype Memory k v where
  Memory :: SymFC.Array (SymFC.BitVec k) (SymFC.BitVec v) -> Memory k v

emptyM
  :: Embeddable memory Memory
  => 1 <= k
  => 1 <= v
  => SymFC.KnownNat k
  => SymFC.KnownNat v
  => memory k v
emptyM @memory | Embedding <- embedding @memory = coerce $ SymFC.aconst 0

readM
  :: Embeddable memory (Memory 32 8)
  => Embeddable address (Clash.BitVec 32)
  => Embeddable byte (Clash.BitVec 8)
  => address
  -> memory
  -> byte
readM @memory @address @byte
  | Embedding <- embedding @memory
  , Embedding <- embedding @address
  , Embedding <- embedding @byte
  = coerce \(Clash.BitVecP addr) memory -> do
    Clash.BitVecP $ SymFC.aselect memory addr

writeM
  :: Embeddable memory (Memory 32 8)
  => Embeddable address (Clash.BitVec 32)
  => Embeddable byte (Clash.BitVec 8)
  => address
  -> byte
  -> memory
  -> memory
writeM @memory @address @byte
  | Embedding <- embedding @memory
  , Embedding <- embedding @address
  , Embedding <- embedding @byte
  = coerce \(Clash.BitVecP addr) (Clash.BitVecP byte) memory -> do
    SymFC.astore memory addr byte

eqM
  :: Embeddable memory (Memory 32 8)
  => memory
  -> memory
  -> Bool
eqM @memory
  | Embedding <- embedding @memory
  = coerce \lhs rhs -> SymFC.ite (SymFC.aeq lhs rhs) True False

type RegFile = SymFC.Array (SymFC.BitVec 5) (SymFC.BitVec 32)

emptyR :: Embeddable regfile RegFile => regfile
emptyR @regfile | Embedding <- embedding @regfile = coerce $ SymFC.aconst 0

readR
  :: Embeddable regfile RegFile
  => Embeddable address (Clash.BitVec 5)
  => Embeddable byte (Clash.BitVec 32)
  => address
  -> regfile
  -> byte
readR @regfile @address @byte
  | Embedding <- embedding @regfile
  , Embedding <- embedding @address
  , Embedding <- embedding @byte
  = coerce \(Clash.BitVecP addr) regfile -> do
    Clash.BitVecP $ SymFC.aselect regfile addr

writeR
  :: Embeddable regfile RegFile
  => Embeddable address (Clash.BitVec 5)
  => Embeddable byte (Clash.BitVec 32)
  => address
  -> byte
  -> regfile
  -> regfile
writeR @regfile @address @byte
  | Embedding <- embedding @regfile
  , Embedding <- embedding @address
  , Embedding <- embedding @byte
  = coerce \(Clash.BitVecP addr) (Clash.BitVecP byte) regfile -> do
    SymFC.astore regfile addr byte

eqR
  :: Embeddable regfile RegFile
  => regfile
  -> regfile
  -> Bool
eqR @regfile
  | Embedding <- embedding @regfile
  = coerce \lhs rhs -> SymFC.ite (SymFC.aeq lhs rhs) True False
