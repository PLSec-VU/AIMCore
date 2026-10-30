{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | The ALU shifts as SMT bitvector shifts.
--
-- In @src@ rather than with the other embeddings in "Proof.SMT.Axioms" because
-- the stepper needs them too: the hardware proofs and the stepper then send the
-- same shift terms to the solver.
module Stepper.Shift
  ( shiftEmbeddings,
    sllWordE,
    srlWordE,
    sraWordE,
  )
where

import Clash.Prelude (Nat, Type)
import Core (sllWord, sraWord, srlWord)
import Data.Coerce (coerce)
import SymFC (Embeddings (..))
import qualified SymFC.BuiltIn as P
import SymFC.BuiltIn (Embeddable, Embedding (..), embedding)
import qualified SymFC.Clash as Clash

-- | The ALU shifts, mapped to the SMT bitvector shifts.
--
-- See 'Core.sllWord' for why the wrappers exist: they keep the shift amount a
-- bitvector, so a query stays in the bitvector-and-array fragment that Bitwuzla
-- and Yices can read.
shiftEmbeddings :: Embeddings
shiftEmbeddings =
  Embeddings
    { typeEmbeddings = [],
      termEmbeddings =
        [ ('sllWord, 'sllWordE),
          ('srlWord, 'srlWordE),
          ('sraWord, 'sraWordE)
        ]
    }

-- Shifts as SMT bitvector shifts ----------------------------------------------
--
-- Embeddings for 'Core.sllWord', 'Core.srlWord' and 'Core.sraWord'. See the note
-- on those functions for why they exist: without them each shift site converts
-- its five-bit amount through 'Integer', and the resulting @ubv_to_int@ /
-- @int_to_bv@ pair makes every query unreadable to the bitvector-only solvers.
--
-- The amount is zero-extended to the word width because SMT's shifts require
-- both operands to have the same sort. That is exact rather than a choice: the
-- amount is five bits, so it is always less than 32 and no shift saturates.

-- | @shiftL@ on a 'Word' as @bvshl@.
sllWordE ::
  forall (bvX :: Nat -> Type) (bvN :: Nat -> Type).
  Embeddable bvX Clash.BitVec =>
  Embeddable bvN Clash.BitVec =>
  bvX 32 ->
  bvN 5 ->
  bvX 32
sllWordE
  | Embedding <- embedding @bvX
  , Embedding <- embedding @bvN = coerce go
  where
    go :: Clash.BitVec 32 -> Clash.BitVec 5 -> Clash.BitVec 32
    go (Clash.BitVecP x) (Clash.BitVecP n) =
      Clash.BitVecP (P.bvshl x (P.bvzext n))

-- | @shiftR@ on a 'Word' as @bvlshr@.
srlWordE ::
  forall (bvX :: Nat -> Type) (bvN :: Nat -> Type).
  Embeddable bvX Clash.BitVec =>
  Embeddable bvN Clash.BitVec =>
  bvX 32 ->
  bvN 5 ->
  bvX 32
srlWordE
  | Embedding <- embedding @bvX
  , Embedding <- embedding @bvN = coerce go
  where
    go :: Clash.BitVec 32 -> Clash.BitVec 5 -> Clash.BitVec 32
    go (Clash.BitVecP x) (Clash.BitVecP n) =
      Clash.BitVecP (P.bvlshr x (P.bvzext n))

-- | Arithmetic @shiftR@ on a 'Word' as @bvashr@.
sraWordE ::
  forall (bvX :: Nat -> Type) (bvN :: Nat -> Type).
  Embeddable bvX Clash.BitVec =>
  Embeddable bvN Clash.BitVec =>
  bvX 32 ->
  bvN 5 ->
  bvX 32
sraWordE
  | Embedding <- embedding @bvX
  , Embedding <- embedding @bvN = coerce go
  where
    go :: Clash.BitVec 32 -> Clash.BitVec 5 -> Clash.BitVec 32
    go (Clash.BitVecP x) (Clash.BitVecP n) =
      Clash.BitVecP (P.bvashr x (P.bvzext n))

