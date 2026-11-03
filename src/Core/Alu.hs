-- | The arithmetic and comparison semantics of instructions: what the ALU
-- computes for an operation, and whether a branch's comparison holds. Shared
-- by the pipeline ("Core.Pipeline") and the ISA ("Core.ISA"), so that neither
-- depends on the other.
module Core.Alu
  ( alu,
    branch,
    sllWord,
    srlWord,
    sraWord,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Core.Instruction (Arith (..), Comparison (..))
import Types (Word)
import Prelude hiding (Ordering (..), Word, init, not, undefined, (&&), (||))

-- | What the ALU computes for an arithmetic operation on two words.
alu :: Arith -> Word -> Word -> Word
alu op lhs rhs = case op of
  ADD -> lhs + rhs
  SUB -> lhs - rhs
  XOR -> lhs .^. rhs
  OR -> lhs .|. rhs
  AND -> lhs .&. rhs
  SLL -> sllWord lhs (shiftBits rhs)
  SRL -> srlWord lhs (shiftBits rhs)
  SRA -> sraWord lhs (shiftBits rhs)
  SLT -> set (sign lhs < sign rhs)
  SLTU -> set (lhs < rhs)
  where
    shiftBits = slice d4 d0
    sign = unpack @(Signed 32)
    set b = if b then 1 else 0

-- | The three RISC-V shifts, with the shift amount kept as a bitvector.
--
-- RISC-V takes the amount from the low five bits of the second operand, so no
-- amount can reach the word width and these agree with the SMT shifts on the
-- nose.
--
-- They exist as named 'OPAQUE' functions, rather than 'shiftL' applied inline,
-- for the verifier's sake. 'Data.Bits.shiftL' takes an 'Int', so an inline
-- amount forces @fromIntegral@ on the five-bit slice, and that is modelled
-- through 'Integer': every shift site then emits an integer round trip
-- (@ubv_to_int@ to @int_to_bv@) wrapped in two overflow guards. Those few terms
-- pull integer arithmetic into a query that is otherwise pure bitvectors and
-- arrays, and Bitwuzla and Yices have no integer theory at all -- they reject
-- such a query outright rather than solve it slowly. Keeping the amount a
-- 'BitVector' behind a name lets "Proof.SMT.Axioms" map each shift to its SMT
-- counterpart; see 'Proof.SMT.Array.sllWordE'.
--
-- OPAQUE is essential, for the same reason as 'Proof.SMT.Array.loadRA': the
-- axiom is keyed on the name, so an inlined wrapper would leave nothing to
-- rewrite.
{-# OPAQUE sllWord #-}
sllWord :: Word -> BitVector 5 -> Word
sllWord x n = shiftL x (fromIntegral n)

-- | Logical right shift; 'BitVector' is unsigned, so 'shiftR' is @bvlshr@.
{-# OPAQUE srlWord #-}
srlWord :: Word -> BitVector 5 -> Word
srlWord x n = shiftR x (fromIntegral n)

-- | Arithmetic right shift, via 'Signed' as the ISA prescribes.
{-# OPAQUE sraWord #-}
sraWord :: Word -> BitVector 5 -> Word
sraWord x n = pack (shiftR (unpack x :: Signed 32) (fromIntegral n))

-- | Does a branch's comparison hold of two words?
branch :: Comparison -> Word -> Word -> Bool
branch op lhs rhs = case op of
  EQ -> lhs == rhs
  NE -> lhs /= rhs
  LT -> sign lhs < sign rhs
  GE -> sign lhs >= sign rhs
  LTU -> lhs < rhs
  GEU -> lhs >= rhs
  where
    sign = unpack @(Signed 32)
