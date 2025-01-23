module Types
  ( RegIdx
  , Imm
  , UImm
  , Word
  , Address
  ) where

import Prelude hiding (Word)
import Clash.Prelude (Unsigned, BitVector)

-- | Index into register file.
type RegIdx = Unsigned 5

-- | Immediate value.
type Imm = BitVector 12

-- | Upper immediate value.
type UImm = BitVector 20

-- | Word size of this core.
type Word = BitVector 32

-- | Memory addresses used in this core.
type Address = Unsigned 32

