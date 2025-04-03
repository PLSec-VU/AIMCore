module Types
  ( RegIdx,
    Imm,
    UImm,
    Word,
    Address,
    Byte,
    Size (..),
    vecWordToByte,
  )
where

import Clash.Prelude hiding (Word)
import GHC.TypeNats
import Prelude hiding (Word, concatMap)

-- | Index into register file.
type RegIdx = Unsigned 5

-- | Immediate value.
type Imm = BitVector 12

-- | Upper immediate value.
type UImm = BitVector 20

-- | Word size of this core.
type Word = BitVector 32

-- | Byte size
type Byte = BitVector 8

-- | Memory addresses used in this core.
type Address = Unsigned 32

-- | Word sizes.
data Size
  = Byte
  | Half
  | Word
  deriving (Eq, Show, Generic, NFDataX, Enum, Bounded)

vecWordToByte ::
  Vec n Word ->
  Vec ((GHC.TypeNats.*) n 4) Byte
vecWordToByte =
  concatMap splitWord
  where
    splitWord :: Word -> Vec 4 Byte
    splitWord word =
      let b0 = slice d7 d0 word
          b1 = slice d15 d8 word
          b2 = slice d23 d16 word
          b3 = slice d31 d24 word
       in b0 :> b1 :> b2 :> b3 :> Nil
