module Instruction
  ( Instruction (..)
  , decode
  , encode
  , nop

  , IOperation (..)
  , Arith (..)
  , Env (..)
  , Comparison (..)
  , Size (..)
  , Sign (..)
  , UBase (..)
  ) where

import Clash.Prelude hiding (Word, Ordering (..))
import Prelude hiding (Word, Ordering (..), undefined)
import Data.Maybe (fromMaybe)
import Types

-- | All arithmetic and logic operations.
data Arith
  = ADD
  -- ^ Addition
  | SUB
  -- ^ Subtraction
  | XOR
  -- ^ Bitwise Exclusive Or
  | OR
  -- ^ Bitwise Or
  | AND
  -- ^ Bitwise And
  | SLL
  -- ^ Shift Left Logical
  | SRL
  -- ^ Shift Right Logical
  | SRA
  -- ^ Shift Right Arithmetic
  | SLT
  -- ^ Set Less Than
  | SLTU
  -- ^ Set Less Than Unsigned
  deriving Show

-- | Comparison operations.
data Comparison
  = EQ
  -- ^ Equals
  | NE
  -- ^ Not Equals
  | LT
  -- ^ Less Than
  | GE
  -- ^ Greater or Equals than
  | LTU
  -- ^ Less Than Unsigned
  | GEU
  -- ^ Greater or Equals than Unsigned
  deriving Show

-- | Word sizes that can be loaded or stored.
data Size
  = Byte
  | Half
  | Word
  deriving Show

-- | Sign extension of a load operation.
data Sign
  = Signed
  | Unsigned
  deriving Show

-- | The base for an upper type operation: lui or auipc.
data UBase
  = PC
  | Zero
  deriving Show

-- | Environment operation: ecall or ebreak.
data Env
  = Call
  | Break
  deriving Show

-- | Types of operations using the immediate encoding.
data IOperation
  = Arith Arith
  | Load Size Sign
  | Env Env
  | Jump
  deriving Show

-- | Decoded instructions
data Instruction
  = RType Arith RegIdx RegIdx RegIdx
  -- ^ RType op rd rs1 rs2

  | IType IOperation RegIdx RegIdx Imm
  -- ^ IType op rd rs1 imm

  | SType Size Imm RegIdx RegIdx
  -- ^ SType size imm rs1 rs2

  | BType Comparison Imm RegIdx RegIdx
  -- ^ BType cmp imm rs1 rs2

  | UType UBase RegIdx UImm
  -- ^ UType base rd imm

  | JType RegIdx UImm
  -- ^ JType rd imm

  | Invalid
  deriving Show

-- | Decode a word to an instruction.
--
-- https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf
decode :: Word -> Instruction
decode word = fromMaybe Invalid $ do
  let opcode = slice d6 d0 word

  let rd = unpack $ slice d11 d7 word
  let rs1 = unpack $ slice d19 d15 word
  let rs2 = unpack $ slice d24 d20 word

  let funct3 = slice d14 d12 word
  let funct7 = slice d31 d25 word

  let immI = slice d31 d20 word
  let immS = slice d31 d25 word
         ++# slice d11 d7 word
  let immB = slice d31 d31 word
         ++# slice d7 d7 word
         ++# slice d30 d25 word
         ++# slice d11 d8 word
  let immU = slice d31 d12 word
  let immJ = slice d31 d31 word
         ++# slice d19 d12 word
         ++# slice d20 d20 word
         ++# slice d30 d21 word

  case opcode of
    -- R-Type
    0b011_0011 -> do
      arith <- case (funct3, funct7) of
        (0x0, 0x00) -> pure ADD
        (0x0, 0x20) -> pure SUB
        (0x4, 0x00) -> pure XOR
        (0x6, 0x00) -> pure OR
        (0x7, 0x00) -> pure AND
        (0x1, 0x00) -> pure SLL
        (0x5, 0x00) -> pure SRL
        (0x5, 0x20) -> pure SRA
        (0x2, 0x00) -> pure SLT
        (0x3, 0x00) -> pure SLTU
        _ -> empty
      pure $ RType arith rd rs1 rs2
      
    -- I-Type arithmetic
    0b001_0011 -> do
      arith <- case funct3 of
        0x0 -> pure ADD
        0x4 -> pure XOR
        0x6 -> pure OR
        0x7 -> pure AND
        0x1 | funct7 == 0x00 -> pure SLL
        0x5 | funct7 == 0x00 -> pure SRL
        0x5 | funct7 == 0x20 -> pure SRA
        0x2 -> pure SLT
        0x3 -> pure SLTU
        _ -> empty

      pure $ IType (Arith arith) rd rs1 immI

    -- I-Type load
    0b000_0011 -> do
      (size, sign) <- case funct3 of
        0x0 -> pure (Byte, Signed)
        0x1 -> pure (Half, Signed)
        0x2 -> pure (Word, Signed)
        0x4 -> pure (Byte, Unsigned)
        0x5 -> pure (Half, Unsigned)
        _ -> empty

      pure $ IType (Load size sign) rd rs1 immI

    -- I-Type jump
    0b110_0111 | funct3 == 0x0 -> pure $ IType Jump rd rs1 immI

    -- I-Type environment
    0b111_0011 | funct3 == 0x0 -> do
      env <- case immI of
        0 -> pure Call
        1 -> pure Break
        _ -> empty

      pure $ IType (Env env) rd rs1 immI

    -- S-Type
    0b010_0011 -> do
      size <- case funct3 of
        0x0 -> pure Byte
        0x1 -> pure Half
        0x2 -> pure Word
        _ -> empty

      pure $ SType size immS rs1 rs2

    -- B-Type
    0b110_0011 -> do
      cmp <- case funct3 of
        0x0 -> pure EQ
        0x1 -> pure NE
        0x4 -> pure LT
        0x5 -> pure GE
        0x6 -> pure LTU
        0x7 -> pure GEU
        _ -> empty

      pure $ BType cmp immB rs1 rs2

    -- U-Type load upper immediate
    0b011_0111 -> pure $ UType Zero rd immU

    -- U-Type add upper immediate to PC
    0b001_0111 -> pure $ UType PC rd immU

    -- J-Type
    0b110_1111 -> pure $ JType rd immJ

    _ -> empty

-- | Decode a word to an instruction.
--
-- https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf
encode :: Instruction -> Word
encode instruction = do
  case instruction of
    RType op rd rs1 rs2 -> do
      let opcode = 0b011_0011 :: BitVector 7
      let (funct3, funct7) :: (BitVector 3, BitVector 7) = case op of
            ADD  -> (0x0, 0x00)
            SUB  -> (0x0, 0x20)
            XOR  -> (0x4, 0x00)
            OR   -> (0x6, 0x00)
            AND  -> (0x7, 0x00)
            SLL  -> (0x1, 0x00)
            SRL  -> (0x5, 0x00)
            SRA  -> (0x5, 0x20)
            SLT  -> (0x2, 0x00)
            SLTU -> (0x3, 0x00)

      let rd' = pack rd
      let rs1' = pack rs1
      let rs2' = pack rs2
      funct7 ++# rs2' ++# rs1' ++# funct3 ++# rd' ++# opcode

    IType (Arith arith) rd rs1 imm -> do
      let opcode = 0b001_0011 :: BitVector 7
      let funct3 = case arith of
            ADD  -> 0x0
            XOR  -> 0x4
            OR   -> 0x6
            AND  -> 0x7
            SLL | slice d11 d5 imm == 0 -> 0x1
            SRL | slice d11 d5 imm == 0 -> 0x5
            SRA | slice d11 d5 imm == 0 -> 0x5
            SLT  -> 0x2
            SLTU -> 0x3
            _  -> error "Incorrect instruction"

      let rd' = pack rd
      let rs1' = pack rs1
      imm ++# rs1' ++# funct3 ++# rd' ++# opcode

    IType (Load size sign) rd rs1 imm -> do
      let opcode = 0b000_0011 :: BitVector 7
      let funct3 = case (size, sign) of
            (Byte, Signed) -> 0x0
            (Half, Signed) -> 0x1
            (Word, Signed) -> 0x2
            (Byte, Unsigned) -> 0x4
            (Half, Unsigned) -> 0x5
            _  -> error "Incorrect instruction"

      let rd' = pack rd
      let rs1' = pack rs1
      imm ++# rs1' ++# funct3 ++# rd' ++# opcode

    IType Jump rd rs1 imm -> do
      let opcode = 0b110_0111 :: BitVector 7
      let funct3 = 0x0
      let rd' = pack rd
      let rs1' = pack rs1
      imm ++# rs1' ++# funct3 ++# rd' ++# opcode

    IType (Env env) rd rs1 _ -> do
      let opcode = 0b111_0011 :: BitVector 7
      let funct3 = 0x0 :: BitVector 3
      let imm :: BitVector 12 = case env of
            Call -> 0
            Break -> 1
      let rd' = pack rd
      let rs1' = pack rs1
      imm ++# rs1' ++# funct3 ++# rd' ++# opcode

    SType size imm rs1 rs2 -> do
      let opcode = 0b010_0011 :: BitVector 7
      let funct3 :: BitVector 3 = case size of
            Byte -> 0x0
            Half -> 0x1
            Word -> 0x2

      let immU = slice d11 d5 imm
      let immL = slice d4 d0 imm

      let rs1' = pack rs1
      let rs2' = pack rs2
      immU ++# rs2' ++# rs1' ++# funct3 ++# immL ++# opcode

    BType cmp imm rs1 rs2 -> do
      let opcode = 0b010_0011 :: BitVector 7

      let funct3 :: BitVector 3 = case cmp of
            EQ  -> 0x0
            NE  -> 0x1
            LT  -> 0x4
            GE  -> 0x5
            LTU -> 0x6
            GEU -> 0x7

      -- Note: The first bit of the imm is always 0, which is not stored
      -- directly. This is why the slices are all indexed at (-1) offset.
      let imm4to1 = slice d3 d0 imm
      let imm10to5 = slice d9 d4 imm
      let imm11 = slice d10 d10 imm
      let imm12 = slice d11 d11 imm

      let rs1' = pack rs1
      let rs2' = pack rs2
      imm12 ++# imm10to5 ++# rs2' ++# rs1' ++# funct3 ++# imm4to1 ++# imm11 ++# opcode

    UType base rd imm -> do
      let opcode :: BitVector 7 = case base of
            Zero -> 0b011_0111
            PC -> 0b001_0111
      let rd' = pack rd
      imm ++# rd' ++# opcode

    JType rd imm -> do
      let opcode :: BitVector 7 = 0b110_1111

      -- Note: The first bit of the imm is always 0, which is not stored
      -- directly. This is why the slices are all indexed at (-1) offset.
      let imm10to1 = slice d9 d0 imm
      let imm11 = slice d10 d10 imm
      let imm19to12 = slice d18 d11 imm
      let imm20 = slice d19 d19 imm

      let rd' = pack rd
      imm20 ++# imm19to12 ++# imm11 ++# imm10to1 ++# rd' ++# opcode

    Invalid -> 0

nop :: Instruction
nop = RType ADD 0 0 0
