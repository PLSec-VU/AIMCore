module Instruction
  ( Instruction (..),
    Reason4Stall,
    nop,
    decode,
    decode',
    encode,
    encode',
    IOperation (..),
    Arith (..),
    Env (..),
    Comparison (..),
    Sign (..),
    UBase (..),
    getRd,
    getRs1,
    getRs2,
    isBreak,
    isCall,
    break,
    loadHazard,
    isLoad,
    loadExtend,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, break)
import Control.Monad
import Data.Maybe (fromMaybe, isJust)
import Types
import Prelude hiding (Ordering (..), Word, break, undefined)

-- | All arithmetic and logic operations.
data Arith
  = -- | Addition
    ADD
  | -- | Subtraction
    SUB
  | -- | Bitwise Exclusive Or
    XOR
  | -- | Bitwise Or
    OR
  | -- | Bitwise And
    AND
  | -- | Shift Left Logical
    SLL
  | -- | Shift Right Logical
    SRL
  | -- | Shift Right Arithmetic
    SRA
  | -- | Set Less Than
    SLT
  | -- | Set Less Than Unsigned
    SLTU
  deriving (Eq, Show, Generic, NFDataX, Enum, Bounded)

-- | Comparison operations.
data Comparison
  = -- | Equals
    EQ
  | -- | Not Equals
    NE
  | -- | Less Than
    LT
  | -- | Greater or Equals than
    GE
  | -- | Less Than Unsigned
    LTU
  | -- | Greater or Equals than Unsigned
    GEU
  deriving (Eq, Show, Generic, NFDataX, Enum, Bounded)

-- | Sign extension of a load operation.
data Sign
  = Signed
  | Unsigned
  deriving (Eq, Show, Generic, NFDataX, Enum, Bounded)

-- | The base for an upper type operation: lui or auipc.
data UBase
  = PC
  | Zero
  deriving (Eq, Show, Generic, NFDataX, Enum, Bounded)

-- | Environment operation: ecall or ebreak.
data Env
  = Call
  | Break
  deriving (Eq, Show, Generic, NFDataX, Enum, Bounded)

-- | Types of operations using the immediate encoding.
data IOperation
  = Arith Arith
  | Load Size Sign
  | Env Env
  | Jump
  deriving (Eq, Show, Generic, NFDataX)

-- | Reason for replacing an instruction with a nop. Some of these can be collapsed if we want to optimize later.
data Reason4Stall
  = -- | We took a branch 1 cycle ago.
    BranchFirstCycle
  | -- | We took a branch 2 cycles ago.
    BranchSecondCycle
  | -- | A load hazard occurred 1 cycle ago.
    LoadHazardFirstCycle
  | -- | A load hazard occurred 2 cycles ago.
    LoadHazardSecondCycle
  | -- | A syscall occurred 1 cycle ago.
    SyscallFirstCycle
  | -- | No instruction read because of memory bus overload.
    MemoryBusBusy
  | -- | First cycle.
    FirstCycle
  | -- | Failed to decode an instruction.
    DecodeFail
  | -- | Security violation.
    SecurityViolation.
  deriving (Eq, Show, Generic, NFDataX)

-- | Decoded instructions
data Instruction
  = -- | RType op rd rs1 rs2
    RType Arith RegIdx RegIdx RegIdx
  | -- | IType op rd rs1 imm
    IType IOperation RegIdx RegIdx Imm
  | -- | SType size imm rs1 rs2
    SType Size Imm RegIdx RegIdx
  | -- | BType cmp imm rs1 rs2
    BType Comparison BImm RegIdx RegIdx
  | -- | UType base rd imm
    UType UBase RegIdx UImm
  | -- | JType rd imm
    JType RegIdx JImm
  | -- | nop
    Nop Reason4Stall
  deriving (Eq, Show, Generic, NFDataX)

nop :: Instruction
nop = RType ADD 0 0 0

{-# INLINE decode' #-}
decode' :: Word -> Instruction
decode' word = case decode word of
  Just instr -> instr
  Nothing -> Nop DecodeFail

-- | Decode a word to an instruction.
--
-- https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf
decode :: Word -> Maybe Instruction
decode word = do
  let opcode = slice d6 d0 word

  let rd = unpack $ slice d11 d7 word
  let rs1 = unpack $ slice d19 d15 word
  let rs2 = unpack $ slice d24 d20 word

  let funct3 = slice d14 d12 word
  let funct7 = slice d31 d25 word

  let immI = slice d31 d20 word
  let immS =
        slice d31 d25 word
          ++# slice d11 d7 word
  let immB =
        slice d31 d31 word
          ++# slice d7 d7 word
          ++# slice d30 d25 word
          ++# slice d11 d8 word
          ++# (0 :: BitVector 1)
  let immU = slice d31 d12 word
  let immJ =
        slice d31 d31 word
          ++# slice d19 d12 word
          ++# slice d20 d20 word
          ++# slice d30 d21 word
          ++# (0 :: BitVector 1)

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

-- | Encode an instruction to a word.
--
-- https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf
encode' :: Instruction -> Maybe Word
encode' instruction =
  case instruction of
    RType op rd rs1 rs2 -> do
      let opcode = 0b011_0011 :: BitVector 7
      let (funct3, funct7) :: (BitVector 3, BitVector 7) = case op of
            ADD -> (0x0, 0x00)
            SUB -> (0x0, 0x20)
            XOR -> (0x4, 0x00)
            OR -> (0x6, 0x00)
            AND -> (0x7, 0x00)
            SLL -> (0x1, 0x00)
            SRL -> (0x5, 0x00)
            SRA -> (0x5, 0x20)
            SLT -> (0x2, 0x00)
            SLTU -> (0x3, 0x00)
      let rd' = pack rd
      let rs1' = pack rs1
      let rs2' = pack rs2
      pure $ funct7 ++# rs2' ++# rs1' ++# funct3 ++# rd' ++# opcode
    IType (Arith arith) rd rs1 imm -> do
      let opcode = 0b001_0011 :: BitVector 7
      funct3 <- case arith of
        ADD -> pure 0x0
        XOR -> pure 0x4
        OR -> pure 0x6
        AND -> pure 0x7
        SLL | slice d11 d5 imm == 0 -> pure 0x1
        SRL | slice d11 d5 imm == 0 -> pure 0x5
        SRA | slice d11 d5 imm == 0 -> pure 0x5
        SLT -> pure 0x2
        SLTU -> pure 0x3
        _ -> Nothing -- error $ "Incorrect instruction: " <> show instruction
      let rd' = pack rd
      let rs1' = pack rs1
      pure $ imm ++# rs1' ++# funct3 ++# rd' ++# opcode
    IType (Load size sign) rd rs1 imm -> do
      let opcode = 0b000_0011 :: BitVector 7
      funct3 <- case (size, sign) of
        (Byte, Signed) -> pure 0x0
        (Half, Signed) -> pure 0x1
        (Word, Signed) -> pure 0x2
        (Byte, Unsigned) -> pure 0x4
        (Half, Unsigned) -> pure 0x5
        _ -> Nothing
      let rd' = pack rd
      let rs1' = pack rs1
      pure $ imm ++# rs1' ++# funct3 ++# rd' ++# opcode
    IType Jump rd rs1 imm -> do
      let opcode = 0b110_0111 :: BitVector 7
      let funct3 = 0x0
      let rd' = pack rd
      let rs1' = pack rs1
      pure $ imm ++# rs1' ++# funct3 ++# rd' ++# opcode
    IType (Env env) rd rs1 _ -> do
      let opcode = 0b111_0011 :: BitVector 7
      let funct3 = 0x0 :: BitVector 3
      let imm :: BitVector 12 = case env of
            Call -> 0
            Break -> 1
      let rd' = pack rd
      let rs1' = pack rs1
      pure $ imm ++# rs1' ++# funct3 ++# rd' ++# opcode
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
      pure $ immU ++# rs2' ++# rs1' ++# funct3 ++# immL ++# opcode
    BType cmp imm rs1 rs2 -> do
      let opcode = 0b110_0011 :: BitVector 7
      let funct3 :: BitVector 3 = case cmp of
            EQ -> 0x0
            NE -> 0x1
            LT -> 0x4
            GE -> 0x5
            LTU -> 0x6
            GEU -> 0x7

      -- Note: The first bit of the imm is always 0, so we right-shift by 1
      -- to remove it before storing in the instruction
      let immShifted = shiftR imm 1 -- Remove the least significant bit
      let imm4to1 = slice d3 d0 immShifted
      let imm10to5 = slice d9 d4 immShifted
      let imm11 = slice d10 d10 immShifted
      let imm12 = slice d11 d11 immShifted

      let rs1' = pack rs1
      let rs2' = pack rs2
      pure $ imm12 ++# imm10to5 ++# rs2' ++# rs1' ++# funct3 ++# imm4to1 ++# imm11 ++# opcode
    UType base rd imm -> do
      let opcode :: BitVector 7 = case base of
            Zero -> 0b011_0111
            PC -> 0b001_0111
      let rd' = pack rd
      pure $ imm ++# rd' ++# opcode
    JType rd imm -> do
      let opcode :: BitVector 7 = 0b110_1111

      -- Note: The first bit of the imm is always 0, so we right-shift by 1
      -- to remove it before storing in the instruction
      let immShifted = shiftR imm 1 -- Remove the least significant bit
      let imm10to1 = slice d9 d0 immShifted
      let imm11 = slice d10 d10 immShifted
      let imm19to12 = slice d18 d11 immShifted
      let imm20 = slice d19 d19 immShifted

      let rd' = pack rd
      pure $ imm20 ++# imm10to1 ++# imm11 ++# imm19to12 ++# rd' ++# opcode
    Nop _ ->
      encode' nop

encode :: Instruction -> Word
encode instr =
  fromMaybe (error $ "Incorrect instruction: " <> show instr) $ encode' instr

-- | Get the destination register of an instruction, if any.
getRd :: (Alternative f) => Instruction -> f RegIdx
getRd = \case
  Instruction.RType _ rd _ _ -> pure rd
  Instruction.IType (Env Call) _ _ _ -> pure 10 -- a0
  Instruction.IType _ rd _ _ -> pure rd
  Instruction.UType _ rd _ -> pure rd
  Instruction.JType rd _ -> pure rd
  Instruction.Nop _ -> pure 0
  _ -> empty

-- | Get the first source register of an instruction, if any.
getRs1 :: (Alternative f) => Instruction -> f RegIdx
getRs1 = \case
  Instruction.RType _ _ rs1 _ -> pure rs1
  Instruction.IType (Env Call) _ _ _ -> pure 17 -- a7
  Instruction.IType _ _ rs1 _ -> pure rs1
  Instruction.SType _ _ rs1 _ -> pure rs1
  Instruction.BType _ _ rs1 _ -> pure rs1
  Instruction.Nop _ -> pure 0
  _ -> empty

-- | Get the second source register of an instruction, if any.
getRs2 :: (Alternative f) => Instruction -> f RegIdx
getRs2 = \case
  Instruction.RType _ _ _ rs2 -> pure rs2
  Instruction.SType _ _ _ rs2 -> pure rs2
  Instruction.BType _ _ _ rs2 -> pure rs2
  Instruction.Nop _ -> pure 0
  _ -> empty

isBreak :: Instruction -> Bool
isBreak (IType (Env Break) _ _ _) = True
isBreak _ = False

isCall :: Instruction -> Bool
isCall (IType (Env Call) _ _ _) = True
isCall _ = False

isLoad :: Instruction -> Bool
isLoad (IType Load {} _ _ _) = True
isLoad _ = False

isNopBranchFirstCycle :: Instruction -> Bool
isNopBranchFirstCycle (Nop BranchFirstCycle) = True
isNopBranchFirstCycle _ = False

isNopLoadHazardFirstCycle :: Instruction -> Bool
isNopLoadHazardFirstCycle (Nop LoadHazardFirstCycle) = True
isNopLoadHazardFirstCycle _ = False

break :: Instruction
break = IType (Env Break) 0 0 0

loadHazard :: Instruction -> Instruction -> Bool
loadHazard de_ir ex_ir@(IType Load {} _ _ _) = isJust $ do
  let mr1 = noZero $ getRs1 de_ir
      mr2 = noZero $ getRs2 de_ir
      mrd = noZero $ getRd ex_ir
  (guard =<< (==) <$> mrd <*> mr1)
    <|> (guard =<< (==) <$> mrd <*> mr2)
  where
    noZero (Just 0) = Nothing
    noZero r = r
loadHazard _ _ = False

loadExtend :: Size -> Sign -> Word -> Word
loadExtend Byte Signed = signExtend . slice d7 d0
loadExtend Byte Unsigned = zeroExtend . slice d7 d0
loadExtend Half Signed = signExtend . slice d15 d0
loadExtend Half Unsigned = zeroExtend . slice d15 d0
loadExtend Word _ = signExtend . slice d31 d0
