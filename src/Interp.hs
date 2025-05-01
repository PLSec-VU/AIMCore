module Interp where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Core
import Instruction
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data Interp = Interp
  { interpRes :: Word,
    interpAddr :: Maybe Address,
    interpBranched :: Maybe Bool
  }

interp :: Instruction -> Word -> Word -> Address -> Interp
interp instr r1 r2 pc =
  case instr of
    RType op rd _ _ ->
      Interp (alu False op r1 r2) Nothing Nothing
    IType iop rd _ imm ->
      let op =
            case iop of
              Arith op' -> op'
              _ -> ADD
          alu_res = alu True op r1 (signExtend imm)
       in case iop of
            Arith {} -> Interp alu_res Nothing Nothing
            Load size sign -> Interp (unpack alu_res) Nothing Nothing
            Jump ->
              Interp (pack $ pc + 4) (Just $ unpack alu_res) Nothing
            Env Break ->
              Interp alu_res Nothing Nothing
            Env Call ->
              Interp alu_res Nothing Nothing
    SType size imm _ _ ->
      Interp (unpack (r1 + signExtend imm)) Nothing Nothing
    BType cmp imm _ _ ->
      let branched = branch cmp r1 r2
       in Interp 0 (if branched then Just $ pc + unpack (signExtend imm) else Nothing) (Just branched)
    UType Zero rd imm ->
      Interp (imm ++# 0 `shiftL` 12) Nothing Nothing
    UType PC rd imm ->
      let imm' = imm ++# 0 `shiftL` 12
       in Interp (pack pc + imm') Nothing Nothing
    JType rd imm ->
      Interp (pack $ pc + 4) (Just $ pc + unpack (signExtend imm)) Nothing
