module Interp where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Instruction
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data Interp = Interp
  { interpRes :: Word,
    interpAddr :: Maybe Address,
    interpBranched :: Maybe Bool
  }

interp :: (Access f) => Instruction -> f Word -> f Word -> Address -> Interp
interp instr r1 r2 pc =
  case instr of
    RType op rd _ _ ->
      Interp (unAccess $ alu False op r1 r2) Nothing Nothing
    IType iop rd _ imm ->
      let op =
            case iop of
              Arith op' -> op'
              _ -> ADD
          alu_res = alu True op r1 (pure $ signExtend imm)
       in case iop of
            Arith {} -> Interp (unAccess alu_res) Nothing Nothing
            Load size sign -> Interp (unpack $ unAccess alu_res) Nothing Nothing
            Jump ->
              Interp (pack $ pc + 4) (Just $ unpack $ unAccess alu_res) Nothing
            Env Break ->
              Interp (unAccess alu_res) Nothing Nothing
            Env Call ->
              Interp 0 Nothing Nothing
    SType size imm _ _ ->
      Interp (unpack (unAccess r1 + signExtend imm)) Nothing Nothing
    BType cmp imm _ _ ->
      let branched = unAccess $ branch cmp r1 r2
       in Interp 0 (if branched then Just $ pc + unpack (signExtend imm) else Nothing) (Just branched)
    UType Zero rd imm ->
      Interp (imm ++# (0 :: BitVector 12)) Nothing Nothing
    UType PC rd imm ->
      let imm' = imm ++# (0 :: BitVector 12)
       in Interp (pack pc + imm') Nothing Nothing
    JType rd imm ->
      Interp (pack $ pc + 4) (Just $ pc + unpack (signExtend imm)) Nothing
