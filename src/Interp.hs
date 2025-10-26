module Interp where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Core
import Instruction
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data Interp f = Interp
  { interpRes :: f Word,
    interpAddr :: f (Maybe Address),
    interpBranched :: f (Maybe Bool)
  }

interp :: (Access f) => Instruction -> (f Word) -> (f Word) -> Address -> Interp f
interp instr r1 r2 pc =
  case instr of
    RType op rd _ _ ->
      Interp (alu False op r1 r2) (pure Nothing) (pure Nothing)
    IType iop rd _ imm ->
      let op =
            case iop of
              Arith op' -> op'
              _ -> ADD
          alu_res = alu True op r1 (pure $ signExtend imm)
       in case iop of
            Arith {} -> Interp alu_res (pure Nothing) (pure Nothing)
            Load size sign -> Interp (unpack <$> alu_res) (pure Nothing) (pure Nothing)
            Jump ->
              Interp (pure $ pack $ pc + 4) (Just . unpack <$> alu_res) (pure Nothing)
            Env Break ->
              Interp alu_res (pure Nothing) (pure Nothing)
            Env Call ->
              Interp alu_res (pure Nothing) (pure Nothing)
    SType size imm _ _ ->
      Interp ((unpack . (+ signExtend imm)) <$> r1) (pure Nothing) (pure Nothing)
    BType cmp imm _ _ ->
      let branched = branch cmp r1 r2
       in Interp
            (pure 0)
            ( do
                branched' <- branched
                if branched'
                  then pure $ Just $ pc + unpack (signExtend imm)
                  else pure Nothing
            )
            (Just <$> branched)
    UType Zero rd imm ->
      Interp (pure $ imm ++# 0 `shiftL` 12) (pure Nothing) (pure Nothing)
    UType PC rd imm ->
      let imm' = imm ++# 0 `shiftL` 12
       in Interp (pure $ pack pc + imm') (pure Nothing) (pure Nothing)
    JType rd imm ->
      Interp (pure $ pack $ pc + 4) (pure $ Just $ pc + unpack (signExtend imm)) (pure Nothing)
