module Interp where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Instruction
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data Interp f = Interp
  { interpRes :: f Word,
    interpAddr :: f (Maybe Address),
    interpBranched :: Maybe (f Bool)
  }

interp :: (Access f) => Instruction -> f Word -> f Word -> Address -> Interp f
interp instr r1 r2 pc =
  case instr of
    RType op rd _ _ ->
      Interp (alu op r1 r2) (pure Nothing) Nothing
    IType iop rd _ imm ->
      let op =
            case iop of
              Arith op' -> op'
              _ -> ADD
          alu_res = alu op r1 (pure $ signExtend imm)
       in case iop of
            Arith {} -> Interp alu_res (pure Nothing) Nothing
            Load size sign ->
              let addr = fmap (unpack :: Word -> Address) alu_res
               in Interp (fmap bitCoerce addr) (fmap Just addr) Nothing
            Jump ->
              Interp (pure $ pack $ pc + 4) (fmap (Just . unpack) alu_res) Nothing
            Env _ ->
              Interp (pure 0) (pure Nothing) Nothing
    SType size imm _ _ ->
      let addr = fmap (unpack :: Word -> Address) (alu ADD r1 (pure $ signExtend imm))
       in Interp (fmap bitCoerce addr) (fmap Just addr) Nothing
    BType cmp imm _ _ ->
      let branched = branch cmp r1 r2
          jumpAddr = (\b -> if b then Just (pc + unpack (signExtend imm)) else Nothing) <$> branched
          branched' = case fromPublic branched of
            Just b -> Just (pure b)
            Nothing -> Nothing
       in Interp (pure 0) jumpAddr branched'
    UType Zero rd imm ->
      Interp (pure $ imm ++# (0 :: BitVector 12)) (pure Nothing) Nothing
    UType PC rd imm ->
      let imm' = imm ++# (0 :: BitVector 12)
       in Interp (pure $ pack pc + imm') (pure Nothing) Nothing
    JType rd imm ->
      Interp (pure $ pack $ pc + 4) (pure $ Just $ pc + unpack (signExtend imm)) Nothing
    Nop _ ->
      Interp (pure 0) (pure Nothing) Nothing

