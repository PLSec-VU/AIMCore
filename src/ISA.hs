{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module ISA
  ( Func,
    Done (..),
    DepReg (..),
    apply,
    Instr (..),
    PC,
    depSet,
    getRd,
    getR1,
    getR2,
    isLoad,
    loadHazard,
    interp,
    interp',
  )
where

import Access
import Clash.Prelude hiding (Const, Log, Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Instruction (Sign)
import qualified Instruction
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type PC = Address

data Func a = Func
  { isaFunc :: Word -> Word -> PC -> a,
    isaDeps :: (Maybe RegIdx, Maybe RegIdx)
  }

instance Show (Func a) where
  show _ = "<Func>"

instance Functor Func where
  fmap g (Func f d) = Func (\r1 r2 pc -> g $ f r1 r2 pc) d

newtype Done a = Done {unDone :: a}
  deriving (Show, Eq)

apply :: Func a -> Word -> Word -> PC -> Done a
apply (Func f _) r1 r2 pc = Done $ f r1 r2 pc

data Instr f
  = Reg RegIdx (f Word)
  | Load Size Sign RegIdx (f Address)
  | Jump RegIdx (f Address) (f Address)
  | Store Size (f Address) RegIdx
  | Branch (f Bool) (f Address)
  | Nop
  | Break

deriving instance
  ( Show (f Word),
    Show (f Address),
    Show (f Bool)
  ) =>
  Show (Instr f)

deriving instance
  ( Eq (f Word),
    Eq (f Address),
    Eq (f Bool)
  ) =>
  Eq (Instr f)

getRd :: Instr a -> Maybe RegIdx
getRd (Reg rd _) = pure rd
getRd (Load _ _ rd _) = pure rd
getRd (Jump rd _ _) = pure rd
getRd _ = empty

getR1 :: Instr Func -> Maybe RegIdx
getR1 = fst . deps

getR2 :: Instr Func -> Maybe RegIdx
getR2 = snd . deps

isLoad :: Instr a -> Bool
isLoad Load {} = True
isLoad _ = False

loadHazard :: Instr Func -> Instr Func -> Bool
loadHazard de_ir (ISA.Load _ _ rd _) =
  elem rd $ S.toList $ depSet de_ir
loadHazard _ _ = False

class DepReg a where
  deps :: a -> (Maybe RegIdx, Maybe RegIdx)

instance DepReg (Func a) where
  deps (Func _ d) = d

instance DepReg (Instr Func) where
  deps (Reg _ f) = deps f
  deps (Load _ _ _ f) = deps f
  deps (Jump _ _ f) = deps f
  deps (Store _ f r2) = (fst $ deps f, pure r2)
  deps (Branch f _) = deps f
  deps Break = (empty, empty)
  deps Nop = (empty, empty)

depSet :: (DepReg a) => a -> Set RegIdx
depSet a =
  let (mr1, mr2) = deps a
   in S.fromList $ catMaybes [mr1, mr2]

interp :: (Access f) => Input f -> Instr Func
interp input
  | not (inputIsInstr input) = Nop
  | otherwise = interp' $ Instruction.decode' $ unAccess $ inputMem input

interp' :: Instruction.Instruction -> Instr Func
interp' instr
  | Instruction.isBreak instr = Break
  | otherwise =
      case instr of
        Instruction.RType op rd r1 r2 ->
          Reg rd $ binaryF r1 r2 $ \w1 w2 -> unAccess $ alu op (Identity w1) (Identity w2)
        Instruction.IType iop rd r1 imm ->
          let op =
                case iop of
                  Instruction.Arith op' -> op'
                  _ -> Instruction.ADD
              alu_res = unaryF r1 $ \w -> unAccess $ alu op (Identity w) (Identity $ signExtend imm)
           in case iop of
                Instruction.Arith {} ->
                  Reg rd alu_res
                Instruction.Load size sign ->
                  Load size sign rd $ bitCoerce <$> alu_res
                -- Instruction.Load size sign -> do
                --  let loadExtend =
                --        case (size, sign) of
                --          (Byte, Instruction.Signed) -> signExtend . slice d7 d0
                --          (Byte, Instruction.Unsigned) -> zeroExtend . slice d7 d0
                --          (Half, Instruction.Signed) -> signExtend . slice d15 d0
                --          (Half, Instruction.Unsigned) -> signExtend . slice d15 d0
                --          (Word, _) -> signExtend . slice d31 d0
                --  Load size rd $ bitCoerce . loadExtend <$> alu_res
                Instruction.Jump ->
                  Jump rd (pcF (bitCoerce . (+ 4))) $ bitCoerce <$> alu_res
                Instruction.Env Instruction.Break ->
                  Break
                Instruction.Env Instruction.Call ->
                  Nop
        Instruction.SType size imm r1 r2 -> do
          let addr_comp = unpack <$> unaryF r1 (+ signExtend imm)
           in Store size addr_comp r2
        Instruction.BType cmp imm r1 r2 ->
          let branched_comp = binaryF r1 r2 $ \w1 w2 -> unAccess $ branch cmp (Identity w1) (Identity w2)
              addr_comp = pcF (+ bitCoerce (signExtend imm))
           in Branch branched_comp addr_comp
        Instruction.UType Instruction.Zero rd imm ->
          let imm' = imm ++# 0 `shiftL` 12
           in Reg rd $ constF imm'
        Instruction.UType Instruction.PC rd imm -> do
          let imm' = imm ++# 0 `shiftL` 12
           in Reg rd $ pcF $ \pc -> bitCoerce pc + imm'
        Instruction.JType rd imm ->
          Jump rd (pcF (bitCoerce . (+ 4))) $ pcF (+ bitCoerce (signExtend imm))
        Instruction.Nop _ ->
          interp' Instruction.nop
  where
    constF :: a -> Func a
    constF a =
      Func
        { isaFunc = const $ const $ const a,
          isaDeps = (empty, empty)
        }

    unaryF :: RegIdx -> (Word -> a) -> Func a
    unaryF rid f =
      Func
        { isaFunc = \r _ _ -> f r,
          isaDeps = (pure rid, empty)
        }

    binaryF :: RegIdx -> RegIdx -> (Word -> Word -> a) -> Func a
    binaryF rid1 rid2 f =
      Func
        { isaFunc = \r1 r2 _ -> f r1 r2,
          isaDeps = (pure rid1, pure rid2)
        }

    pcF :: (PC -> a) -> Func a
    pcF f =
      Func
        { isaFunc = const $ const f,
          isaDeps = (empty, empty)
        }
