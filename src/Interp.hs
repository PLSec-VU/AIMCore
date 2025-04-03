{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Interp
  ( InterpF,
    DepReg (..),
    applyInterpF,
    Inst (..),
    Done (..),
    PC,
    depSet,
    getRd,
    getR1,
    getR2,
    interp,
  )
where

import Clash.Prelude hiding (Const, Log, Ordering (..), Word, def, init, lift, log)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Instruction as Inst
import Pipe
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type PC = Address

data InterpF a = InterpF
  { isaFunc :: Word -> Word -> PC -> a,
    isaDeps :: (Maybe RegIdx, Maybe RegIdx)
  }

instance Show (InterpF a) where
  show _ = "<InterpF>"

instance Functor InterpF where
  fmap g (InterpF f d) = InterpF (\r1 r2 pc -> g $ f r1 r2 pc) d

newtype Done a = Done {unDone :: a}
  deriving (Show)

constF :: a -> InterpF a
constF a =
  InterpF
    { isaFunc = const $ const $ const a,
      isaDeps = (empty, empty)
    }

unaryF :: RegIdx -> (Word -> a) -> InterpF a
unaryF rid f =
  InterpF
    { isaFunc = \r _ _ -> f r,
      isaDeps = (pure rid, empty)
    }

binaryF :: RegIdx -> RegIdx -> (Word -> Word -> a) -> InterpF a
binaryF rid1 rid2 f =
  InterpF
    { isaFunc = \r1 r2 _ -> f r1 r2,
      isaDeps = (pure rid1, pure rid2)
    }

pcF :: (PC -> a) -> InterpF a
pcF f =
  InterpF
    { isaFunc = const $ const f,
      isaDeps = (empty, empty)
    }

applyInterpF :: InterpF a -> Word -> Word -> PC -> Done a
applyInterpF (InterpF f _) r1 r2 pc = Done $ f r1 r2 pc

data Inst f
  = Reg RegIdx (f Word)
  | Load Size RegIdx (f Address)
  | Jump RegIdx (f Address) (f Address)
  | JumpReg RegIdx (f Address) (f Address)
  | Store Size (f Address) RegIdx
  | Branch (f Bool) (f Address)
  | Nop
  | Break

deriving instance
  ( Show (f Word),
    Show (f Address),
    Show (f Bool)
  ) =>
  Show (Inst f)

class DepReg a where
  deps :: a -> (Maybe RegIdx, Maybe RegIdx)

instance DepReg (InterpF a) where
  deps (InterpF _ d) = d

instance DepReg (Inst InterpF) where
  deps (Reg _ f) = deps f
  deps (Load _ _ f) = deps f
  deps Jump {} = (empty, empty)
  deps (JumpReg _ _ f) = deps f
  deps (Store _ f r2) = (fst $ deps f, pure r2)
  deps (Branch f _) = deps f
  deps Break = (empty, empty)
  deps Nop = (empty, empty)

depSet :: (DepReg a) => a -> Set RegIdx
depSet a =
  let (mr1, mr2) = deps a
   in S.fromList $ catMaybes [mr1, mr2]

getRd :: Inst f -> Maybe RegIdx
getRd (Reg rd _) = pure rd
getRd (Load _ rd _) = pure rd
getRd (Jump rd _ _) = pure rd
getRd (JumpReg rd _ _) = pure rd
getRd _ = empty

getR1 :: Inst InterpF -> Maybe RegIdx
getR1 = fst . deps

getR2 :: Inst InterpF -> Maybe RegIdx
getR2 = snd . deps

interp :: Input -> Inst InterpF
interp input
  | not (inputIsInst input) = Nop
  | Inst.isBreak inst = Break
  | otherwise =
      case inst of
        Inst.RType op rd r1 r2 ->
          Reg rd $ binaryF r1 r2 $ alu op
        Inst.IType iop rd r1 imm ->
          let op =
                case iop of
                  Inst.Arith op' -> op'
                  _ -> Inst.ADD
              alu_res = unaryF r1 $ flip (alu op) (signExtend imm)
           in case iop of
                Inst.Arith {} ->
                  Reg rd alu_res
                Inst.Load size sign -> do
                  let loadExtend =
                        case (size, sign) of
                          (Byte, Inst.Signed) -> signExtend . slice d7 d0
                          (Byte, Inst.Unsigned) -> zeroExtend . slice d7 d0
                          (Half, Inst.Signed) -> signExtend . slice d15 d0
                          (Half, Inst.Unsigned) -> signExtend . slice d15 d0
                          (Word, _) -> signExtend . slice d31 d0
                  Load size rd $ bitCoerce . loadExtend <$> alu_res
                Inst.Jump ->
                  JumpReg rd (pcF (bitCoerce . (+ 4))) $ bitCoerce <$> alu_res
                Inst.Env Inst.Break ->
                  Break
                Inst.Env Inst.Call ->
                  Nop
        Inst.SType size imm r1 r2 -> do
          let addr_comp = unpack <$> unaryF r1 (+ signExtend imm)
           in Store size addr_comp r2
        Inst.BType cmp imm r1 r2 ->
          let branched_comp = binaryF r1 r2 $ branch cmp
              addr_comp = pcF (+ bitCoerce (signExtend imm))
           in Branch branched_comp addr_comp
        Inst.UType Inst.Zero rd imm ->
          let imm' = imm ++# 0 `shiftL` 12
           in Reg rd $ constF imm'
        Inst.UType Inst.PC rd imm -> do
          let imm' = imm ++# 0 `shiftL` 12
           in Reg rd $ pcF $ \pc -> bitCoerce pc + imm'
        Inst.JType rd imm ->
          Jump rd (pcF (bitCoerce . (+ 4))) $ pcF (+ bitCoerce (signExtend imm))
  where
    inst = Inst.decode' $ inputMem input
