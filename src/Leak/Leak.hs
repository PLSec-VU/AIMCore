{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Leak.Leak where

import Clash.Prelude hiding (Const, Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace
import GHC.TypeNats
import Instruction hiding (decode, halt)
import qualified Instruction
import Pipe
import Regfile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type PC = Address

data ISAF a = ISAF
  { isaFunc :: Word -> Word -> PC -> a,
    isaDeps :: (Maybe RegIdx, Maybe RegIdx)
  }

instance Show (ISAF a) where
  show _ = "<ISAF>"

instance Functor ISAF where
  fmap g (ISAF f deps) = ISAF (\r1 r2 pc -> g $ f r1 r2 pc) deps

newtype Done a = Done {unDone :: a}
  deriving (Show)

constF :: a -> ISAF a
constF a =
  ISAF
    { isaFunc = const $ const $ const a,
      isaDeps = (empty, empty)
    }

unaryF :: RegIdx -> (Word -> a) -> ISAF a
unaryF rid f =
  ISAF
    { isaFunc = \r _ _ -> f r,
      isaDeps = (pure rid, empty)
    }

binaryF :: RegIdx -> RegIdx -> (Word -> Word -> a) -> ISAF a
binaryF rid1 rid2 f =
  ISAF
    { isaFunc = \r1 r2 _ -> f r1 r2,
      isaDeps = (pure rid1, pure rid2)
    }

pcF :: (PC -> a) -> ISAF a
pcF f =
  ISAF
    { isaFunc = const $ const f,
      isaDeps = (empty, empty)
    }

applyISAF :: ISAF a -> Word -> Word -> PC -> Done a
applyISAF (ISAF f _) r1 r2 pc = Done $ f r1 r2 pc

data LeakInst f
  = LReg RegIdx (f Word)
  | LLoad Size RegIdx (f Address)
  | LJump RegIdx (f Address) (f Address)
  | LJumpReg RegIdx (f Address) (f Address)
  | LStore Size (f Address) RegIdx
  | LBranch (f Bool) (f Address)
  | LNop
  | LHalt

deriving instance
  ( Show (f Word),
    Show (f Address),
    Show (f Bool)
  ) =>
  Show (LeakInst f)

getLeakRd :: LeakInst f -> Maybe RegIdx
getLeakRd (LReg rd _) = pure rd
getLeakRd (LLoad _ rd _) = pure rd
getLeakRd (LJump rd _ _) = pure rd
getLeakRd (LJumpReg rd _ _) = pure rd
getLeakRd _ = empty

getLeakR1 :: LeakInst ISAF -> Maybe RegIdx
getLeakR1 (LReg _ f) = fst $ deps f
getLeakR1 (LLoad _ _ f) = fst $ deps f
getLeakR1 (LJump {}) = empty
getLeakR1 (LJumpReg _ _ f) = fst $ deps f
getLeakR1 (LStore _ f _) = fst $ deps f
getLeakR1 (LBranch f _) = fst $ deps f
getLeakR1 LNop = empty
getLeakR1 LHalt = empty

getLeakR2 :: LeakInst ISAF -> Maybe RegIdx
getLeakR2 (LReg _ f) = snd $ deps f
getLeakR2 (LLoad _ _ f) = snd $ deps f
getLeakR2 (LJump {}) = empty
getLeakR2 (LJumpReg _ _ f) = snd $ deps f
getLeakR2 (LStore _ _ r2) = pure r2
getLeakR2 (LBranch f _) = snd $ deps f
getLeakR2 LNop = empty
getLeakR2 LHalt = empty

class DepReg a where
  deps :: a -> (Maybe RegIdx, Maybe RegIdx)

instance DepReg (ISAF a) where
  deps (ISAF _ d) = d

instance DepReg (LeakInst ISAF) where
  deps (LReg _ f) = deps f
  deps (LLoad _ _ f) = deps f
  deps (LJump _ _ _) = (empty, empty)
  deps (LJumpReg _ _ f) = deps f
  deps (LStore _ f r2) = (fst $ deps f, pure r2)
  deps (LBranch f _) = deps f
  deps LHalt = (empty, empty)
  deps LNop = (empty, empty)

depSet :: (DepReg a) => a -> Set RegIdx
depSet a =
  let (mr1, mr2) = deps a
   in S.fromList $ catMaybes [mr1, mr2]

leak :: Input -> LeakInst ISAF
leak input
  | not (inputIsInst input) = LNop
  | otherwise =
      case inst of
        RType op rd r1 r2 ->
          LReg rd $ binaryF r1 r2 $ alu op
        IType iop rd r1 imm ->
          let op =
                case iop of
                  Arith op' -> op'
                  _ -> ADD
              alu_res = unaryF r1 $ flip (alu op) (signExtend imm)
           in case iop of
                Arith {} ->
                  LReg rd alu_res
                Load size sign -> do
                  let loadExtend =
                        case (size, sign) of
                          (Byte, Signed) -> signExtend . slice d7 d0
                          (Byte, Unsigned) -> zeroExtend . slice d7 d0
                          (Half, Signed) -> signExtend . slice d15 d0
                          (Half, Unsigned) -> signExtend . slice d15 d0
                          (Word, _) -> signExtend . slice d31 d0
                  LLoad size rd $ bitCoerce . loadExtend <$> alu_res
                Jump ->
                  LJumpReg rd (pcF (bitCoerce . (+ 4))) $ bitCoerce <$> alu_res
                Env {} -> error ""
        SType size imm r1 r2 -> do
          let addr_comp = unpack <$> unaryF r1 (+ (signExtend imm))
           in LStore size addr_comp r2
        BType cmp imm r1 r2 ->
          let branched_comp = binaryF r1 r2 $ branch cmp
              addr_comp = pcF (+ bitCoerce (signExtend imm))
           in LBranch branched_comp addr_comp
        UType Zero rd imm ->
          let imm' = imm ++# 0 `shiftL` 12
           in LReg rd $ constF imm'
        UType PC rd imm -> do
          let imm' = imm ++# 0 `shiftL` 12
           in LReg rd $ pcF $ \pc -> bitCoerce pc + imm'
        JType rd imm ->
          LJump rd (pcF (bitCoerce . (+ 4))) $ pcF (+ bitCoerce (signExtend imm))
        EBREAK -> LHalt
        _ -> LNop
  where
    inst = Instruction.decode $ inputMem input
