{-# LANGUAGE DeriveFunctor #-}
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

data WithReg a
  = Const a
  | Unary RegIdx (Word -> a)
  | Binary RegIdx RegIdx (Word -> Word -> a)

instance Functor WithReg where
  fmap f (Const a) = Const $ f a
  fmap f (Unary r g) = Unary r $ f . g
  fmap f (Binary ri1 ri2 g) = Binary ri1 ri2 $ \r1 r2 -> f $ g r1 r2

type PC = Address

newtype PCM a = PCM (Reader PC a)
  deriving (MonadReader PC, Monad, Functor, Applicative)

instance Show (PCM a) where
  show (PCM _) = "<pcm>"

applyPC :: PCM a -> PC -> a
applyPC (PCM m) = runReader m

instance Show (WithReg a) where
  show (Const a) = "Const"
  show (Unary r _) =
    unwords ["Unary", show r, "<fun>"]
  show (Binary r1 r2 _) =
    unwords ["Binary", show r1, show r2, "<fun>"]

newtype RegComp a = RegComp (WithReg a)
  deriving (Show, Functor)

newtype Done a = Done (PCM a)
  deriving (Show)

applyRegComp :: RegComp a -> PCM Word -> PCM Word -> Done a
applyRegComp (RegComp (Const a)) _ _ = Done $ pure a
applyRegComp (RegComp (Unary _ f)) r _ = Done $ f <$> r
applyRegComp (RegComp (Binary _ _ f)) r1 r2 = Done $ f <$> r1 <*> r2

data LeakInst reg pc
  = LReg RegIdx (reg Word)
  | LLoad Size RegIdx (reg Address)
  | LJump RegIdx (pc Address) (pc Address)
  | LJumpReg RegIdx (pc Address) (reg Address)
  | LStore Size (reg Address) RegIdx
  | LBranch (reg Bool) (pc Address)
  | LNop
  | LHalt

deriving instance
  ( Show (reg Word),
    Show (reg Address),
    Show (reg Bool),
    Show (pc Address)
  ) =>
  Show (LeakInst reg pc)

getLeakRd :: LeakInst reg pc -> Maybe RegIdx
getLeakRd (LReg rd _) = pure rd
getLeakRd (LLoad _ rd _) = pure rd
getLeakRd (LJump rd _ _) = pure rd
getLeakRd (LJumpReg rd _ _) = pure rd
getLeakRd _ = Nothing

getLeakR1 :: LeakInst RegComp pc -> Maybe RegIdx
getLeakR1 (LReg _ f) = fst $ deps f
getLeakR1 (LLoad _ _ f) = fst $ deps f
getLeakR1 (LJump {}) = Nothing
getLeakR1 (LJumpReg _ _ f) = fst $ deps f
getLeakR1 (LStore _ f _) = fst $ deps f
getLeakR1 (LBranch f _) = fst $ deps f

getLeakR2 :: LeakInst RegComp pc -> Maybe RegIdx
getLeakR2 (LReg _ f) = snd $ deps f
getLeakR2 (LLoad _ _ f) = snd $ deps f
getLeakR2 (LJump {}) = Nothing
getLeakR2 (LJumpReg _ _ f) = snd $ deps f
getLeakR2 (LStore _ _ r2) = pure r2
getLeakR2 (LBranch f _) = snd $ deps f

class DepReg a where
  deps :: a -> (Maybe RegIdx, Maybe RegIdx)

instance DepReg (WithReg a) where
  deps (Unary r _) = (pure r, Nothing)
  deps (Binary r1 r2 _) = (pure r1, pure r2)

instance DepReg (RegComp a) where
  deps (RegComp wr) = deps wr

instance DepReg (LeakInst RegComp b) where
  deps (LReg _ f) = deps f
  deps (LLoad _ _ f) = deps f
  deps (LJump _ _ _) = (Nothing, Nothing)
  deps (LJumpReg _ _ f) = deps f
  deps (LStore _ f r2) = (fst $ deps f, Just r2)
  deps (LBranch f _) = deps f
  deps LHalt = (Nothing, Nothing)
  deps LNop = (Nothing, Nothing)

depSet :: (DepReg a) => a -> Set RegIdx
depSet a =
  let (mr1, mr2) = deps a
   in S.fromList $ catMaybes [mr1, mr2]

binComp :: RegIdx -> RegIdx -> (Word -> Word -> a) -> RegComp a
binComp r1 r2 f = RegComp $ Binary r1 r2 f

unaryComp :: RegIdx -> (Word -> a) -> RegComp a
unaryComp r f = RegComp $ Unary r f

constComp :: a -> RegComp a
constComp = RegComp . Const

leak :: Input -> LeakInst RegComp PCM
leak input
  | not (inputIsInst input) = LNop
  | otherwise =
      case inst of
        RType op rd r1 r2 ->
          LReg rd $ binComp r1 r2 $ alu op
        IType iop rd r1 imm ->
          let op =
                case iop of
                  Arith op' -> op'
                  _ -> ADD
              alu_res = unaryComp r1 $ \r -> alu op r (signExtend imm)
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
                  LJumpReg rd (asks (bitCoerce . (+ 4))) $ bitCoerce <$> alu_res
                Env {} -> error ""
        SType size imm r1 r2 -> do
          let addr_comp = unpack <$> unaryComp r1 (\r -> alu ADD r (signExtend imm))
           in LStore size addr_comp r2
        BType cmp imm r1 r2 ->
          let branched_comp = binComp r1 r2 $ branch cmp
              addr_comp = asks (+ bitCoerce (signExtend imm))
           in LBranch branched_comp addr_comp
        UType Zero rd imm ->
          let imm' = imm ++# 0 `shiftL` 12
           in LReg rd $ constComp imm'
        -- FIX
        -- UType PC rd imm -> do
        --  let imm' = imm ++# 0 `shiftL` 12
        --  writeRF rd imm'
        --  pure $ mkInst' LOther
        JType rd imm ->
          LJump rd (asks (bitCoerce . (+ 4))) $ asks (+ bitCoerce (signExtend imm))
        EBREAK -> LHalt
        _ -> LNop
  where
    inst = Instruction.decode $ inputMem input

-- \| not (inputIsInst input) = pure leakNop
-- \| otherwise =
--     unlessStall $ do
--       let mkInst' = mkInst inst
--       inst' <- case inst of
--         RType op rd r1 r2 -> do
--           writeRF rd =<< alu op <$> readRF r1 <*> readRF r2
--           pure $ mkInst' LOther
--         IType iop rd r1 imm -> do
--           let op =
--                 case iop of
--                   Arith op' -> op'
--                   _ -> ADD
--           res <- alu op <$> readRF r1 <*> pure (signExtend imm)
--           case iop of
--             Arith {} -> do
--               writeRF rd res
--               pure $ mkInst' LOther
--             Load size sign -> do
--               let loadExtend = \case
--                     (Byte, Signed) -> signExtend $ slice d7 d0 res
--                     (Byte, Unsigned) -> zeroExtend $ slice d7 d0 res
--                     (Half, Signed) -> signExtend $ slice d15 d0 res
--                     (Half, Unsigned) -> signExtend $ slice d15 d0 res
--                     (Word, _) -> signExtend $ slice d31 d0 res
--                   val = loadExtend (size, sign)
--               writeRF rd =<< readRAM (unpack val)
--               pure $ mkInst' $ LLoad rd
--             Jump -> do
--               writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--               pure $ mkInst' $ LJ $ bitCoerce res
--             Env {} -> error ""
--         SType size imm r1 r2 -> do
--           addr <- unpack <$> (alu ADD <$> readRF r1 <*> pure (signExtend imm))
--           val <- readRF r2
--           writeRAM size addr val
--           pure $ mkInst' LStore
--         BType cmp imm r1 r2 -> do
--           branched <- branch cmp <$> readRF r1 <*> readRF r2
--           if branched
--             then
--               mkInst' . LJ . bitCoerce
--                 <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
--             else pure $ mkInst' LOther
--         UType Zero rd imm -> do
--           let imm' = imm ++# 0 `shiftL` 12
--           writeRF rd $ imm'
--           pure $ mkInst' LOther
--         UType PC rd imm -> do
--           let imm' = imm ++# 0 `shiftL` 12
--           writeRF rd imm'
--           pure $ mkInst' LOther
--         JType rd imm -> do
--           writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--           mkInst' . LJ . bitCoerce
--             <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
--         EBREAK ->
--           pure $ mkInst' LHalt
--         _ ->
--           pure leakNop
--       case leakBaseInst inst' of
--         LJ pc' -> modify $ \s ->
--           s
--             { leakPc = pc',
--               leakStall = leakStall s + 2
--             }
--         LLoad {} -> modify $ \s -> s {leakStall = leakStall s + 1}
--         _ -> modify $ \s -> s {leakPc = leakPc s + 4}

--       pure inst'

-- inst = Instruction.decode $ inputMem input
-- unlessStall m = do
--  stall <- gets leakStall
--  if (stall > 0)
--    then do
--      modify $ \s -> s {leakStall = stall - 1}
--      pure leakNop
--    else m

-- leakRun :: LeakState -> Input -> (LeakState, LeakInst)
-- leakRun s i = swap $ runState (leak i) s
--
-- readRF :: RegIdx -> LeakM Word
-- readRF idx = gets $ lookupRF idx . leakRF
--
-- writeRF :: RegIdx -> Word -> LeakM ()
-- writeRF idx val =
--  modify $ \s -> s {leakRF = modifyRF idx val $ leakRF s}
--
-- writeRAM :: Size -> Address -> Word -> LeakM ()
-- writeRAM size addr w =
--  modify $ \s -> s {leakRAM = write size addr w $ leakRAM s}
--
-- readRAM :: Address -> LeakM Word
-- readRAM addr = gets $ readWord addr . leakRAM
