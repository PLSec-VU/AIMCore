module Leak where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.TypeNats
import Instruction hiding (Jump, decode, halt)
import qualified Instruction
import Pipe
import Regfile
import Simulate (readWord)
import qualified Simulate
import Types
import Prelude hiding (Ordering (..), Word, const, init, map, not, undefined, (!!), (&&), (||))
import qualified Prelude

-- `fromJust` is safe here because the `fetch` stage unconditionally always reads
-- from memory (and its read may just be superseded by the `memory` stage).
obs :: Output -> Address
obs = memAddress . fromJust . getFirst . outMem

data WithReg
  = Constant Word
  | Unary RegIdx (Word -> Word)
  | Binary RegIdx RegIdx (Word -> Word -> Word)

instance Show WithReg where
  show _ = "withreg"

data JumpType
  = Offset
  | Absolute
  deriving (Show)

data LeakInst
  = RegStore RegIdx WithReg
  | MemLoad RegIdx WithReg
  | MemStore WithReg
  | Jump JumpType Address
  | Nop
  | Halt
  deriving (Show)

regTarg :: LeakInst -> Maybe RegIdx
regTarg (RegStore rd _) = pure rd
regTarg (MemLoad rd _) = pure rd
regTarg _ = Nothing

instrRd :: LeakInst -> Maybe Word
instrRd (RegStore _ (Constant w)) = pure w
instrRd (MemLoad _ (Constant w)) = pure w
instrRd _ = Nothing

class DepReg a where
  deps :: a -> (Maybe RegIdx, Maybe RegIdx)

instance DepReg WithReg where
  deps (Constant _) = (Nothing, Nothing)
  deps (Unary r _) = (pure r, Nothing)
  deps (Binary r1 r2 _) = (pure r1, pure r2)

instance DepReg LeakInst where
  deps (RegStore _ f) = deps f
  deps (MemLoad _ f) = deps f
  deps (MemStore f) = deps f
  deps _ = (Nothing, Nothing)

data LeakOut = LeakOut
  { leakOutInst :: First LeakInst,
    leakOutRs1 :: First Word,
    leakOutRs2 :: First Word
  }

instance Semigroup LeakOut where
  LeakOut i r1 r2 <> LeakOut i' r1' r2' = LeakOut (i <> i') (r1 <> r1') (r2 <> r2')

instance Monoid LeakOut where
  mempty = LeakOut mempty mempty mempty

data SimIn = SimIn
  { simInInst :: LeakInst,
    simInRs1 :: Word,
    simInRs2 :: Word
  }

type LeakM = RWS Input LeakOut LeakState

data LeakState = LeakState
  {
  }
  deriving (Show, Eq)

initLeak :: LeakState
initLeak =
  LeakState
    {
    }

instOut :: LeakInst -> LeakM ()
instOut inst =
  tell $ mempty {leakOutInst = pure inst}

leak :: LeakM ()
leak = do
  Input mem rs1 rs2 <- ask
  case Instruction.decode mem of
    RType op rd r1 r2 ->
      tell $
        mempty {leakOutInst = pure $ RegStore rd $ Binary r1 r2 $ alu op}
    IType iop rd r1 imm -> do
      let op =
            case iop of
              Arith op' -> op'
              _ -> ADD
          comp_res rs1 = alu op rs1 $ signExtend imm
      case iop of
        Arith {} ->
          tell $
            mempty
              { leakOutInst = pure $ RegStore rd $ Unary r1 comp_res
              }
        Load size sign -> do
          let loadExtend rs1 = \case
                (Byte, Signed) -> signExtend $ slice d7 d0 $ comp_res rs1
                (Byte, Unsigned) -> zeroExtend $ slice d7 d0 $ comp_res rs1
                (Half, Signed) -> signExtend $ slice d15 d0 $ comp_res rs1
                (Half, Unsigned) -> signExtend $ slice d15 d0 $ comp_res rs1
                (Word, _) -> signExtend $ slice d31 d0 $ comp_res rs1
              comp_val rs1 = loadExtend rs1 (size, sign)
          tell $
            mempty
              { leakOutInst =
                  pure $
                    MemLoad rd $
                      Unary r1 $
                        bitCoerce . alu ADD (signExtend imm) . comp_val
              }
    SType size imm r1 _ ->
      tell $
        mempty
          { leakOutInst = pure $ MemStore $ Unary r1 $ bitCoerce . alu ADD (signExtend imm)
          }
    JType rd imm ->
      tell $
        mempty {leakOutInst = pure $ Jump Offset (bitCoerce $ signExtend imm)}
    EBREAK ->
      tell $ mempty {leakOutInst = pure Halt}
    _ -> tell $ mempty {leakOutInst = pure Nop}

-- JType rd imm -> do
--  writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--  LJ . bitCoerce
--    <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))

-- s <- get
-- case leakLastInst s of
--  RType op rd _ _ ->
--    instOut $ RegWrite rd $ alu op rs1 rs2

-- modify $ \s -> s {leakLastInst = Instruction.decode mem}

-- IType iop rd r1 imm -> do
--  let op =
--        case iop of
--          Arith op' -> op'
--          _ -> ADD
--  res <- alu op rs1 $ pure (signExtend imm)
--  case iop of
--    Arith {} -> pure Nop
--    Load size sign -> do
--      let loadExtend = \case
--            (Byte, Signed) -> signExtend $ slice d7 d0 res
--            (Byte, Unsigned) -> zeroExtend $ slice d7 d0 res
--            (Half, Signed) -> signExtend $ slice d15 d0 res
--            (Half, Unsigned) -> signExtend $ slice d15 d0 res
--            (Word, _) -> signExtend $ slice d31 d0 res
--          val = loadExtend (size, sign)
--      writeRF rd =<< readRAM (unpack val)
--      pure Nop
--    Jump -> do
--      writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--      pure $ LJ $ bitCoerce res
--    Env {} -> error ""
-- SType size imm r1 r2 -> do
--  addr <- unpack <$> (alu ADD <$> readRF r1 <*> pure (signExtend imm))
--  val <- readRF r2
--  writeRAM size addr val
--  pure Nop
-- BType cmp imm r1 r2 -> do
--  branched <- branch cmp <$> readRF r1 <*> readRF r2
--  if branched
--    then
--      LJ . bitCoerce
--        <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
--    else pure Nop
-- UType Zero rd imm -> do
--  let imm' = imm ++# 0 `shiftL` 12
--  writeRF rd $ imm'
--  pure Nop
-- UType PC rd imm -> do
--  let imm' = imm ++# 0 `shiftL` 12
--  writeRF rd imm'
--  pure Nop
-- JType rd imm -> do
--  writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--  LJ . bitCoerce
--    <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))

leakOutToSimIn :: LeakOut -> SimIn
leakOutToSimIn (LeakOut minst mrs1 mrs2) =
  SimIn
    (fromMaybe Nop $ getFirst minst)
    (fromMaybe 0 $ getFirst mrs1)
    (fromMaybe 0 $ getFirst mrs2)

leakRun :: LeakState -> Input -> (LeakState, SimIn)
leakRun s i = fmap leakOutToSimIn $ execRWS leak i s

type SimM = RWS SimIn (First Address) SimState

stall :: [Stage] -> SimM ()
stall stages =
  modify $ \s -> s {simStall = simStall s <> S.fromList stages}

stallingM :: Stage -> SimM Bool
stallingM stage =
  S.member stage <$> gets simStall

data Stage = Fe | De | Ex | Mem | Wb
  deriving (Show, Eq, Ord)

data SimState = SimState
  { simPc :: Address,
    simExInstr :: LeakInst,
    simExRes :: Word,
    simMemInstr :: LeakInst,
    simMemRes :: Word,
    simWbInstr :: LeakInst,
    simStall :: Set Stage,
    simHalt :: Bool
  }
  deriving (Show)

withReg :: WithReg -> SimM Word
withReg (Constant a) = pure a
withReg (Unary _ f) = f <$> asks simInRs1
withReg (Binary _ _ f) = f <$> asks simInRs1 <*> asks simInRs2

initSim :: SimState
initSim =
  SimState
    { simPc = 4 * 50,
      simExInstr = Nop,
      simExRes = 0,
      simMemInstr = Nop,
      simMemRes = 0,
      simWbInstr = Nop,
      simStall = mempty,
      simHalt = False
    }

simFetch :: SimM ()
simFetch = do
  s <- get
  stalling <- stallingM Fe
  unless stalling $ do
    modify $ \s ->
      s
        { simPc = simPc s + 4
        }
  tell $ pure $ simPc s

simDecode :: SimM ()
simDecode = do
  s <- get
  instr <- asks simInInst
  instr' <-
    case instr of
      Jump Offset offset -> pure $ Jump Absolute $ simPc s + offset
      _ -> pure instr
  case instr' of
    MemLoad {} -> stall [Fe]
    _ -> pure ()
  stalling <- stallingM De
  modify $ \s ->
    s
      { simExInstr =
          if stalling
            then Nop
            else instr'
      }

simExecute :: SimM ()
simExecute = do
  s <- get
  case simExInstr s of
    RegStore rd f ->
      withReg f >> pure ()
    MemStore f -> do
      instr' <- MemStore . Constant <$> withRegFwd f
      modify $ \s ->
        s
          { simExInstr = instr'
          }
    Jump Absolute addr -> do
      stall [De]
      modify $ \s ->
        s
          { simPc = addr
          }
    _ -> pure ()
  modify $ \s -> s {simMemInstr = simExInstr s}
  where
    withRegFwd :: WithReg -> SimM Word
    withRegFwd (Constant w) = pure w
    withRegFwd (Unary r1 f) =
      f <$> regFwd r1 (asks simInRs1)
    withRegFwd (Binary r1 r2 f) =
      f <$> regFwd r1 (asks simInRs1) <*> regFwd r2 (asks simInRs2)

    regFwd :: RegIdx -> SimM Word -> SimM Word
    regFwd r defm = do
      mw <- runMaybeT $ getFwdReg r
      case mw of
        Nothing -> defm
        Just w -> pure w

    getFwdReg :: RegIdx -> MaybeT SimM Word
    getFwdReg r =
      do
        s <- get
        me_rd <- MaybeT $ regTarg <$> gets simMemInstr
        wb_rd <- MaybeT $ regTarg <$> gets simWbInstr
        fwd (hazardRW r me_rd) (simMemInstr s)
          <|> fwd (hazardRW r wb_rd) (simWbInstr s)
      where
        fwd cond instr =
          MaybeT $ pure $ instrRd instr

    hazardRW :: RegIdx -> RegIdx -> Bool
    hazardRW r rd =
      rd /= 0 && r == rd

simMemory :: SimM ()
simMemory = do
  instr <- gets simMemInstr
  case instr of
    MemLoad {} -> stall [Fe]
    MemStore (Constant addr) -> do
      tell $ pure $ bitCoerce addr
      stall [Fe]
    _ -> pure ()

  modify $ \s -> s {simWbInstr = simExInstr s}

simWriteback :: SimM ()
simWriteback = do
  instr <- gets simWbInstr
  halted <- gets simHalt

  when halted $ do
    tell $ pure 0

  case instr of
    Halt -> do
      modify $ \s ->
        s
          { simMemInstr = Nop,
            simExInstr = Nop,
            simHalt = True
          }
      tell $ pure 0
    RegStore rd val -> pure ()
    MemStore (Constant addr) ->
      tell $ pure $ bitCoerce addr
    _ -> pure ()

simTick :: SimM ()
simTick = do
  modify $ \s -> s {simStall = mempty}
  simWriteback
  simMemory
  simExecute
  simDecode
  simFetch

simRun :: SimState -> SimIn -> (SimState, Address)
simRun s i = (fromMaybe 0 . getFirst) <$> execRWS simTick i s

simLeakRun :: (LeakState, SimState) -> Input -> ((LeakState, SimState), Address)
simLeakRun (ls, ss) input = ((ls', ss'), addr)
  where
    (ls', simin) = leakRun ls input
    (ss', addr) = simRun ss simin

simIO :: forall n. (KnownNat n) => Vec n Byte -> IO ()
simIO =
  void
    . runRWST
      (simulate initInput initPipe initLeak initSim)
      ()
    . flip Simulate.Mem initRF
  where
    simulate i s leak_s sim_s =
      void $ forever $ do
        lift $ putStrLn "Press Enter to continue."
        _ <- lift getLine
        (i', s', _o) <- Simulate.report $ Simulate.simStep i s
        let ((leak_s', sim_s'), leak_addr) = simLeakRun (leak_s, sim_s) i
        lift $
          putStrLn $
            unlines
              [ "Leak out:",
                "--------------------",
                show leak_addr,
                "Leak state:",
                "--------------------",
                show leak_s,
                "Sim state:",
                "--------------------",
                show sim_s
              ]
        simulate i' s' leak_s' sim_s'
