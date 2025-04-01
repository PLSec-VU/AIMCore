{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Leak.PC where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
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
import Leak.Leak
import Pipe
import Regfile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data BaseTimeInst
  = TJump (Maybe Address)
  | TLoad RegIdx
  | TStore
  | TOther
  | THalt
  deriving (Show, Eq)

timeNop :: TimeInst
timeNop = TimeInst TOther mempty

data TimeInst = TimeInst
  { timeBaseInst :: BaseTimeInst,
    timeDeps :: Set RegIdx
    -- Debugging fields
    -- timeOgInst :: Instruction,
    -- timeDePc :: Address
  }
  deriving (Show, Eq)

data Stage = Fe | De | Ex | Mem | Wb
  deriving (Show, Eq, Ord)

data TimeState = TimeState
  { timeFePc :: Address,
    timeDePc :: Address,
    timeExPc :: Address,
    timeExInstr :: LeakInst ISAF,
    timeMemInstr :: LeakInst Done,
    timeWbInstr :: LeakInst Done,
    timeStall :: Set Stage,
    timeHalt :: Bool,
    timeMeRegFwd :: Maybe (RegIdx, Word),
    timeWbRegFwd :: Maybe (RegIdx, Word),
    timeJumpAddr :: Maybe Address
  }
  deriving (Show)

initTime :: TimeState
initTime =
  TimeState
    { timeFePc = initPc,
      timeDePc = 0,
      timeExPc = 0,
      timeExInstr = LNop,
      timeMemInstr = LNop,
      timeWbInstr = LNop,
      timeHalt = False,
      timeStall = mempty,
      timeMeRegFwd = Nothing,
      timeWbRegFwd = Nothing,
      timeJumpAddr = Nothing
    }

resetTimeCtrl :: TimeM ()
resetTimeCtrl =
  modify $ \s ->
    s
      { timeStall = mempty,
        timeMeRegFwd = Nothing,
        timeWbRegFwd = Nothing,
        timeJumpAddr = Nothing
      }

data TimeOut = TimeOut
  { timeInst :: First TimeInst,
    -- timeBranched :: First (Maybe Address),
    timeJumpAddress :: First Address
  }

instance Semigroup TimeOut where
  TimeOut i1 a1 <> TimeOut i2 a2 = TimeOut (i1 <> i2) (a1 <> a2)

instance Monoid TimeOut where
  mempty = TimeOut mempty mempty

type TimeM = RWS (LeakInst ISAF, Input) TimeOut TimeState

stallingM :: Stage -> TimeM Bool
stallingM stage =
  S.member stage <$> gets timeStall

stall :: [Stage] -> TimeM ()
stall stages =
  modify $ \s -> s {timeStall = timeStall s <> S.fromList stages}

outputNothing :: TimeM ()
outputNothing = tell mempty

timeFetch :: TimeM ()
timeFetch = do
  s <- get
  stalling <- stallingM Fe
  modify $ \s ->
    if stalling
      then
        s
          { timeFePc = fromMaybe (timeFePc s) (timeJumpAddr s)
          }
      else
        s
          { timeFePc = fromMaybe (timeFePc s + 4) (timeJumpAddr s),
            timeDePc = timeFePc s
          }

timeDecode :: TimeM ()
timeDecode = do
  s <- get
  instr <- fst <$> ask
  ex_ir <- gets timeExInstr
  when (isLoad instr) $ do
    stall [Fe]
  when (loadHazard instr ex_ir) $
    stall [De]
  stalling <- stallingM De
  unless stalling $
    tell $
      mempty
        { timeInst =
            pure $
              TimeInst
                { timeBaseInst = mkTimeInst instr,
                  timeDeps = depSet instr
                }
        }

  modify $ \s ->
    if stalling
      then s {timeExInstr = LNop}
      else
        s
          { timeExInstr = instr,
            timeExPc = timeDePc s
          }
  where
    isLoad :: LeakInst f -> Bool
    isLoad (LLoad {}) = True
    isLoad _ = False

    loadHazard :: LeakInst ISAF -> LeakInst f -> Bool
    loadHazard de_ir ex_ir@(LLoad _ rd _) =
      any (== rd) $ S.toList $ depSet de_ir
    loadHazard _ _ = False

    mkTimeInst (LReg _ _) = TOther
    mkTimeInst (LLoad _ rd _) = TLoad rd
    mkTimeInst (LJump _ _ _) = TJump Nothing
    mkTimeInst (LJumpReg _ _ _) = TJump Nothing
    mkTimeInst (LStore _ _ _) = TStore
    mkTimeInst (LBranch _ _) = TJump Nothing
    mkTimeInst LHalt = THalt
    mkTimeInst LNop = TOther

timeExecute :: TimeM ()
timeExecute = do
  instr <- gets timeExInstr
  pc <- gets timeExPc
  r1 <- rs1
  r2 <- rs2
  let apply f = unDone $ applyISAF f r1 r2 pc

  instr' <-
    case instr of
      LReg rd f ->
        pure $ LReg rd $ Done $ apply f
      LLoad size rd f ->
        pure $ LLoad size rd $ Done $ apply f
      LJump rd f_push_pc f_jump_addr -> do
        let push_pc = apply f_push_pc
            jump_addr = apply f_jump_addr
        stall [De]
        tell $ mempty {timeJumpAddress = pure jump_addr}
        modify $ \s -> s {timeJumpAddr = pure jump_addr}
        pure $ LJump rd (Done push_pc) (Done jump_addr)
      LJumpReg rd f_push_pc f_jump_addr -> do
        let addr = apply f_jump_addr
            push_pc = apply f_push_pc
        stall [De]
        tell $ mempty {timeJumpAddress = pure addr}
        modify $ \s -> s {timeJumpAddr = pure addr}
        pure $ LJumpReg rd (Done push_pc) (Done addr)
      LStore size f_addr ri2 ->
        pure $ LStore size (Done $ apply f_addr) ri2
      LBranch f_branched f_addr -> do
        pc <- gets timeExPc
        let branched = apply f_branched
            address = apply f_addr
        when branched $ do
          stall [De]
          modify $ \s -> s {timeJumpAddr = pure address}
          tell $ mempty {timeJumpAddress = pure address}
        pure $ LBranch (Done branched) (Done address)
      LHalt -> pure LHalt
      LNop -> pure LNop

  modify $ \s -> s {timeMemInstr = instr'}
  where
    rs1 :: TimeM Word
    rs1 = regWithFwd getLeakR1 =<< asks (inputRs1 . snd)

    rs2 :: TimeM Word
    rs2 = regWithFwd getLeakR2 =<< asks (inputRs2 . snd)

    regWithFwd :: (LeakInst ISAF -> Maybe RegIdx) -> Word -> TimeM Word
    regWithFwd getR def = do
      ir <- gets timeExInstr
      let checkForFwd line = do
            (fwdIdx, fwdVal) <- MaybeT $ gets line
            guard (hazardRW getR ir fwdIdx)
            pure fwdVal
      fmap
        (fromMaybe $ def)
        $ runMaybeT
        $ checkForFwd timeMeRegFwd <|> checkForFwd timeWbRegFwd

    hazardRW :: (LeakInst f -> Maybe RegIdx) -> LeakInst f -> RegIdx -> Bool
    hazardRW getR src rd = isJust $ do
      rs <- getR src
      guard $ rd /= 0 && rs == rd

timeMemory :: TimeM ()
timeMemory = do
  instr <- gets timeMemInstr

  mres <-
    case instr of
      LReg _ (Done res) ->
        pure $ Just res
      LLoad {} -> do
        stall [Fe]
        pure Nothing
      LJump _ (Done addr) _ -> do
        stall [De]
        pure $ Just $ bitCoerce addr
      LJumpReg _ (Done addr) _ -> do
        stall [De]
        pure $ Just $ bitCoerce addr
      LStore {} -> do
        stall [Fe]
        pure Nothing
      LBranch (Done branched) _ -> do
        when branched $
          stall [De]
        pure Nothing
      _ -> pure Nothing

  try $ do
    rd <- MaybeT $ pure $ getLeakRd instr
    res <- MaybeT $ pure mres
    lift $ modify $ \s -> s {timeMeRegFwd = pure (rd, res)}

  modify $ \s -> s {timeWbInstr = timeMemInstr s}

timeWriteback :: TimeM ()
timeWriteback = do
  input <- asks $ inputMem . snd
  instr <- gets timeWbInstr
  halted <- gets timeHalt

  when halted $
    outputNothing

  case instr of
    LLoad {} -> stall [De]
    LStore {} -> stall [De]
    _ -> pure ()

  mres <-
    case instr of
      LReg _ (Done res) ->
        pure $ Just res
      LLoad {} -> do
        stall [De]
        pure $ Just input
      LJump _ (Done addr) _ ->
        pure $ Just $ bitCoerce addr
      LJumpReg _ (Done addr) _ ->
        pure $ Just $ bitCoerce addr
      LStore {} -> do
        stall [De]
        pure Nothing
      LHalt -> do
        modify $ \s ->
          s
            { timeMemInstr = LNop,
              timeExInstr = LNop,
              timeHalt = True
            }
        outputNothing
        pure Nothing
      _ -> pure Nothing

  try $ do
    rd <- MaybeT $ pure $ getLeakRd instr
    res <- MaybeT $ pure mres
    lift $ modify $ \s -> s {timeWbRegFwd = pure (rd, res)}

timeTick :: TimeM ()
timeTick = do
  resetTimeCtrl
  timeWriteback
  timeMemory
  timeExecute
  timeDecode
  timeFetch

timeRun :: TimeState -> (LeakInst ISAF, Input) -> (TimeState, TimeOut)
timeRun s i = execRWS timeTick i s

data SimState = SimState
  { simFePc :: Address,
    simDePc :: Address,
    simExPc :: Address,
    simExInstr :: TimeInst,
    simMemInstr :: TimeInst,
    simWbInstr :: TimeInst,
    simJumpAddr :: Maybe Address,
    simStall :: Set Stage,
    simHalt :: Bool
  }
  deriving (Show)

initSim :: SimState
initSim =
  SimState
    { simFePc = initPc,
      simDePc = 0,
      simExPc = 0,
      simExInstr = timeNop,
      simMemInstr = timeNop,
      simWbInstr = timeNop,
      simHalt = False,
      simStall = mempty,
      simJumpAddr = Nothing
    }

resetSimCtrl :: SimM ()
resetSimCtrl =
  modify $ \s ->
    s
      { simStall = mempty,
        simJumpAddr = Nothing
      }

type SimM = RWS TimeOut (First (Maybe Address)) SimState

simDoStall :: [Stage] -> SimM ()
simDoStall stages =
  modify $ \s -> s {simStall = simStall s <> S.fromList stages}

simStallingM :: Stage -> SimM Bool
simStallingM stage =
  S.member stage <$> gets simStall

simOutputPC :: Address -> SimM ()
simOutputPC addr =
  tell $ pure $ pure addr

simOutputNothing :: SimM ()
simOutputNothing = tell $ pure Nothing

simFetch :: SimM ()
simFetch = do
  s <- get
  stalling <- simStallingM Fe
  modify $ \s ->
    if stalling
      then
        s
          { simFePc = fromMaybe (simFePc s) (simJumpAddr s)
          }
      else
        s
          { simFePc = fromMaybe (simFePc s + 4) (simJumpAddr s),
            simDePc = simFePc s
          }

  simOutputPC $ simFePc s

simDecode :: SimM ()
simDecode = do
  s <- get
  instr <- fromMaybe timeNop . getFirst <$> asks timeInst
  when (isLoad instr) $ do
    simOutputNothing
    simDoStall [Fe]
  ex_ir <- gets simExInstr
  when (loadHazard instr ex_ir) $
    simDoStall [De]
  stalling <- simStallingM De
  modify $ \s ->
    s
      { simExInstr =
          if stalling
            then timeNop
            else instr,
        simExPc = simDePc s
      }
  where
    isLoad :: TimeInst -> Bool
    isLoad (TimeInst (TLoad {}) _) = True
    isLoad _ = False

    loadHazard :: TimeInst -> TimeInst -> Bool
    loadHazard de_ir ex_ir@(TimeInst (TLoad rd) _) =
      any (== rd) $ S.toList $ timeDeps de_ir
    loadHazard _ _ = False

simExecute :: SimM ()
simExecute = do
  instr <- gets simExInstr
  mjmpAddr <- getFirst <$> asks timeJumpAddress
  modify $ \s ->
    s
      { simJumpAddr = mjmpAddr,
        simMemInstr = instr
      }
  case timeBaseInst instr of
    TJump {} -> do
      simDoStall [De]
      modify $ \s ->
        s
          { simMemInstr = instr {timeBaseInst = TJump mjmpAddr}
          }
    _ -> pure ()
  where

simMemory :: SimM ()
simMemory = do
  instr <- gets simMemInstr
  case timeBaseInst instr of
    TLoad {} -> do
      simOutputNothing
      simDoStall [Fe]
    TStore -> do
      simOutputNothing
      simDoStall [Fe]
    TJump {} ->
      simDoStall [De]
    _ -> pure ()

  modify $ \s -> s {simWbInstr = simMemInstr s}

simWriteback :: SimM ()
simWriteback = do
  instr <- gets simWbInstr
  halted <- gets simHalt

  when halted $
    simOutputNothing

  case timeBaseInst instr of
    THalt -> do
      simOutputNothing
      modify $ \s ->
        s
          { simMemInstr = timeNop,
            simExInstr = timeNop,
            simHalt = True
          }
      simOutputNothing
    TLoad {} -> simDoStall [De]
    TStore -> simDoStall [De]
    _ -> pure ()

simTick :: SimM ()
simTick = do
  modify $ \s -> s {simStall = mempty, simJumpAddr = empty}
  simWriteback
  simMemory
  simExecute
  simDecode
  simFetch

simRun :: SimState -> TimeOut -> (SimState, Maybe Address)
simRun s i = (fromMaybe Nothing . getFirst) <$> execRWS simTick i s

leakTimeSimRun ::
  Input ->
  (TimeState, SimState) ->
  ((TimeState, SimState), Maybe Address)
leakTimeSimRun input (ts, ss) = ((ts', ss'), addr)
  where
    time_in = leak input
    (ts', sim_in) = timeRun ts (time_in, input)
    (ss', addr) = simRun ss sim_in

simulator ::
  forall m.
  ( MonadState ((Pipe, Output), Simulate.Mem MEM_SIZE_BYTES) m
  ) =>
  Vec PROG_SIZE Word ->
  CircuitSim m Input (TimeState, SimState) (Maybe Address, Maybe Address)
simulator prog =
  CircuitSim
    { circuitInput = initInput,
      circuitState = (initTime, initSim),
      circuitStep = step,
      circuitNext = next
    }
  where
    obs :: Output -> Maybe Address
    obs sim_o = do
      mem <- getFirst $ outMem sim_o
      guard $ memIsInst mem
      pure $ memAddress mem

    step ::
      Input ->
      (TimeState, SimState) ->
      m ((TimeState, SimState), (Maybe Address, Maybe Address))
    step i s = do
      ((sim_s, _), mem) <- get
      let (sim_res@(_, sim_o), mem') = runState (circuitStep Simulate.simulator i sim_s) mem
      put (sim_res, mem')
      let (s', o) = leakTimeSimRun i s
      pure (s', (o, obs sim_o))

    next :: (Maybe Address, Maybe Address) -> m (Maybe Input)
    next (o, sim_addr) = do
      ((_, sim_o), mem) <- get
      let (mi, mem') = runState (circuitNext Simulate.simulator sim_o) mem
      modify $ \(s, _mem) -> (s, mem')
      pure mi

runSimulator ::
  ( CircuitSim
      (State ((Pipe, Output), Simulate.Mem MEM_SIZE_BYTES))
      Input
      (TimeState, SimState)
      (Maybe Address, Maybe Address) ->
    State ((Pipe, Output), Simulate.Mem MEM_SIZE_BYTES) a
  ) ->
  Vec PROG_SIZE Word ->
  a
runSimulator f prog = evalState (f $ simulator prog) $ mkS prog
  where
    mkS prog = ((initPipe, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  Vec PROG_SIZE Word ->
  [((TimeState, SimState), (Maybe Address, Maybe Address), Maybe Input)]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'
