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

data BaseTimeInst pc
  = TJump (Maybe (pc Address))
  | TBranch (Maybe (pc Bool)) (pc Address)
  | TLoad RegIdx
  | TStore
  | TOther
  | THalt

deriving instance
  ( Show (pc Bool),
    Show (pc Address)
  ) =>
  Show (BaseTimeInst pc)

timeNop :: TimeInst PCM
timeNop = TimeInst TOther mempty

data TimeInst pc = TimeInst
  { timeBaseInst :: BaseTimeInst pc,
    timeDeps :: Set RegIdx
    -- Debugging fields
    -- timeOgInst :: Instruction,
    -- timeDePc :: Address
  }

deriving instance
  ( Show (pc Bool),
    Show (pc Address)
  ) =>
  Show (TimeInst pc)

data Stage = Fe | De | Ex | Mem | Wb
  deriving (Show, Eq, Ord)

data TimeState = TimeState
  { timeExInstr :: LeakInst RegComp PCM,
    timeExRes :: Word,
    timeMemInstr :: LeakInst Done PCM,
    timeMemRes :: Word,
    timeWbInstr :: LeakInst Done PCM,
    timeStall :: Set Stage,
    timeHalt :: Bool,
    timeMeRegFwd :: Maybe (RegIdx, PCM Word),
    timeWbRegFwd :: Maybe (RegIdx, PCM Word)
  }
  deriving (Show)

initTime :: TimeState
initTime =
  TimeState
    { timeExInstr = LNop,
      timeExRes = 0,
      timeMemInstr = LNop,
      timeMemRes = 0,
      timeWbInstr = LNop,
      timeStall = mempty,
      timeHalt = False,
      timeMeRegFwd = Nothing,
      timeWbRegFwd = Nothing
    }

data TimeOut = TimeOut
  { timeInst :: First (TimeInst PCM),
    timeBranched :: First (PCM Bool),
    timeJumpAddress :: First (PCM Address)
  }

instance Semigroup TimeOut where
  TimeOut i1 b1 a1 <> TimeOut i2 b2 a2 = TimeOut (i1 <> i2) (b1 <> b2) (a1 <> a2)

instance Monoid TimeOut where
  mempty = TimeOut mempty mempty mempty

type TimeM = RWS (LeakInst RegComp PCM, Input) TimeOut TimeState

stallingM :: Stage -> TimeM Bool
stallingM stage =
  S.member stage <$> gets timeStall

stall :: [Stage] -> TimeM ()
stall stages =
  modify $ \s -> s {timeStall = timeStall s <> S.fromList stages}

-- outputNothing :: TimeM ()
-- outputNothing = tell mempty

timeDecode :: TimeM ()
timeDecode = do
  s <- get
  instr <- fst <$> ask
  when (isLoad instr) $
    stall [Fe]
  ex_ir <- gets timeExInstr
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
    s
      { timeExInstr =
          if stalling
            then LNop
            else instr
      }
  where
    isLoad :: LeakInst reg pc -> Bool
    isLoad (LLoad {}) = True
    isLoad _ = False

    loadHazard :: LeakInst RegComp PCM -> LeakInst reg pc -> Bool
    loadHazard de_ir ex_ir@(LLoad _ rd _) =
      any (== rd) $ S.toList $ depSet de_ir
    loadHazard _ _ = False

    mkTimeInst (LReg _ _) = TOther
    mkTimeInst (LLoad _ rd _) = TLoad rd
    mkTimeInst (LJump _ _ maddr) = TJump $ Just maddr
    mkTimeInst (LJumpReg _ _ _) = TJump Nothing
    mkTimeInst (LStore _ _ _) = TStore
    mkTimeInst (LBranch _ maddr) = TBranch Nothing maddr
    mkTimeInst LHalt = THalt
    mkTimeInst LNop = TOther

timeExecute :: TimeM ()
timeExecute = do
  instr <- gets timeExInstr
  r1 <- rs1
  r2 <- rs2

  instr' <-
    case instr of
      LReg rd f ->
        pure $ LReg rd $ applyRegComp f r1 r2
      LLoad size rd f ->
        pure $ LLoad size rd $ applyRegComp f r1 r2
      LJump rd m_push_pc m_jump_addr -> do
        pure $ LJump rd m_push_pc m_jump_addr
      LJumpReg rd m_push_pc f_jump_addr -> do
        let Done maddr = applyRegComp f_jump_addr r1 r2
        tell $ mempty {timeJumpAddress = pure maddr}
        pure $ LJumpReg rd m_push_pc $ Done maddr
      LStore size f_addr ri2 ->
        pure $ LStore size (applyRegComp f_addr r1 r2) ri2
      LBranch f_branched m_addr -> do
        let Done m_branch_res = applyRegComp f_branched r1 r2
        tell $ mempty {timeBranched = pure m_branch_res}
        pure $ LBranch (Done m_branch_res) m_addr
      LHalt -> pure LHalt
      LNop -> pure LNop

  modify $ \s -> s {timeMemInstr = instr'}
  where
    rs1 :: TimeM (PCM Word)
    rs1 = regWithFwd getLeakR1 =<< asks (inputRs1 . snd)

    rs2 :: TimeM (PCM Word)
    rs2 = regWithFwd getLeakR2 =<< asks (inputRs2 . snd)

    regWithFwd :: (LeakInst RegComp PCM -> Maybe RegIdx) -> Word -> TimeM (PCM Word)
    regWithFwd getR def = do
      ir <- gets timeExInstr
      let checkForFwd line = do
            (fwdIdx, fwdVal) <- MaybeT $ gets line
            guard (hazardRW getR ir fwdIdx)
            pure fwdVal
      fmap
        (fromMaybe $ pure def)
        $ runMaybeT
        $ checkForFwd timeMeRegFwd <|> checkForFwd timeWbRegFwd

    hazardRW :: (LeakInst reg pc -> Maybe RegIdx) -> LeakInst reg pc -> RegIdx -> Bool
    hazardRW getR src rd = isJust $ do
      rs <- getR src
      guard $ rd /= 0 && rs == rd

timeMemory :: TimeM ()
timeMemory = do
  instr <- gets timeMemInstr
  case instr of
    LLoad {} -> do
      -- outputNothing
      stall [Fe]
    LStore {} -> do
      -- outputNothing
      stall [Fe]
    LJump {} ->
      stall [De]
    LJumpReg {} ->
      stall [De]
    LBranch {} ->
      stall [De]
    _ -> pure ()

  modify $ \s -> s {timeWbInstr = timeMemInstr s}

timeWriteback :: TimeM ()
timeWriteback = do
  instr <- gets timeWbInstr
  halted <- gets timeHalt

  -- when halted $
  -- outputNothing

  case instr of
    -- THalt -> do
    --  modify $ \s ->
    --    s
    --      { timeMemInstr = timeNop,
    --        timeExInstr = timeNop,
    --        timeHalt = True
    --      }
    --  outputNothing
    LLoad {} -> stall [De]
    LStore {} -> stall [De]
    _ -> pure ()

timeTick :: TimeM ()
timeTick = do
  modify $ \s -> s {timeStall = mempty}
  timeWriteback
  timeMemory
  timeExecute
  timeDecode

timeRun :: TimeState -> (LeakInst RegComp PCM, Input) -> (TimeState, TimeOut)
timeRun s i = execRWS timeTick i s

data SimState = SimState
  { simFePc :: Address,
    simDePc :: Address,
    simExPc :: Address,
    simExInstr :: TimeInst PCM,
    simMemInstr :: TimeInst PCM,
    simWbInstr :: TimeInst PCM,
    simJumpStack :: [PCM Address],
    simBranchStack :: [PCM Bool],
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
      simJumpStack = mempty,
      simBranchStack = mempty,
      simJumpAddr = Nothing,
      simStall = mempty,
      simHalt = False
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
  mjmpAddr <- getFirst <$> asks timeJumpAddress
  modify $ \s -> s {simJumpStack = simJumpStack s Prelude.++ catMaybes [mjmpAddr]}
  stalling <- simStallingM Fe
  unless stalling $ do
    modify $ \s ->
      s
        { simFePc = fromMaybe (simFePc s + 4) (simJumpAddr s),
          simDePc = simFePc s
        }
  simOutputPC $ simFePc s

simDecode :: SimM ()
simDecode = do
  s <- get
  instr <- fromMaybe timeNop . getFirst <$> asks timeInst
  when (isLoad instr) $
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
    isLoad :: TimeInst PCM -> Bool
    isLoad (TimeInst (TLoad {}) _) = True
    isLoad _ = False

    loadHazard :: TimeInst PCM -> TimeInst PCM -> Bool
    loadHazard de_ir ex_ir@(TimeInst (TLoad rd) _) =
      any (== rd) $ S.toList $ timeDeps de_ir
    loadHazard _ _ = False

simExecute :: SimM ()
simExecute = do
  instr <- gets simExInstr
  case timeBaseInst instr of
    TJump {} -> do
      simDoStall [De]
      doJump
    TBranch _ mpc -> do
      simDoStall [De]
      doBranch mpc
    _ -> pure ()

  modify $ \s -> s {simMemInstr = instr}
  where
    doBranch :: PCM Address -> SimM ()
    doBranch mpc = do
      branchStack <- gets simBranchStack
      pc <- gets simExPc
      case branchStack of
        [] -> error "" -- should never happen --pure ()
        (b : bs) -> do
          let pc' = applyPC mpc pc
          modify $ \s ->
            s
              { simBranchStack = bs,
                simJumpAddr = if (applyPC b pc) then pure pc' else Nothing
              }

    doJump :: SimM ()
    doJump = do
      jmpStack <- gets simJumpStack
      case jmpStack of
        [] -> error "" -- should never happen --pure ()
        (j : js) -> do
          pc <- gets simExPc
          let pc' = applyPC j pc
          modify $ \s ->
            s
              { simJumpStack = js,
                simJumpAddr = pure pc'
              }

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
    -- Add Branch support
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
