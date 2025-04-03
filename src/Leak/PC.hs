{-# LANGUAGE UndecidableInstances #-}

module Leak.PC
  ( leakTimeSimRun,
    simulator,
    runSimulator,
    watchSim,
    pcsEqual,
    proj,
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Instruction (Instruction)
import qualified Instruction
import Interp (Done (..), InterpF, applyInterpF)
import qualified Interp
import Pipe
import Regfile
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data BaseTimeInst
  = TJump
  | TLoad RegIdx
  | TStore
  | TOther
  | TBreak
  deriving (Show, Eq)

timeNop :: TimeInst
timeNop = TimeInst TOther mempty

data TimeInst = TimeInst
  { timeBaseInst :: BaseTimeInst,
    timeDeps :: Set RegIdx
  }
  deriving (Show, Eq)

data Stage = Fe | De | Ex | Mem | Wb
  deriving (Show, Eq, Ord)

data TimeState = TimeState
  { timeFePc :: Address,
    timeDePc :: Address,
    timeExPc :: Address,
    timeExInstr :: Interp.Inst InterpF,
    timeMemInstr :: Interp.Inst Done,
    timeWbInstr :: Interp.Inst Done,
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
      timeExInstr = Interp.Nop,
      timeMemInstr = Interp.Nop,
      timeWbInstr = Interp.Nop,
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
    timeJumpAddress :: First Address
  }

instance Semigroup TimeOut where
  TimeOut i1 a1 <> TimeOut i2 a2 = TimeOut (i1 <> i2) (a1 <> a2)

instance Monoid TimeOut where
  mempty = TimeOut mempty mempty

type TimeM = RWS (Interp.Inst InterpF, Input) TimeOut TimeState

stallingM :: Stage -> TimeM Bool
stallingM stage =
  gets $ S.member stage . timeStall

stall :: [Stage] -> TimeM ()
stall stages =
  modify $ \s -> s {timeStall = timeStall s <> S.fromList stages}

outputNothing :: TimeM ()
outputNothing = tell mempty

timeFetch :: TimeM ()
timeFetch = do
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
                  timeDeps = Interp.depSet instr
                }
        }

  modify $ \s ->
    if stalling
      then s {timeExInstr = Interp.Nop}
      else
        s
          { timeExInstr = instr,
            timeExPc = timeDePc s
          }
  where
    isLoad :: Interp.Inst f -> Bool
    isLoad (Interp.Load {}) = True
    isLoad _ = False

    loadHazard :: Interp.Inst InterpF -> Interp.Inst f -> Bool
    loadHazard de_ir (Interp.Load _ rd _) =
      elem rd $ S.toList $ Interp.depSet de_ir
    loadHazard _ _ = False

mkTimeInst :: Interp.Inst f -> BaseTimeInst
mkTimeInst Interp.Reg {} = TOther
mkTimeInst (Interp.Load _ rd _) = TLoad rd
mkTimeInst Interp.Jump {} = TJump
mkTimeInst Interp.JumpReg {} = TJump
mkTimeInst Interp.Store {} = TStore
mkTimeInst Interp.Branch {} = TJump
mkTimeInst Interp.Break = TBreak
mkTimeInst Interp.Nop = TOther

timeExecute :: TimeM ()
timeExecute = do
  instr <- gets timeExInstr
  pc <- gets timeExPc
  r1 <- rs1
  r2 <- rs2
  let apply f = unDone $ applyInterpF f r1 r2 pc

  instr' <-
    case instr of
      Interp.Reg rd f ->
        pure $ Interp.Reg rd $ Done $ apply f
      Interp.Load size rd f ->
        pure $ Interp.Load size rd $ Done $ apply f
      Interp.Jump rd f_push_pc f_jump_addr -> do
        let push_pc = apply f_push_pc
            jump_addr = apply f_jump_addr
        stall [De]
        tell $ mempty {timeJumpAddress = pure jump_addr}
        modify $ \s -> s {timeJumpAddr = pure jump_addr}
        pure $ Interp.Jump rd (Done push_pc) (Done jump_addr)
      Interp.JumpReg rd f_push_pc f_jump_addr -> do
        let addr = apply f_jump_addr
            push_pc = apply f_push_pc
        stall [De]
        tell $ mempty {timeJumpAddress = pure addr}
        modify $ \s -> s {timeJumpAddr = pure addr}
        pure $ Interp.JumpReg rd (Done push_pc) (Done addr)
      Interp.Store size f_addr ri2 ->
        pure $ Interp.Store size (Done $ apply f_addr) ri2
      Interp.Branch f_branched f_addr -> do
        let branched = apply f_branched
            address = apply f_addr
        when branched $ do
          stall [De]
          modify $ \s -> s {timeJumpAddr = pure address}
          tell $ mempty {timeJumpAddress = pure address}
        pure $ Interp.Branch (Done branched) (Done address)
      Interp.Break -> pure Interp.Break
      Interp.Nop -> pure Interp.Nop

  modify $ \s -> s {timeMemInstr = instr'}
  where
    rs1 :: TimeM Word
    rs1 = regWithFwd Interp.getR1 =<< asks (inputRs1 . snd)

    rs2 :: TimeM Word
    rs2 = regWithFwd Interp.getR2 =<< asks (inputRs2 . snd)

    regWithFwd :: (Interp.Inst InterpF -> Maybe RegIdx) -> Word -> TimeM Word
    regWithFwd getR def = do
      ir <- gets timeExInstr
      let checkForFwd line = do
            (fwdIdx, fwdVal) <- MaybeT $ gets line
            guard (hazardRW getR ir fwdIdx)
            pure fwdVal
      fmap
        (fromMaybe def)
        $ runMaybeT
        $ checkForFwd timeMeRegFwd <|> checkForFwd timeWbRegFwd

    hazardRW :: (Interp.Inst f -> Maybe RegIdx) -> Interp.Inst f -> RegIdx -> Bool
    hazardRW getR src rd = isJust $ do
      rs <- getR src
      guard $ rd /= 0 && rs == rd

timeMemory :: TimeM ()
timeMemory = do
  instr <- gets timeMemInstr

  mres <-
    case instr of
      Interp.Reg _ (Done res) ->
        pure $ Just res
      Interp.Load {} -> do
        stall [Fe]
        pure Nothing
      Interp.Jump _ (Done addr) _ -> do
        stall [De]
        pure $ Just $ bitCoerce addr
      Interp.JumpReg _ (Done addr) _ -> do
        stall [De]
        pure $ Just $ bitCoerce addr
      Interp.Store {} -> do
        stall [Fe]
        pure Nothing
      Interp.Branch (Done branched) _ -> do
        when branched $
          stall [De]
        pure Nothing
      _ -> pure Nothing

  try $ do
    rd <- MaybeT $ pure $ Interp.getRd instr
    res <- MaybeT $ pure mres
    lift $ modify $ \s -> s {timeMeRegFwd = pure (rd, res)}

  modify $ \s -> s {timeWbInstr = timeMemInstr s}

timeWriteback :: TimeM ()
timeWriteback = do
  input <- asks $ inputMem . snd
  instr <- gets timeWbInstr
  halted <- gets timeHalt

  when
    halted
    outputNothing

  case instr of
    Interp.Load {} -> stall [De]
    Interp.Store {} -> stall [De]
    _ -> pure ()

  mres <-
    case instr of
      Interp.Reg _ (Done res) ->
        pure $ Just res
      Interp.Load {} -> do
        stall [De]
        pure $ Just input
      Interp.Jump _ (Done addr) _ ->
        pure $ Just $ bitCoerce addr
      Interp.JumpReg _ (Done addr) _ ->
        pure $ Just $ bitCoerce addr
      Interp.Store {} -> do
        stall [De]
        pure Nothing
      Interp.Break -> do
        modify $ \s ->
          s
            { timeMemInstr = Interp.Nop,
              timeExInstr = Interp.Nop,
              timeHalt = True
            }
        outputNothing
        pure Nothing
      _ -> pure Nothing

  try $ do
    rd <- MaybeT $ pure $ Interp.getRd instr
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

timeRun :: TimeState -> (Interp.Inst InterpF, Input) -> (TimeState, TimeOut)
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
  gets (S.member stage . simStall)

simOutputPC :: Address -> SimM ()
simOutputPC addr =
  tell $ pure $ pure addr

simOutputNothing :: SimM ()
simOutputNothing = tell $ pure Nothing

simFetch :: SimM ()
simFetch = do
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

  simOutputPC =<< gets simFePc

simDecode :: SimM ()
simDecode = do
  instr <- fromMaybe timeNop . getFirst <$> asks timeInst
  when (isLoad instr) $ do
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
    loadHazard de_ir (TimeInst (TLoad rd) _) =
      elem rd $ S.toList $ timeDeps de_ir
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
    TJump -> do
      case mjmpAddr of
        Just {} -> do
          simDoStall [De]
          modify $ \s ->
            s
              { simMemInstr = instr {timeBaseInst = TJump}
              }
        Nothing ->
          modify $ \s ->
            s
              { simMemInstr = instr {timeBaseInst = TOther}
              }
    _ -> pure ()

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
    TJump ->
      simDoStall [De]
    _ -> pure ()

  modify $ \s -> s {simWbInstr = simMemInstr s}

simWriteback :: SimM ()
simWriteback = do
  instr <- gets simWbInstr
  halted <- gets simHalt

  when
    halted
    simOutputNothing

  case timeBaseInst instr of
    TBreak -> do
      simOutputNothing
      modify $ \s ->
        s
          { simMemInstr = timeNop,
            simExInstr = timeNop,
            simHalt = True
          }
    TLoad {} -> do
      simDoStall [De]
    TStore -> do
      simDoStall [De]
    _ -> pure ()

simTick :: SimM ()
simTick = do
  resetSimCtrl
  simWriteback
  simMemory
  simExecute
  simDecode
  simFetch

simRun :: SimState -> TimeOut -> (SimState, Maybe Address)
simRun s i = fromMaybe Nothing . getFirst <$> execRWS simTick i s

leakTimeSimRun ::
  Input ->
  (TimeState, SimState) ->
  ((TimeState, SimState), Maybe Address)
leakTimeSimRun input (ts, ss) = ((ts', ss'), addr)
  where
    time_in = Interp.interp input
    (ts', sim_in) = timeRun ts (time_in, input)
    (ss', addr) = simRun ss sim_in

simulator ::
  forall m.
  ( MonadState ((Pipe, Output), Simulate.Mem MEM_SIZE_BYTES) m
  ) =>
  CircuitSim m Input (TimeState, SimState) (Maybe Address, Maybe Address)
simulator =
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
    next (_o, _sim_addr) = do
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
runSimulator f prog = evalState (f simulator) s
  where
    s = ((initPipe, mempty), Simulate.Mem (mkRAM prog) initRF)

watchSim ::
  Vec PROG_SIZE Word ->
  [((TimeState, SimState), (Maybe Address, Maybe Address), Maybe Input)]
watchSim = runSimulator watch

pcsEqual :: Vec PROG_SIZE Word -> Bool
pcsEqual = all check . watchSim
  where
    check (_, (o, o'), _) = o == o'

proj :: Pipe -> (TimeState, SimState)
proj s = (ts, ss)
  where
    ts =
      TimeState
        { timeFePc = fePc s,
          timeDePc = dePc s,
          timeExPc = exPc s,
          timeExInstr = convertInst $ exIr s,
          timeMemInstr = convertInstDone (meIr s) Mem,
          timeWbInstr = convertInstDone (wbIr s) Wb,
          timeStall = convertStall $ pipeCtrl s,
          timeHalt = pipeHalt s,
          timeMeRegFwd = ctrlMeRegFwd $ pipeCtrl s,
          timeWbRegFwd = ctrlWbRegFwd $ pipeCtrl s,
          timeJumpAddr = ctrlExBranch $ pipeCtrl s
        }
    ss =
      SimState
        { simFePc = fePc s,
          simDePc = dePc s,
          simExPc = exPc s,
          simExInstr = convertTime $ exIr s,
          simMemInstr = convertTime $ meIr s,
          simWbInstr = convertTime $ wbIr s,
          simHalt = pipeHalt s,
          simStall = convertStall $ pipeCtrl s,
          simJumpAddr = ctrlExBranch $ pipeCtrl s
        }

    convertTime :: Instruction -> TimeInst
    convertTime inst = TimeInst (mkTimeInst $ convertInst inst) (Interp.depSet $ convertInst inst)

    convertInst :: Instruction -> Interp.Inst InterpF -- fix
    convertInst i =
      Interp.interp $
        Input
          { inputIsInst = True,
            inputMem = Instruction.encode i,
            inputRs1 = 0,
            inputRs2 = 0
          }
    convertInstDone :: Instruction -> Stage -> Interp.Inst Done -- fix
    convertInstDone i stage =
      case li of
        Interp.Reg rd _ -> Interp.Reg rd $ Done res
        Interp.Load size rd _ -> Interp.Load size rd $ Done $ bitCoerce res
        Interp.Jump rd _ _ -> Interp.Jump rd (Done $ bitCoerce res) lol
        Interp.JumpReg rd _ _ -> Interp.JumpReg rd (Done $ bitCoerce res) lol
        Interp.Store size _ r2 -> Interp.Store size (Done $ bitCoerce res) r2
        Interp.Branch _ _ -> Interp.Branch (Done branched) lol
        Interp.Nop -> Interp.Nop
        Interp.Break -> Interp.Break
      where
        li = convertInst i
        res
          | stage == Mem = meRe s
          | otherwise = wbRe s
        branched = (stage == Mem) && meBranch s
        lol = Done 0

    convertStall :: Control -> Set Stage
    convertStall ctrl =
      S.fromList [stage | (stage, conds) <- stallConditions, any ($ ctrl) conds]
      where
        stallConditions =
          [ (Fe, [ctrlDecodeLoad, ctrlMemOutputActive]),
            (De, [ctrlFirstCycle, isJust . ctrlExBranch, ctrlMemInputActive, ctrlMemBranch])
          ]
