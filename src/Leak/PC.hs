module Leak.PC where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.TypeNats
import Instruction hiding (decode, halt)
import qualified Instruction
import Pipe
import Regfile
import Simulate (MonadSim)
import qualified Simulate
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, not, undefined, (!!), (&&), (||))

-- `fromJust` is safe here because the `fetch` stage unconditionally always reads
-- from memory (and its read may just be superseded by the `memory` stage).
obs :: Output -> Address
obs = memAddress . fromJust . getFirst . outMem

data BaseLeakInst
  = LJ Address
  | LLoad RegIdx
  | LStore
  | LOther
  | LHalt
  deriving (Eq, Ord, Show)

-- | Instructions passed to the Simulator
data LeakInst = LeakInst
  { leakBaseInst :: BaseLeakInst,
    leakDeps :: Set RegIdx
  }
  deriving (Eq, Ord, Show)

data LeakState = LeakState
  { leakRF :: Regfile,
    leakRAM :: Vec MEM_SIZE_BYTES Byte,
    leakPc :: Address
  }
  deriving (Eq, Show)

leakNop :: LeakInst
leakNop = LeakInst LOther mempty

initLeak :: Vec PROG_SIZE Instruction -> LeakState
initLeak prog =
  LeakState
    { leakRF = initRF,
      leakRAM = mkRAM $ mkProg prog,
      leakPc = initPc
    }

mkInst :: Instruction -> BaseLeakInst -> LeakInst
mkInst inst leakInst =
  LeakInst
    { leakBaseInst = leakInst,
      leakDeps = S.fromList $ catMaybes [getRs1 inst, getRs2 inst]
    }

type LeakM = State LeakState

leak :: Input -> LeakM LeakInst
leak input
  | not $ inputIsInst input = pure leakNop
  | otherwise =
      let inst = Instruction.decode $ inputMem input
          mkInst' = mkInst inst
       in case inst of
            RType op rd r1 r2 -> do
              writeRF rd =<< alu op <$> readRF r1 <*> readRF r2
              pure $ mkInst' LOther
            IType iop rd r1 imm -> do
              let op =
                    case iop of
                      Arith op' -> op'
                      _ -> ADD
              res <- alu op <$> readRF r1 <*> pure (signExtend imm)
              case iop of
                Arith {} -> pure $ mkInst' LOther
                Load size sign -> do
                  let loadExtend = \case
                        (Byte, Signed) -> signExtend $ slice d7 d0 res
                        (Byte, Unsigned) -> zeroExtend $ slice d7 d0 res
                        (Half, Signed) -> signExtend $ slice d15 d0 res
                        (Half, Unsigned) -> signExtend $ slice d15 d0 res
                        (Word, _) -> signExtend $ slice d31 d0 res
                      val = loadExtend (size, sign)
                  writeRF rd =<< readRAM (unpack val)
                  pure $ mkInst' $ LLoad rd
                Jump -> do
                  writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
                  pure $ mkInst' $ LJ $ bitCoerce res
                Env {} -> error ""
            SType size imm r1 r2 -> do
              addr <- unpack <$> (alu ADD <$> readRF r1 <*> pure (signExtend imm))
              val <- readRF r2
              writeRAM size addr val
              pure $ mkInst' LStore
            BType cmp imm r1 r2 -> do
              branched <- branch cmp <$> readRF r1 <*> readRF r2
              if branched
                then
                  mkInst' . LJ . bitCoerce
                    <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
                else pure $ mkInst' LOther
            UType Zero rd imm -> do
              let imm' = imm ++# 0 `shiftL` 12
              writeRF rd $ imm'
              pure $ mkInst' LOther
            UType PC rd imm -> do
              let imm' = imm ++# 0 `shiftL` 12
              writeRF rd imm'
              pure $ mkInst' LOther
            JType rd imm -> do
              writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
              mkInst' . LJ . bitCoerce
                <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
            EBREAK ->
              pure $ mkInst' LHalt
            _ ->
              pure leakNop

leakRun :: LeakState -> Input -> (LeakState, LeakInst)
leakRun s i = swap $ runState (leak i) s

readRF :: RegIdx -> LeakM Word
readRF idx = gets $ lookupRF idx . leakRF

writeRF :: RegIdx -> Word -> LeakM ()
writeRF idx val =
  modify $ \s -> s {leakRF = modifyRF idx val $ leakRF s}

writeRAM :: Size -> Address -> Word -> LeakM ()
writeRAM size addr w =
  modify $ \s -> s {leakRAM = write size addr w $ leakRAM s}

readRAM :: Address -> LeakM Word
readRAM addr = gets $ readWord addr . leakRAM

type SimM = RWS LeakInst (First (Maybe Address)) SimState

stall :: [Stage] -> SimM ()
stall stages =
  modify $ \s -> s {simStall = simStall s <> S.fromList stages}

stallingM :: Stage -> SimM Bool
stallingM stage =
  S.member stage <$> gets simStall

outputPC :: Address -> SimM ()
outputPC addr =
  tell $ pure $ pure addr

outputNothing :: SimM ()
outputNothing = tell $ pure Nothing

data Stage = Fe | De | Ex | Mem | Wb
  deriving (Show, Eq, Ord)

data SimState = SimState
  { simFePc :: Address,
    simDePc :: Address,
    simExPc :: Address,
    simJumpAddr :: Maybe Address,
    simExInstr :: LeakInst,
    simExRes :: Word,
    simMemInstr :: LeakInst,
    simMemRes :: Word,
    simWbInstr :: LeakInst,
    simStall :: Set Stage,
    simHalt :: Bool
  }
  deriving (Show)

initSim :: SimState
initSim =
  SimState
    { simFePc = 4 * 50,
      simDePc = 0,
      simExPc = 0,
      simJumpAddr = Nothing,
      simExInstr = leakNop,
      simExRes = 0,
      simMemInstr = leakNop,
      simMemRes = 0,
      simWbInstr = leakNop,
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
        { simFePc = fromMaybe (simFePc s + 4) (simJumpAddr s),
          simDePc = simFePc s
        }
  outputPC $ simFePc s

simDecode :: SimM ()
simDecode = do
  s <- get
  instr <- ask
  when (isLoad instr) $
    stall [Fe]
  ex_ir <- gets simExInstr
  when (loadHazard instr ex_ir) $
    stall [De]
  stalling <- stallingM De
  modify $ \s ->
    s
      { simExInstr =
          if stalling
            then leakNop
            else instr,
        simExPc = simDePc s
      }
  where
    isLoad :: LeakInst -> Bool
    isLoad (LeakInst LLoad {} _) = True
    isLoad _ = False

    loadHazard :: LeakInst -> LeakInst -> Bool
    loadHazard de_ir ex_ir@(LeakInst (LLoad rd) _) =
      any (== rd) $ S.toList $ leakDeps de_ir
    loadHazard _ _ = False

simExecute :: SimM ()
simExecute = do
  instr <- gets simExInstr
  case leakBaseInst instr of
    LJ addr -> do
      stall [De]
      modify $ \s -> s {simJumpAddr = pure addr}
    _ -> pure ()

  modify $ \s -> s {simMemInstr = instr}

simMemory :: SimM ()
simMemory = do
  instr <- gets simMemInstr
  case leakBaseInst instr of
    LLoad _ -> do
      outputNothing
      stall [Fe]
    LStore -> do
      outputNothing
      stall [Fe]
    LJ {} ->
      stall [De]
    _ -> pure ()

  modify $ \s -> s {simWbInstr = simMemInstr s}

simWriteback :: SimM ()
simWriteback = do
  instr <- gets simWbInstr
  halted <- gets simHalt

  when halted $
    outputNothing

  case leakBaseInst instr of
    LHalt -> do
      modify $ \s ->
        s
          { simMemInstr = leakNop,
            simExInstr = leakNop,
            simHalt = True
          }
      outputNothing
    LLoad {} -> stall [De]
    LStore -> stall [De]
    _ -> pure ()

simTick :: SimM ()
simTick = do
  modify $ \s -> s {simStall = mempty, simJumpAddr = empty}
  simWriteback
  simMemory
  simExecute
  simDecode
  simFetch

simRun :: SimState -> LeakInst -> (SimState, Maybe Address)
simRun s i = (fromMaybe Nothing . getFirst) <$> execRWS simTick i s

simLeakRun ::
  (LeakState, SimState) ->
  Input ->
  ((LeakState, SimState), Maybe Address)
simLeakRun (ls, ss) input = ((ls', ss'), addr)
  where
    (ls', simin) = leakRun ls input
    (ss', addr) = simRun ss simin

-- simulator :: forall m. (MonadSim m) => CircuitSim Input (LeakState, SimState)
-- simulator =
--  CircuitSim
--    { circuitInput = initInput,
--      circuitState = (initLeak, initSim),
--      circuitStep = step,
--      circuitNext = next
--    }
--  where
--    step :: (MonadSim m) => Input -> Pipe -> m (Pipe, Output)
--    step i s = undefined
--      where
--        simPipe :: Pipe -> Input -> (Control, Pipe, Output)
--        simPipe = undefined
--          where
--            simPipeM :: CPUM Control
--            simPipeM = undefined
--
--    next :: (MonadSim m) => Output -> m (Maybe Input)
--    next (Output mem rs1 rs2 rd hlt) = undefined

-- simIO ::
--  forall n.
--  ( KnownNat n,
--    KnownNat (200 + (((GHC.TypeNats.*) n 4) + 200)), -- fix
--    KnownNat ((GHC.TypeNats.*) n 4)
--  ) =>
--  Vec n Word ->
--  IO ()
-- simIO prog =
--  void
--    $ runRWST
--      (simulate initInput initPipe (initLeak prog) initSim)
--      ()
--    $ Simulate.Mem (Simulate.mkRAM prog) initRF
--  where
--    simulate i s leak_s sim_s =
--      void $ forever $ do
--        lift $ putStrLn "Press Enter to continue."
--        _ <- lift getLine
--        (i', s', _o) <- reportIO $ Simulate.simStep i s
--        let ((leak_s', sim_s'), leak_addr) = simLeakRun (leak_s, sim_s) i
--        lift $
--          putStrLn $
--            unlines
--              [ "Leak out:",
--                "--------------------",
--                show leak_addr,
--                "Leak state:",
--                "--------------------",
--                show leak_s,
--                "Sim state:",
--                "--------------------",
--                show sim_s
--              ]
--        simulate i' s' leak_s' sim_s'
