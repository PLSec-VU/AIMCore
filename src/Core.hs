module Core
  ( initInput,
    init,
    initCtrl,
    withCtrlReset,
    circuit,
    Input (..),
    Output (..),
    State (..),
    fetch,
    decode,
    dispatch,
    issue,
    execute,
    writeback,
    commit,
    CPUM,
    MemAccess (..),
    Control (..),
    ReservationStation (..),
    ReorderBuffer (..),
    ROBEntry (..),
    alu,
    branch,
    topEntity,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Monoid
import Instruction hiding (decode)
import Types
import Util
import Prelude hiding (map, (!!), Ordering (..), Word, init, lines, not, undefined, (&&), (||), repeat, zip)

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Input ->
  Signal System Output
topEntity = exposeClockResetEnable $ mealy circuit init

data Input = Input
  { inputIsInstr :: Bool,
    inputMem :: Word,
    inputRs1 :: Word,
    inputRs2 :: Word
  }
  deriving (Show, Generic, NFDataX)

data MemAccess = MemAccess
  { memIsInstr :: Bool,
    memAddress :: Address,
    memSize :: Size,
    memVal :: Maybe Word
  }
  deriving (Show, Generic, NFDataX)

data Output = Output
  { outMem :: First MemAccess,
    outRs1 :: First RegIdx,
    outRs2 :: First RegIdx,
    outRd :: First (RegIdx, Word),
    outHalt :: First Bool
  }
  deriving (Show, Generic, NFDataX)

instance Semigroup Output where
  Output mem rs1 rs2 rd hlt <> Output mem' rs1' rs2' rd' hlt' =
    Output (mem <> mem') (rs1 <> rs1') (rs2 <> rs2') (rd <> rd') (hlt <> hlt')

instance Monoid Output where
  mempty = Output mempty mempty mempty mempty mempty

data ReservationStation = ReservationStation
  { rsValid :: Bool,
    rsOp :: Arith,
    rsVj :: Maybe Word,
    rsVk :: Maybe Word,
    rsQj :: Maybe StationId,
    rsQk :: Maybe StationId,
    rsRobSlot :: BitVector 4,
    rsAddress :: Maybe Address
  }
  deriving (Show, Generic, NFDataX)

data ROBEntry = ROBEntry
  { robValid :: Bool,
    robReady :: Bool,
    robInstr :: Instruction,
    robValue :: Word,
    robDest :: Maybe RegIdx,
    robPc :: Address
  }
  deriving (Show, Generic, NFDataX)

newtype ReorderBuffer = ReorderBuffer (Vec 16 ROBEntry)
  deriving (Show, Generic, NFDataX)

data RegFileEntry = Ready Word | Pending StationId
  deriving (Show, Generic, NFDataX)

newtype RegFile = RegFile (Vec 32 RegFileEntry)
  deriving (Show, Generic, NFDataX)

type StationId = BitVector 4

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateDispatchInstr :: Instruction,
    stateReservationStations :: Vec 8 ReservationStation,
    stateLoadStoreQueue :: Vec 4 ReservationStation,
    stateROB :: ReorderBuffer,
    stateROBHead :: BitVector 4,
    stateROBTail :: BitVector 4,
    stateRegFile :: RegFile,
    stateFreeStations :: Vec 8 Bool,
    stateFreeLSQ :: Vec 4 Bool,
    stateCtrl :: Control,
    stateHalt :: Bool
  }
  deriving (Show, Generic, NFDataX)
data Control = Control
  { ctrlFirstCycle :: Bool,
    ctrlCDB :: Maybe (StationId, Word),
    ctrlBranchMispredict :: Maybe Address,
    ctrlMemBusy :: Bool
  }
  deriving (Show, Eq, Generic, NFDataX)

type CPUM = RWS Input Output State

circuit :: State -> Input -> (State, Output)
circuit = flip $ execRWS pipe

pipe :: CPUM ()
pipe = void $ withCtrlReset $ do
  commit
  writeback
  execute
  issue
  dispatch
  decode
  fetch

initInput :: Input
initInput =
  Input
    { inputIsInstr = False,
      inputMem = 0,
      inputRs1 = 0,
      inputRs2 = 0
    }

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateDispatchInstr = nop,
      stateReservationStations = repeat initRS,
      stateLoadStoreQueue = repeat initRS,
      stateROB = ReorderBuffer $ repeat initROBEntry,
      stateROBHead = 0,
      stateROBTail = 0,
      stateRegFile = initRF,
      stateFreeStations = repeat True,
      stateFreeLSQ = repeat True,
      stateCtrl = initCtrl,
      stateHalt = False
    }
  where
    initRS = ReservationStation False ADD Nothing Nothing Nothing Nothing 0 Nothing
    initROBEntry = ROBEntry False False nop 0 Nothing 0

initCtrl :: Control
initCtrl =
  Control
    { ctrlFirstCycle = True,
      ctrlCDB = Nothing,
      ctrlBranchMispredict = Nothing,
      ctrlMemBusy = False
    }

initRF :: RegFile
initRF = RegFile $ replace 0 (Ready 0) $ repeat (Ready 0)

withCtrlReset :: CPUM () -> CPUM Control
withCtrlReset m = do
  firstCycle <- gets $ ctrlFirstCycle . stateCtrl
  modify $ \s -> s {stateCtrl = initCtrl {ctrlFirstCycle = firstCycle}}
  m
  ctrl <- gets stateCtrl
  modify $ \s -> s {stateCtrl = (stateCtrl s) {ctrlFirstCycle = False}}
  pure ctrl

halt :: CPUM ()
halt =
  modify $ \s -> s {stateHalt = True}

fetch :: CPUM ()
fetch = do
  ctrl <- gets stateCtrl
  pc <- gets stateFePc

  case ctrlBranchMispredict ctrl of
    Just newPc -> do
      modify $ \s -> s { stateFePc = newPc, stateDispatchInstr = nop }
      flushPipeline
    Nothing -> do
      readPC pc
      modify $ \s -> s {
        stateFePc = pc + 4,
        stateDePc = pc
      }

decode :: CPUM ()
decode = do
  input <- ask
  ctrl <- gets stateCtrl

  let ir = if inputIsInstr input && not (ctrlFirstCycle ctrl)
           then Instruction.decode' $ inputMem input
           else nop

  modify $ \s -> s { stateDispatchInstr = ir }

dispatch :: CPUM ()
dispatch = do
  instr <- gets stateDispatchInstr
  robTail <- gets stateROBTail
  freeStations <- gets stateFreeStations
  freeLSQ <- gets stateFreeLSQ
  regFile <- gets stateRegFile

  when (instr /= nop) $ do
    rob <- gets stateROB
    let canAllocate = not (isROBFull rob robTail)
        needsALU = isALUInstr instr
        needsLSQ = isMemInstr instr
        hasStation = if needsALU
                    then any id $ toList freeStations
                    else if needsLSQ
                    then any id $ toList freeLSQ
                    else True

    when (canAllocate && hasStation) $ do
      pc <- gets stateDePc
      let robEntry = ROBEntry True False instr 0 (getRd instr) pc
      allocateROBEntry robEntry

      case getRd instr of
        Just rd -> do
          let stationId = robTail
          modify $ \s -> s { stateRegFile = reserveRF rd stationId regFile }
        Nothing -> pure ()

      allocateReservationStation instr robTail

issue :: CPUM ()
issue = do
  stations <- gets stateReservationStations
  lsq <- gets stateLoadStoreQueue

  let readyALU = findReadyStation stations
      readyMem = findReadyStation lsq

  case readyALU of
    Just idx -> markIssued (fromIntegral idx) True
    Nothing -> pure ()

  case readyMem of
    Just idx -> markIssued (fromIntegral idx) False
    Nothing -> pure ()

execute :: CPUM ()
execute = do
  stations <- gets stateReservationStations
  lsq <- gets stateLoadStoreQueue

  forM_ (zip (iterateI (+1) 0) stations) $ \(idx :: BitVector 4, rs) ->
    when (rsValid rs && isJust (rsVj rs) && isJust (rsVk rs)) $ do
      let result = alu False (rsOp rs) (fromJust $ rsVj rs) (fromJust $ rsVk rs)
          stationId = idx
      setLines $ \c -> c { ctrlCDB = Just (stationId, result) }
      clearReservationStation idx True

writeback :: CPUM ()
writeback = do
  ctrl <- gets stateCtrl

  case ctrlCDB ctrl of
    Just (stationId, value) -> do
      updateROBResult stationId value
      updateWaitingStations stationId value
    Nothing -> pure ()

commit :: CPUM ()
commit = do
  rob <- gets stateROB
  headIdx <- gets stateROBHead
  regFile <- gets stateRegFile

  let (ReorderBuffer robVec) = rob
      headEntry = robVec !! headIdx

  when (robValid headEntry && robReady headEntry) $ do
    case robDest headEntry of
      Just rd -> do
        let newRegFile = releaseRF rd (robValue headEntry) regFile
        modify $ \s -> s { stateRegFile = newRegFile }
        tell $ mempty { outRd = pure (rd, robValue headEntry) }
      Nothing -> pure ()

    when (isBranch $ robInstr headEntry) $ do
      pure ()

    let newHead = (headIdx + 1) `mod` 16
    modify $ \s -> s { stateROBHead = newHead }

allocateROBEntry :: ROBEntry -> CPUM ()
allocateROBEntry entry = do
  tailIdx <- gets stateROBTail
  rob <- gets stateROB
  let (ReorderBuffer robVec) = rob
      newROBVec = replace tailIdx entry robVec
      newTail = (tailIdx + 1) `mod` 16
  modify $ \s -> s {
    stateROB = ReorderBuffer newROBVec,
    stateROBTail = newTail
  }

allocateReservationStation :: Instruction -> BitVector 4 -> CPUM ()
allocateReservationStation instr robSlot = do
  regFile <- gets stateRegFile

  let rs1Val = case getRs1 instr of
        Just rs1 -> case lookupRF rs1 regFile of
          Ready val -> Just val
          Pending _ -> Nothing
        Nothing -> Just 0

      rs2Val = case getRs2 instr of
        Just rs2 -> case lookupRF rs2 regFile of
          Ready val -> Just val
          Pending _ -> Nothing
        Nothing -> Just 0

  let rsEntry = ReservationStation {
    rsValid = True,
    rsOp = getArithOp instr,
    rsVj = rs1Val,
    rsVk = rs2Val,
    rsQj = getRs1 instr >>= getSourceStation regFile,
    rsQk = getRs2 instr >>= getSourceStation regFile,
    rsRobSlot = robSlot,
    rsAddress = getMemAddress instr
  }

  if isMemInstr instr
    then allocateToLSQ rsEntry
    else allocateToALU rsEntry

alu :: Bool -> Arith -> Word -> Word -> Word
alu itype op lhs rhs = case op of
  ADD -> lhs + rhs
  SUB -> lhs - rhs
  XOR -> lhs .^. rhs
  OR -> lhs .|. rhs
  AND -> lhs .&. rhs
  SLL -> lhs `shiftL` shiftBits rhs
  SRL -> lhs `shiftR` shiftBits rhs
  SRA -> pack $ sign lhs `shiftR` shiftBits rhs
  SLT -> set $ sign lhs > sign rhs
  SLTU -> set $ lhs > rhs
  where
    shiftBits s
      | itype = fromIntegral $ slice d4 d0 s
      | otherwise = fromIntegral s
    sign = unpack @(Signed 32)
    set b = if b then 1 else 0

branch :: Comparison -> Word -> Word -> Bool
branch op lhs rhs = case op of
  EQ -> lhs == rhs
  NE -> lhs /= rhs
  LT -> sign lhs < sign rhs
  GE -> sign lhs >= sign rhs
  LTU -> lhs < rhs
  GEU -> lhs >= rhs
  where
    sign = unpack @(Signed 32)

isROBFull :: ReorderBuffer -> BitVector 4 -> Bool
isROBFull (ReorderBuffer rob) tailIdx =
  robValid $ rob !! tailIdx

findReadyStation :: KnownNat n => Vec n ReservationStation -> Maybe (Index n)
findReadyStation stations =
  findIndex (\rs -> rsValid rs && isJust (rsVj rs) && isJust (rsVk rs)) stations

getSourceStation :: RegFile -> RegIdx -> Maybe StationId
getSourceStation regFile idx = case lookupRF idx regFile of
  Pending sid -> Just sid
  Ready _ -> Nothing

lookupRF :: RegIdx -> RegFile -> RegFileEntry
lookupRF idx (RegFile rf) = rf !! idx

reserveRF :: RegIdx -> StationId -> RegFile -> RegFile
reserveRF idx sid (RegFile rf) = RegFile $ replace idx (Pending sid) rf

releaseRF :: RegIdx -> Word -> RegFile -> RegFile
releaseRF idx val (RegFile rf) = RegFile $ replace idx (Ready val) rf

isALUInstr :: Instruction -> Bool
isALUInstr (Instruction.RType _ _ _ _) = True
isALUInstr (Instruction.IType (Arith _) _ _ _) = True
isALUInstr _ = False

isMemInstr :: Instruction -> Bool
isMemInstr (Instruction.IType (Load _ _) _ _ _) = True
isMemInstr (Instruction.SType _ _ _ _) = True
isMemInstr _ = False

isBranch :: Instruction -> Bool
isBranch (Instruction.BType _ _ _ _) = True
isBranch (Instruction.JType _ _) = True
isBranch (Instruction.IType Jump _ _ _) = True
isBranch _ = False

getArithOp :: Instruction -> Arith
getArithOp (Instruction.RType op _ _ _) = op
getArithOp (Instruction.IType (Arith op) _ _ _) = op
getArithOp _ = ADD

getMemAddress :: Instruction -> Maybe Address
getMemAddress _ = Nothing

allocateToALU :: ReservationStation -> CPUM ()
allocateToALU rs = do
  stations <- gets stateReservationStations
  case findIndex (not . rsValid) stations of
    Just idx -> do
      let newStations = replace idx rs stations
      modify $ \s -> s { stateReservationStations = newStations }
    Nothing -> pure ()

allocateToLSQ :: ReservationStation -> CPUM ()
allocateToLSQ rs = do
  lsq <- gets stateLoadStoreQueue
  case findIndex (not . rsValid) lsq of
    Just idx -> do
      let newLSQ = replace idx rs lsq
      modify $ \s -> s { stateLoadStoreQueue = newLSQ }
    Nothing -> pure ()

markIssued :: BitVector 4 -> Bool -> CPUM ()
markIssued _ _ = pure ()

clearReservationStation :: BitVector 4 -> Bool -> CPUM ()
clearReservationStation idx isALU = do
  if isALU
    then do
      stations <- gets stateReservationStations
      let newStations = replace (fromIntegral idx) (ReservationStation False ADD Nothing Nothing Nothing Nothing 0 Nothing) stations
      modify $ \s -> s { stateReservationStations = newStations }
    else do
      lsq <- gets stateLoadStoreQueue
      let newLSQ = replace (fromIntegral idx) (ReservationStation False ADD Nothing Nothing Nothing Nothing 0 Nothing) lsq
      modify $ \s -> s { stateLoadStoreQueue = newLSQ }

updateROBResult :: StationId -> Word -> CPUM ()
updateROBResult sid value = do
  rob <- gets stateROB
  let (ReorderBuffer robVec) = rob
      idx = fromIntegral sid
      oldEntry = robVec !! idx
      newEntry = oldEntry { robReady = True, robValue = value }
      newROBVec = replace idx newEntry robVec
  modify $ \s -> s { stateROB = ReorderBuffer newROBVec }

updateWaitingStations :: StationId -> Word -> CPUM ()
updateWaitingStations sid value = do
  stations <- gets stateReservationStations
  let updateStation rs
        | rsQj rs == Just sid = rs { rsQj = Nothing, rsVj = Just value }
        | rsQk rs == Just sid = rs { rsQk = Nothing, rsVk = Just value }
        | otherwise = rs
      newStations = map updateStation stations
  modify $ \s -> s { stateReservationStations = newStations }

flushPipeline :: CPUM ()
flushPipeline = do
  modify $ \s -> s {
    stateReservationStations = repeat (ReservationStation False ADD Nothing Nothing Nothing Nothing 0 Nothing),
    stateLoadStoreQueue = repeat (ReservationStation False ADD Nothing Nothing Nothing Nothing 0 Nothing),
    stateROB = ReorderBuffer $ repeat (ROBEntry False False nop 0 Nothing 0),
    stateROBHead = 0,
    stateROBTail = 0,
    stateRegFile = initRF
  }
readPC :: (MonadWriter Output m) => Address -> m ()
readPC addr =
  tell $
    mempty
      { outMem =
          pure $
            MemAccess
              { memIsInstr = True,
                memAddress = addr,
                memSize = Word,
                memVal = Nothing
              }
      }

setLines :: (MonadState State m) => (Control -> Control) -> m ()
setLines f = modify $ \s -> s {stateCtrl = f $ stateCtrl s}

try :: MaybeT CPUM a -> CPUM ()
try m = void $ runMaybeT m

isBreak :: Instruction -> Bool
isBreak (Instruction.IType Env {} _ _ _) = True
isBreak _ = False

isLoad :: Instruction -> Bool
isLoad (Instruction.IType (Load _ _) _ _ _) = True
isLoad _ = False

