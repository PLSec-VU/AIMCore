module Leak.PC.Leak
  ( pipe,
    circuit,
    init,
    nop,
    isLoad,
    loadHazard,
    mkInstr,
    mkDeps,
    Instr (..),
    BaseInstr (..),
    State (..),
    Out (..),
  )
where

import Access
import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Core (Input, HaltState (..))
import qualified Core
import Data.Functor.Identity
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import qualified Instruction as Instr
import Interp
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type LeakM = RWS (Input Identity) Out State

data BaseInstr
  = Jump
  | Branch
  | Load RegIdx
  | Store
  | Other
  | Call
  | Break
  | Nop Instr.Reason4Stall
  deriving (Show, Eq, Generic)

data Instr = Instr
  { instrBase :: BaseInstr,
    instrDeps :: (Maybe RegIdx, Maybe RegIdx)
  }
  deriving (Show, Eq, Generic)

isLoad :: Instr -> Bool
isLoad (Instr (Load {}) _) = True
isLoad (Instr Call _) = True -- syscalls behaves like load to a0
isLoad _ = False

loadHazard :: Instr -> Instr -> Bool
loadHazard (Instr _ (dep1, dep2)) (Instr (Load rd) _) =
  fromMaybe False ((rd ==) <$> dep1)
    || fromMaybe False ((rd ==) <$> dep2)
loadHazard _ _ = False

nop :: Instr
nop = Instr (Nop Instr.FirstCycle) (Nothing, Nothing)

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Instr.Instruction,
    stateMemInstr :: Instr.Instruction,
    stateMemRes :: Word,
    stateMemVal :: Word,
    stateWbInstr :: Instr.Instruction,
    stateWbRes :: Word,
    stateMeMemInstr :: Bool,
    stateHalt :: HaltState,
    stateMeRegFwd :: Maybe (RegIdx, Word),
    stateWbRegFwd :: Maybe (RegIdx, Word),
    stateJumpAddr :: Maybe Address,
    stateDeLoadHazard :: Maybe Address,
    stateDeCall :: Bool,
    stateFirstCycle :: Bool
  }
  deriving (Show, Eq)

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = Instr.Nop Instr.FirstCycle,
      stateMemInstr = Instr.Nop Instr.FirstCycle,
      stateMemRes = 0,
      stateMemVal = 0,
      stateWbInstr = Instr.Nop Instr.FirstCycle,
      stateWbRes = 0,
      stateMeMemInstr = False,
      stateHalt = Running,
      stateMeRegFwd = Nothing,
      stateWbRegFwd = Nothing,
      stateJumpAddr = Nothing,
      stateDeLoadHazard = Nothing,
      stateDeCall = False,
      stateFirstCycle = True
    }

data Out = Out
  { outInstr :: First Instr,
    outJumpAddr :: First Address,
    outRs1 :: First RegIdx,
    outRs2 :: First RegIdx,
    outMeMemInstr :: First Bool,
    outHalt :: First Bool,
    outBranchTaken :: First Bool,
    outJumpAddrValid :: First Bool
  }
  deriving (Show, Eq, Generic)

instance Semigroup Out where
  Out i1 a1 r1 r2 m1 h1 b1 v1 <> Out i2 a2 r1' r2' m2 h2 b2 v2 =
    Out (i1 <> i2) (a1 <> a2) (r1 <> r1') (r2 <> r2') (m1 <> m2) (h1 <> h2) (b1 <> b2) (v1 <> v2)

instance Monoid Out where
  mempty = Out mempty mempty mempty mempty mempty mempty mempty mempty

setMeMemInstr :: LeakM ()
setMeMemInstr = do
  modify $ \s -> s {stateMeMemInstr = True}
  tell $ mempty {outMeMemInstr = pure True}

outputNothing :: LeakM ()
outputNothing = tell mempty

fetch :: LeakM ()
fetch = do
  pc <- gets stateFePc
  mJumpAddr <- gets stateJumpAddr
  deLoadHazard <- gets stateDeLoadHazard
  deCall <- gets stateDeCall
  meMemInstr <- gets stateMeMemInstr

  let stall = deCall || meMemInstr

  let next_pc =
        fromMaybe
          (fromMaybe
             (if stall then pc else pc + 4)
             deLoadHazard)
          mJumpAddr

  modify $ \s ->
    s
      { stateFePc = next_pc,
        stateDePc = pc
      }

decode :: LeakM ()
decode = do
  input <- ask
  let instr
        | Core.inputIsInstr input =
            Instr.decode' $ runIdentity $ Core.inputMem input
        | otherwise = Instr.Nop Instr.MemoryBusBusy

  exInstr <- gets stateExInstr
  mJumpAddr <- gets stateJumpAddr
  firstCycle <- gets stateFirstCycle

  let branch_first_cycle = Instr.isNopBranchFirstCycle exInstr
  let load_hazard_current_cycle = Instr.loadHazard instr exInstr
  let load_hazard_first_cycle = Instr.isNopLoadHazardFirstCycle exInstr
  let call_current_cycle = Instr.isCall exInstr

  let isSecretInstr = case fromPublic (Core.inputMem input) of Nothing -> True; _ -> False

  let ir' =
        -- If a branch was taken in this cycle, we stall.
        if isJust mJumpAddr then Instr.Nop Instr.BranchFirstCycle
        -- If a branch was taken in the previous cycle, we stall.
        else if branch_first_cycle then Instr.Nop Instr.BranchSecondCycle
        -- If there is a load hazard with the instruction executed in this cycle, we stall.
        else if load_hazard_current_cycle then Instr.Nop Instr.LoadHazardFirstCycle
        -- If there was a load hazard in the previous cycle, we stall.
        else if load_hazard_first_cycle then Instr.Nop Instr.LoadHazardSecondCycle
        -- If a syscall is executed in this cycle, we stall.
        else if call_current_cycle then Instr.Nop Instr.SyscallFirstCycle
        -- If this is the first cycle, the instruction to decode is gibberish from memory.
        else if firstCycle then Instr.Nop Instr.FirstCycle
        -- If memory is busy, we stall.
        else if not (Core.inputIsInstr input) then Instr.Nop Instr.MemoryBusBusy
        -- If we are in SecurityViolation state, we stall.
        else if isSecretInstr then Instr.Nop Instr.SecurityViolation
        -- Otherwise we process the decoded instruction.
        else instr

  when isSecretInstr $ modify $ \s -> s {stateHalt = SecurityViolation}

  let rs1Idx = fromMaybe 0 $ Instr.getRs1 ir'
  let rs2Idx = fromMaybe 0 $ Instr.getRs2 ir'

  tell $
    mempty
      { outInstr =
          pure $
            Instr
              { instrBase = mkInstr ir',
                instrDeps = mkDeps ir'
              },
        outRs1 = pure rs1Idx,
        outRs2 = pure rs2Idx
      }

  when load_hazard_current_cycle $ do
    pc <- gets stateDePc
    modify $ \s -> s {stateDeLoadHazard = Just pc}

  when (Instr.isCall ir') $
    modify $ \s -> s {stateDeCall = True}

  modify $ \s ->
    s
      { stateExInstr = ir',
        stateExPc = stateDePc s
      }

mkDeps :: Instr.Instruction -> (Maybe RegIdx, Maybe RegIdx)
mkDeps instr = (noZero $ Instr.getRs1 instr, noZero $ Instr.getRs2 instr)
  where
    noZero (Just 0) = Nothing
    noZero r = r

mkInstr :: Instr.Instruction -> BaseInstr
mkInstr instr
  | instr == Instr.nop = Nop Instr.FirstCycle
  | otherwise = case instr of
    Instr.RType {} -> Other
    Instr.IType iop rd _ _ ->
      case iop of
        Instr.Arith {} ->
          Other
        Instr.Load {} ->
          Load rd
        Instr.Jump ->
          Jump
        Instr.Env Instr.Break ->
          Break
        Instr.Env Instr.Call ->
          Call
    Instr.SType {} ->
      Store
    Instr.BType {} ->
      Branch
    Instr.UType Instr.Zero _ _ ->
      Other
    Instr.UType Instr.PC _ _ -> do
      Other
    Instr.JType {} ->
      Jump
    Instr.Nop r ->
      Nop r

execute :: LeakM ()
execute = do
  instr <- gets stateExInstr
  let r1M :: LeakM (Identity Word)
      r1M = Identity <$> (regWithFwd Instr.getRs1 =<< (runIdentity <$> asks Core.inputRs1))

      r2M :: LeakM (Identity Word)
      r2M = Identity <$> (regWithFwd Instr.getRs2 =<< (runIdentity <$> asks Core.inputRs2))

      regWithFwd :: (Instr.Instruction -> Maybe RegIdx) -> Word -> LeakM Word
      regWithFwd getR def = do
        ir <- gets stateExInstr
        let checkForFwd line = do
              (fwdIdx, fwdVal) <- MaybeT $ gets line
              guard (hazardRW getR ir fwdIdx)
              pure fwdVal
        fmap
          (fromMaybe def)
          $ runMaybeT
          $ checkForFwd stateMeRegFwd <|> checkForFwd stateWbRegFwd

  interp_res <- interp instr <$> r1M <*> r2M <*> gets stateExPc

  modify $ \s -> s {stateMemInstr = instr, stateMemVal = 0}

  case instr of
    Instr.IType Instr.Jump _ _ _ ->
      case fromPublic (interpAddr interp_res) of
        Just (Just addr) -> do
          informJumpAddr addr
          tell $ mempty { outJumpAddrValid = pure True }
        _ -> unless (isPublic (interpAddr interp_res)) $
               modify $ \s -> s {stateHalt = SecurityViolation}
    Instr.BType {} ->
      case (fromPublic (interpAddr interp_res), interpBranched interp_res) of
        (Just (Just addr), Just branched) ->
          case fromPublic branched of
            Just True -> do
              informJumpAddr addr
              tell $ mempty { outBranchTaken = pure True }
            Just False -> tell $ mempty { outBranchTaken = pure False }
            Nothing -> modify $ \s -> s {stateHalt = SecurityViolation}
        _ -> unless (isPublic (interpAddr interp_res)) $
               modify $ \s -> s {stateHalt = SecurityViolation}
    Instr.JType _ _ ->
      case fromPublic (interpAddr interp_res) of
        Just (Just addr) -> do
          informJumpAddr addr
          tell $ mempty { outJumpAddrValid = pure True }
        _ -> unless (isPublic (interpAddr interp_res)) $
               modify $ \s -> s {stateHalt = SecurityViolation}
    Instr.SType {} -> do
      r2Val <- unAccess <$> r2M
      modify $ \s -> s { stateMemVal = r2Val }
    _ -> pure ()

  modify $ \s -> s {stateMemRes = runIdentity $ interpRes interp_res}
  where
    informJumpAddr :: Address -> LeakM ()
    informJumpAddr jump_addr = do
      tell $ mempty {outJumpAddr = pure jump_addr}
      modify $ \s -> s {stateJumpAddr = pure jump_addr}

    hazardRW :: (Instr.Instruction -> Maybe RegIdx) -> Instr.Instruction -> RegIdx -> Bool
    hazardRW getR src rd = isJust $ do
      rs <- getR src
      guard $ rd /= 0 && rs == rd

memory :: LeakM ()
memory = do
  instr <- gets stateMemInstr
  res <- gets stateMemRes

  let shouldForward = case instr of
        Instr.RType {} -> True
        Instr.IType (Instr.Arith _) _ _ _ -> True
        Instr.JType {} -> True
        Instr.IType Instr.Jump _ _ _ -> True
        Instr.UType {} -> True
        _ -> False

  when shouldForward $ try $ do
    rd <- MaybeT $ pure $ Instr.getRd instr
    lift $ modify $ \s -> s {stateMeRegFwd = pure (rd, res)}

  let isSecretAddr = case fromPublic (Identity res) of Nothing -> True; _ -> False

  case instr of
    Instr.IType Instr.Load {} _ _ _ -> do
      modify $ \s -> s {stateMeRegFwd = Nothing}
      if isSecretAddr
        then do
          modify $ \s -> s {stateHalt = SecurityViolation}
          tell $ mempty {outMeMemInstr = pure False}
        else setMeMemInstr
    Instr.IType (Instr.Env Instr.Call) _ _ _ -> do
      modify $ \s -> s {stateMeRegFwd = Nothing}
      setMeMemInstr
    Instr.SType {} -> do
      if isSecretAddr
        then do
          modify $ \s -> s {stateHalt = SecurityViolation}
          tell $ mempty {outMeMemInstr = pure False}
        else setMeMemInstr
    _ -> pure ()

  modify $ \s ->
    s
      { stateWbInstr = instr,
        stateWbRes = res
      }

writeback :: LeakM ()
writeback = do
  input <- asks Core.inputMem
  instr <- gets stateWbInstr
  stateHalted <- gets stateHalt
  res <- gets stateWbRes

  when (stateHalted /= Running) $ do
    outputNothing
    tell $ mempty { outHalt = pure True }

  when (Instr.isBreak instr) $ do
    modify $ \s ->
      s
        { stateMemInstr = Instr.nop,
          stateExInstr = Instr.nop,
          stateHalt = EBreak
        }
    outputNothing

  let shouldForward = case instr of
        Instr.RType {} -> True
        Instr.IType (Instr.Arith _) _ _ _ -> True
        Instr.IType (Instr.Load {}) _ _ _ -> True
        Instr.JType {} -> True
        Instr.IType Instr.Jump _ _ _ -> True
        Instr.UType {} -> True
        _ -> False

  when shouldForward $ try $ do
    rd <- MaybeT $ pure $ Instr.getRd instr
    lift $ modify $ \s -> s {stateWbRegFwd = pure (rd, res)}

  case instr of
    Instr.IType (Instr.Load size sign) rd _ _ -> do
      let val = Instr.loadExtend size sign (unAccess input)
      modify $ \s -> s {stateWbRegFwd = pure (rd, val)}
    Instr.IType (Instr.Env Instr.Call) _ _ _ -> do
      modify $ \s -> s {stateWbRegFwd = pure (10, unAccess input)}
    _ -> pure ()

pipe :: LeakM ()
pipe = withCtrlReset $ do
  writeback
  memory
  execute
  decode
  fetch
  where
    withCtrlReset m = do
      firstCycle <- gets stateFirstCycle
      modify $ \s ->
        s
          { stateFirstCycle = firstCycle,
            stateJumpAddr = Nothing,
            stateDeLoadHazard = Nothing,
            stateDeCall = False,
            stateMeMemInstr = False,
            stateMeRegFwd = Nothing,
            stateWbRegFwd = Nothing
          }
      void m
      modify $ \s -> s {stateFirstCycle = False}

circuit :: State -> Input Identity -> (State, Out)
circuit = flip $ execRWS pipe
