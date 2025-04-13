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

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Core (Input)
import qualified Core
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import qualified Instruction as Core
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type LeakM = RWS Input Out State

data BaseInstr
  = Jump
  | Load RegIdx
  | Store
  | Other
  | Break
  deriving (Show, Eq)

data Instr = Instr
  { instrBase :: BaseInstr,
    instrDeps :: (Maybe RegIdx, Maybe RegIdx)
  }
  deriving (Show, Eq)

isLoad :: Instr -> Bool
isLoad (Instr (Load {}) _) = True
isLoad _ = False

loadHazard :: Instr -> Instr -> Bool
loadHazard (Instr _ (dep1, dep2)) (Instr (Load rd) _) =
  fromMaybe False ((rd ==) <$> dep1)
    || fromMaybe False ((rd ==) <$> dep2)
loadHazard _ _ = False

nop :: Instr
nop = Instr Other (Nothing, Nothing)

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Core.Instruction,
    stateMemInstr :: Core.Instruction,
    stateMemRes :: Word,
    stateMemBranch :: Bool,
    stateWbInstr :: Core.Instruction,
    stateWbRes :: Word,
    stateStallFetch :: Bool,
    stateStallDecode :: Bool,
    stateHalt :: Bool,
    stateMeRegFwd :: Maybe (RegIdx, Word),
    stateWbRegFwd :: Maybe (RegIdx, Word),
    stateJumpAddr :: Maybe Address
  }
  deriving (Show, Eq)

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = Core.nop,
      stateMemInstr = Core.nop,
      stateMemRes = 0,
      stateMemBranch = False,
      stateWbInstr = Core.nop,
      stateWbRes = 0,
      stateHalt = False,
      stateStallFetch = False,
      stateStallDecode = False,
      stateMeRegFwd = Nothing,
      stateWbRegFwd = Nothing,
      stateJumpAddr = Nothing
    }

data Out = Out
  { outInstr :: First Instr,
    outJumpAddr :: First Address
  }

instance Semigroup Out where
  Out i1 a1 <> Out i2 a2 = Out (i1 <> i2) (a1 <> a2)

instance Monoid Out where
  mempty = Out mempty mempty

stallDecode :: LeakM ()
stallDecode = modify $ \s -> s {stateStallDecode = True}

stallFetch :: LeakM ()
stallFetch = modify $ \s -> s {stateStallFetch = True}

outputNothing :: LeakM ()
outputNothing = tell mempty

fetch :: LeakM ()
fetch = do
  ifM
    (gets stateStallFetch)
    ( modify $ \s ->
        s
          { stateFePc = fromMaybe (stateFePc s) (stateJumpAddr s)
          }
    )
    ( modify $ \s ->
        s
          { stateFePc = fromMaybe (stateFePc s + 4) (stateJumpAddr s),
            stateDePc = stateFePc s
          }
    )

decode :: LeakM ()
decode = do
  input <- ask
  let instr
        | Core.inputIsInstr input =
            Core.decode' $ Core.inputMem input
        | otherwise = Core.nop
  ex_ir <- gets stateExInstr
  when (Core.isLoad instr) $
    stallFetch
  tell $
    mempty
      { outInstr =
          pure $
            Instr
              { instrBase = mkInstr instr,
                instrDeps = mkDeps instr
              }
      }

  ifM
    ((Core.loadHazard instr ex_ir ||) <$> gets stateStallDecode)
    ( modify $ \s -> s {stateExInstr = Core.nop}
    )
    ( do
        modify $ \s ->
          s
            { stateExInstr = instr,
              stateExPc = stateDePc s
            }
    )

mkDeps :: Core.Instruction -> (Maybe RegIdx, Maybe RegIdx)
mkDeps instr = (noZero $ Core.getRs1 instr, noZero $ Core.getRs2 instr)
  where
    noZero (Just 0) = Nothing
    noZero r = r

mkInstr :: Core.Instruction -> BaseInstr
mkInstr instr =
  case instr of
    Core.RType {} -> Other
    Core.IType iop rd _ _ ->
      case iop of
        Core.Arith {} ->
          Other
        Core.Load {} ->
          Load rd
        Core.Jump ->
          Jump
        Core.Env Core.Break ->
          Break
        Core.Env Core.Call ->
          Other
    Core.SType {} ->
      Store
    Core.BType {} ->
      Jump
    Core.UType Core.Zero _ _ ->
      Other
    Core.UType Core.PC _ _ -> do
      Other
    Core.JType {} ->
      Jump

execute :: LeakM ()
execute = do
  instr <- gets stateExInstr
  modify $ \s -> s {stateMemBranch = False}
  let r1M :: LeakM Word
      r1M = regWithFwd Core.getRs1 =<< asks Core.inputRs1

      r2M :: LeakM Word
      r2M = regWithFwd Core.getRs2 =<< asks Core.inputRs2

      regWithFwd :: (Core.Instruction -> Maybe RegIdx) -> Word -> LeakM Word
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

  r1 <- r1M
  r2 <- r2M
  pc <- gets stateExPc

  res <-
    case instr of
      Core.RType op rd _ _ ->
        pure $ Core.alu op r1 r2
      Core.IType iop rd _ imm ->
        let op =
              case iop of
                Core.Arith op' -> op'
                _ -> Core.ADD
            alu_res = Core.alu op r1 (signExtend imm)
         in case iop of
              Core.Arith {} -> pure alu_res
              Core.Load size sign -> pure $ bitCoerce alu_res
              Core.Jump -> do
                informJumpAddr $ bitCoerce $ alu_res
                pure $ bitCoerce $ pc + 4
              Core.Env Core.Break ->
                pure $ alu_res
              Core.Env Core.Call ->
                pure alu_res
      Core.SType size imm _ _ -> do
        pure $ unpack (r1 + signExtend imm)
      Core.BType cmp imm _ _ -> do
        let branched = Core.branch cmp r1 r2
        when branched $ do
          modify $ \s -> s {stateMemBranch = True}
          informJumpAddr $
            pc + bitCoerce (signExtend imm)
        pure 0
      Core.UType Core.Zero rd imm ->
        pure $ imm ++# 0 `shiftL` 12
      Core.UType Core.PC rd imm -> do
        let imm' = imm ++# 0 `shiftL` 12
        pure $ bitCoerce pc + imm'
      Core.JType rd imm -> do
        informJumpAddr $ pc + bitCoerce (signExtend imm)
        pure $ bitCoerce $ pc + 4

  modify $ \s ->
    s
      { stateMemInstr = instr,
        stateMemRes = res
      }
  where
    informJumpAddr :: Address -> LeakM ()
    informJumpAddr jump_addr = do
      stallDecode
      tell $ mempty {outJumpAddr = pure jump_addr}
      modify $ \s -> s {stateJumpAddr = pure jump_addr}

    hazardRW :: (Core.Instruction -> Maybe RegIdx) -> Core.Instruction -> RegIdx -> Bool
    hazardRW getR src rd = isJust $ do
      rs <- getR src
      guard $ rd /= 0 && rs == rd

memory :: LeakM ()
memory = do
  instr <- gets stateMemInstr
  res <- gets stateMemRes

  try $ do
    rd <- MaybeT $ pure $ Core.getRd instr
    lift $ modify $ \s -> s {stateMeRegFwd = pure (rd, res)}

  case instr of
    Core.IType Core.Load {} _ _ _ -> do
      modify $ \s -> s {stateMeRegFwd = Nothing}
      stallFetch
    Core.IType Core.Jump _ _ _ ->
      stallDecode
    Core.JType {} ->
      stallDecode
    Core.BType {} -> do
      branched <- gets stateMemBranch
      when branched $
        stallDecode
    Core.SType {} ->
      stallFetch
    _ -> pure ()

  modify $ \s ->
    s
      { stateWbInstr = instr,
        -- No longer care about the branches (matters for state equivalence)
        -- if isBranch instr
        --  then Core.nop
        --  else instr,
        stateWbRes = res
      }
  where
    isBranch (Core.BType {}) = True
    isBranch _ = False

writeback :: LeakM ()
writeback = do
  input <- asks Core.inputMem
  instr <- gets stateWbInstr
  stateHalted <- gets stateHalt
  res <- gets stateWbRes

  when
    stateHalted
    outputNothing

  when (Core.isBreak instr) $ do
    modify $ \s ->
      s
        { stateMemInstr = Core.nop,
          stateExInstr = Core.nop,
          stateHalt = True
        }
    outputNothing

  try $ do
    rd <- MaybeT $ pure $ Core.getRd instr
    lift $ modify $ \s -> s {stateWbRegFwd = pure (rd, res)}

  case instr of
    Core.IType (Core.Load size sign) rd _ _ -> do
      stallDecode
      let val = Core.loadExtend size sign input
      modify $ \s -> s {stateWbRegFwd = pure (rd, val)}
    Core.SType {} ->
      stallDecode
    _ -> pure ()

pipe :: LeakM ()
pipe = do
  resetCtrl
  writeback
  memory
  execute
  decode
  fetch
  where
    resetCtrl :: LeakM ()
    resetCtrl =
      modify $ \s ->
        s
          { stateStallFetch = False,
            stateStallDecode = False,
            stateMeRegFwd = Nothing,
            stateWbRegFwd = Nothing,
            stateJumpAddr = Nothing
          }

circuit :: State -> Input -> (State, Out)
circuit = flip $ execRWS pipe
