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
import Data.Functor.Identity
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import qualified Instruction as Core
import Interp
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type LeakM = RWS (Input Identity) Out State

data BaseInstr
  = Jump
  | Load RegIdx
  | Store
  | Other
  | Call
  | Break
  deriving (Show, Eq, Generic)

data Instr = Instr
  { instrBase :: BaseInstr,
    instrDeps :: (Maybe RegIdx, Maybe RegIdx)
  }
  deriving (Show, Eq, Generic)

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
    stateWbInstr :: Core.Instruction,
    stateWbRes :: Word,
    stateStallFetch :: Bool,
    stateStallDecode :: Bool,
    stateHalt :: Bool,
    stateMeRegFwd :: Maybe (RegIdx, Word),
    stateWbRegFwd :: Maybe (RegIdx, Word),
    stateJumpAddr :: Maybe Address,
    stateFirstCycle :: Bool
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
      stateWbInstr = Core.nop,
      stateWbRes = 0,
      stateHalt = False,
      stateStallFetch = False,
      stateStallDecode = False,
      stateMeRegFwd = Nothing,
      stateWbRegFwd = Nothing,
      stateJumpAddr = Nothing,
      stateFirstCycle = True
    }

data Out = Out
  { outInstr :: First Instr,
    outJumpAddr :: First Address
  } deriving (Show, Eq, Generic)

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
            Core.decode' $ runIdentity $ Core.inputMem input
        | otherwise = Core.nop
  ex_ir <- gets stateExInstr
  when (Core.isLoad instr || Core.isCall instr) $
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
    ( pure (\a b c -> a || b || c)
        <*> pure (Core.isLoad ex_ir || Core.isCall ex_ir)
        <*> gets stateStallDecode
        <*> gets stateFirstCycle
    )
    ( modify $ \s ->
        s
          { stateExInstr = Core.nop,
            stateExPc = stateDePc s
          }
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
          Call
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
  let r1M :: LeakM Word
      r1M = regWithFwd Core.getRs1 =<< (runIdentity <$> asks Core.inputRs1)

      r2M :: LeakM Word
      r2M = regWithFwd Core.getRs2 =<< (runIdentity <$> asks Core.inputRs2)

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

  interp_res <- interp instr <$> (Identity <$> r1M) <*> (Identity <$> r2M) <*> gets stateExPc

  modify $ \s -> s {stateMemInstr = instr}

  case instr of
    Core.IType Core.Jump _ _ _ ->
      case interp_res of
        Interp _ (Just addr) _ ->
          informJumpAddr addr
        _ -> pure ()
    Core.BType {} ->
      case interp_res of
        Interp _ (Just addr) (Just branched)
          | branched ->
              informJumpAddr addr
        Interp _ _ (Just branched)
          | not branched ->
              modify $ \s -> s {stateMemInstr = Core.nop}
        _ -> pure ()
    Core.JType {} ->
      case interp_res of
        Interp _ (Just addr) _ ->
          informJumpAddr addr
        _ -> pure ()
    _ -> pure ()

  modify $ \s -> s {stateMemRes = interpRes interp_res}
  where
    informJumpAddr :: Address -> LeakM ()
    informJumpAddr jump_addr = do
      stallFetch
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
    Core.SType {} ->
      stallFetch
    _ -> pure ()

  modify $ \s ->
    s
      { stateWbInstr = instr,
        stateWbRes = res
      }

writeback :: LeakM ()
writeback = do
  input <- runIdentity <$> asks Core.inputMem
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
    Core.IType (Core.Env Core.Call) _ _ _ -> do
      stallDecode
      let rd = 10
      let val = input
      modify $ \s -> s {stateWbRegFwd = pure (rd, val)}
    Core.SType {} ->
      stallDecode
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
          { stateStallFetch = False,
            stateStallDecode = False,
            stateMeRegFwd = Nothing,
            stateWbRegFwd = Nothing,
            stateJumpAddr = Nothing,
            stateFirstCycle = firstCycle
          }
      void m
      modify $ \s -> s {stateFirstCycle = False}

circuit :: State -> Input Identity -> (State, Out)
circuit = flip $ execRWS pipe
