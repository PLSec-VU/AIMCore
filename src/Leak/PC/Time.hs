module Leak.PC.Time
  ( pipe,
    circuit,
    init,
    nop,
    isLoad,
    loadHazard,
    mkInstr,
    Stage (..),
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
import Data.Set (Set)
import qualified Data.Set as S
import qualified ISA
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

type TimeM = RWS (ISA.Instr ISA.Func, Input) Out State

data BaseInstr
  = Jump
  | Load RegIdx
  | Store
  | Other
  | Break
  deriving (Show, Eq)

data Instr = Instr
  { instrBase :: BaseInstr,
    instrDeps :: Set RegIdx
  }
  deriving (Show, Eq)

isLoad :: Instr -> Bool
isLoad (Instr (Load {}) _) = True
isLoad _ = False

loadHazard :: Instr -> Instr -> Bool
loadHazard de_ir (Instr (Load rd) _) =
  elem rd $ S.toList $ instrDeps de_ir
loadHazard _ _ = False

nop :: Instr
nop = Instr Other mempty

data Stage = Fe | De | Ex | Mem | Wb
  deriving (Show, Eq, Ord)

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: ISA.Instr ISA.Func,
    stateMemInstr :: ISA.Instr ISA.Done,
    stateWbInstr :: ISA.Instr ISA.Done,
    stateStall :: Set Stage,
    stateHalt :: Bool,
    stateMeRegFwd :: Maybe (RegIdx, Word),
    stateWbRegFwd :: Maybe (RegIdx, Word),
    stateJumpAddr :: Maybe Address
  }
  deriving (Show)

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = ISA.Nop,
      stateMemInstr = ISA.Nop,
      stateWbInstr = ISA.Nop,
      stateHalt = False,
      stateStall = mempty,
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

ifStalling :: Stage -> TimeM a -> TimeM a -> TimeM a
ifStalling stage = ifM $ gets $ S.member stage . stateStall

stall :: Stage -> TimeM ()
stall stage =
  modify $ \s -> s {stateStall = stage `S.insert` stateStall s}

outputNothing :: TimeM ()
outputNothing = tell mempty

fetch :: TimeM ()
fetch = do
  ifStalling
    Fe
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

decode :: TimeM ()
decode = do
  instr <- fst <$> ask
  ex_ir <- gets stateExInstr
  when (ISA.isLoad instr) $
    stall Fe
  when (ISA.loadHazard instr ex_ir) $
    stall De
  ifStalling
    De
    ( modify $ \s -> s {stateExInstr = ISA.Nop}
    )
    ( do
        modify $ \s ->
          s
            { stateExInstr = instr,
              stateExPc = stateDePc s
            }
        tell $
          mempty
            { outInstr =
                pure $
                  Instr
                    { instrBase = mkInstr instr,
                      instrDeps = ISA.depSet instr
                    }
            }
    )

mkInstr :: ISA.Instr a -> BaseInstr
mkInstr ISA.Reg {} = Other
mkInstr (ISA.Load _ rd _) = Load rd
mkInstr ISA.Jump {} = Jump
mkInstr ISA.Store {} = Store
mkInstr ISA.Branch {} = Jump
mkInstr ISA.Break = Break
mkInstr ISA.Nop = Other

execute :: TimeM ()
execute = do
  instr <- gets stateExInstr
  r1 <- r1M
  r2 <- r2M
  pc <- gets stateExPc
  let apply f = ISA.apply f r1 r2 pc

  instr' <-
    case instr of
      ISA.Reg rd f ->
        pure $ ISA.Reg rd $ apply f
      ISA.Load size rd f ->
        pure $ ISA.Load size rd $ apply f
      ISA.Jump rd f_pc f_jump_addr -> do
        let jump_addr = apply f_jump_addr
        informJumpAddr jump_addr
        pure $ ISA.Jump rd (apply f_pc) jump_addr
      ISA.Store size f_addr r ->
        pure $ ISA.Store size (apply f_addr) r
      ISA.Branch f_branched f_jump_addr -> do
        let branched = apply f_branched
            jump_addr = apply f_jump_addr
        when (ISA.unDone branched) $
          informJumpAddr jump_addr
        pure $ ISA.Branch branched jump_addr
      ISA.Break -> pure ISA.Break
      ISA.Nop -> pure ISA.Nop

  modify $ \s -> s {stateMemInstr = instr'}
  where
    informJumpAddr :: ISA.Done Address -> TimeM ()
    informJumpAddr jump_addr = do
      stall De
      tell $ mempty {outJumpAddr = pure $ ISA.unDone jump_addr}
      modify $ \s -> s {stateJumpAddr = pure $ ISA.unDone jump_addr}

    r1M :: TimeM Word
    r1M = regWithFwd ISA.getR1 =<< asks (Core.inputRs1 . snd)

    r2M :: TimeM Word
    r2M = regWithFwd ISA.getR2 =<< asks (Core.inputRs2 . snd)

    regWithFwd :: (ISA.Instr ISA.Func -> Maybe RegIdx) -> Word -> TimeM Word
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

    hazardRW :: (ISA.Instr ISA.Func -> Maybe RegIdx) -> ISA.Instr ISA.Func -> RegIdx -> Bool
    hazardRW getR src rd = isJust $ do
      rs <- getR src
      guard $ rd /= 0 && rs == rd

memory :: TimeM ()
memory = do
  instr <- gets stateMemInstr

  mres <-
    case instr of
      ISA.Reg _ (ISA.Done res) ->
        pure $ Just res
      ISA.Load {} -> do
        stall Fe
        pure Nothing
      ISA.Jump _ (ISA.Done addr) _ -> do
        stall De
        pure $ Just $ bitCoerce addr
      ISA.Store {} -> do
        stall Fe
        pure Nothing
      ISA.Branch (ISA.Done branched) _ -> do
        when branched $
          stall De
        pure Nothing
      _ -> pure Nothing

  try $ do
    rd <- MaybeT $ pure $ ISA.getRd instr
    res <- MaybeT $ pure mres
    lift $ modify $ \s -> s {stateMeRegFwd = pure (rd, res)}

  modify $ \s -> s {stateWbInstr = stateMemInstr s}

writeback :: TimeM ()
writeback = do
  input <- asks $ Core.inputMem . snd
  instr <- gets stateWbInstr
  stateHalted <- gets stateHalt

  when
    stateHalted
    outputNothing

  case instr of
    ISA.Load {} -> stall De
    ISA.Store {} -> stall De
    _ -> pure ()

  mres <-
    case instr of
      ISA.Reg _ (ISA.Done res) ->
        pure $ Just res
      ISA.Load {} -> do
        stall De
        pure $ Just input
      ISA.Jump _ (ISA.Done addr) _ ->
        pure $ Just $ bitCoerce addr
      ISA.Store {} -> do
        stall De
        pure Nothing
      ISA.Break -> do
        modify $ \s ->
          s
            { stateMemInstr = ISA.Nop,
              stateExInstr = ISA.Nop,
              stateHalt = True
            }
        outputNothing
        pure Nothing
      _ -> pure Nothing

  try $ do
    rd <- MaybeT $ pure $ ISA.getRd instr
    res <- MaybeT $ pure mres
    lift $ modify $ \s -> s {stateWbRegFwd = pure (rd, res)}

pipe :: TimeM ()
pipe = do
  resetCtrl
  writeback
  memory
  execute
  decode
  fetch
  where
    resetCtrl :: TimeM ()
    resetCtrl =
      modify $ \s ->
        s
          { stateStall = mempty,
            stateMeRegFwd = Nothing,
            stateWbRegFwd = Nothing,
            stateJumpAddr = Nothing
          }

circuit :: State -> (ISA.Instr ISA.Func, Input) -> (State, Out)
circuit s i = execRWS pipe i s
