module Leak.PC.Sim
  ( pipe,
    circuit,
    init,
    State (..),
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.RWS
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Leak.PC.Leak (Stage (..))
import qualified Leak.PC.Leak as Leak
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Leak.Instr,
    stateMemInstr :: Leak.Instr,
    stateWbInstr :: Leak.Instr,
    stateJumpAddr :: Maybe Address,
    stateStall :: Set Stage,
    stateHalt :: Bool
  }
  deriving (Show, Eq)

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = Leak.nop,
      stateMemInstr = Leak.nop,
      stateWbInstr = Leak.nop,
      stateHalt = False,
      stateStall = mempty,
      stateJumpAddr = Nothing
    }

type SimM = RWS Leak.Out (First (Maybe Address)) State

ifStalling :: Stage -> SimM a -> SimM a -> SimM a
ifStalling stage = ifM $ gets $ S.member stage . stateStall

stall :: Stage -> SimM ()
stall stage =
  modify $ \s -> s {stateStall = stage `S.insert` stateStall s}

outputPc :: Address -> SimM ()
outputPc addr =
  tell $ pure $ pure addr

outputNothing :: SimM ()
outputNothing = tell $ pure Nothing

fetch :: SimM ()
fetch = do
  outputPc =<< gets stateFePc
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

decode :: SimM ()
decode = do
  instr <- fromMaybe Leak.nop . getFirst <$> asks Leak.outInstr
  when (Leak.isLoad instr) $ do
    stall Fe
  ex_ir <- gets stateExInstr
  when (Leak.loadHazard instr ex_ir) $
    stall De

  modify $ \s -> s {stateExPc = stateDePc s}
  ifStalling
    De
    (modify $ \s -> s {stateExInstr = Leak.nop})
    (modify $ \s -> s {stateExInstr = instr})

execute :: SimM ()
execute = do
  instr <- gets stateExInstr
  mjmpAddr <- getFirst <$> asks Leak.outJumpAddr
  modify $ \s ->
    s
      { stateJumpAddr = mjmpAddr,
        stateMemInstr = instr
      }

  case Leak.instrBase instr of
    Leak.Jump -> do
      case mjmpAddr of
        Just {} -> do
          stall De
          modify $ \s ->
            s
              { stateMemInstr = instr {Leak.instrBase = Leak.Jump}
              }
        Nothing ->
          modify $ \s ->
            s
              { stateMemInstr = instr {Leak.instrBase = Leak.Other}
              }
    _ -> pure ()

memory :: SimM ()
memory = do
  instr <- gets stateMemInstr
  case Leak.instrBase instr of
    Leak.Load {} -> do
      outputNothing
      stall Fe
    Leak.Store -> do
      outputNothing
      stall Fe
    Leak.Jump ->
      stall De
    _ -> pure ()

  modify $ \s -> s {stateWbInstr = stateMemInstr s}

writeback :: SimM ()
writeback = do
  instr <- gets stateWbInstr
  halted <- gets stateHalt

  when
    halted
    outputNothing

  case Leak.instrBase instr of
    Leak.Break -> do
      outputNothing
      modify $ \s ->
        s
          { stateMemInstr = Leak.nop,
            stateExInstr = Leak.nop,
            stateHalt = True
          }
    Leak.Load {} -> do
      stall De
    Leak.Store -> do
      stall De
    _ -> pure ()

pipe :: SimM ()
pipe = do
  resetCtrl
  writeback
  memory
  execute
  decode
  fetch
  where
    resetCtrl :: SimM ()
    resetCtrl =
      modify $ \s ->
        s
          { stateStall = mempty,
            stateJumpAddr = Nothing
          }

circuit :: State -> Leak.Out -> (State, Maybe Address)
circuit s i = fromMaybe Nothing . getFirst <$> execRWS pipe i s
