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
import Leak.PC.Time (Stage (..))
import qualified Leak.PC.Time as Time
import Types
import Util
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (||))

data State = State
  { stateFePc :: Address,
    stateDePc :: Address,
    stateExPc :: Address,
    stateExInstr :: Time.Instr,
    stateMemInstr :: Time.Instr,
    stateWbInstr :: Time.Instr,
    stateJumpAddr :: Maybe Address,
    stateStall :: Set Stage,
    stateHalt :: Bool
  }
  deriving (Show)

init :: State
init =
  State
    { stateFePc = initPc,
      stateDePc = 0,
      stateExPc = 0,
      stateExInstr = Time.nop,
      stateMemInstr = Time.nop,
      stateWbInstr = Time.nop,
      stateHalt = False,
      stateStall = mempty,
      stateJumpAddr = Nothing
    }

type SimM = RWS Time.Out (First (Maybe Address)) State

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
  instr <- fromMaybe Time.nop . getFirst <$> asks Time.outInstr
  when (isLoad instr) $ do
    stall Fe
  ex_ir <- gets stateExInstr
  when (loadHazard instr ex_ir) $
    stall De

  modify $ \s -> s {stateExPc = stateDePc s}
  ifStalling
    De
    (modify $ \s -> s {stateExInstr = Time.nop})
    (modify $ \s -> s {stateExInstr = instr})
  where
    isLoad :: Time.Instr -> Bool
    isLoad (Time.Instr (Time.Load {}) _) = True
    isLoad _ = False

    loadHazard :: Time.Instr -> Time.Instr -> Bool
    loadHazard de_ir (Time.Instr (Time.Load rd) _) =
      elem rd $ S.toList $ Time.instrDeps de_ir
    loadHazard _ _ = False

execute :: SimM ()
execute = do
  instr <- gets stateExInstr
  mjmpAddr <- getFirst <$> asks Time.outJumpAddr
  modify $ \s ->
    s
      { stateJumpAddr = mjmpAddr,
        stateMemInstr = instr
      }

  case Time.instrBase instr of
    Time.Jump -> do
      case mjmpAddr of
        Just {} -> do
          stall De
          modify $ \s ->
            s
              { stateMemInstr = instr {Time.instrBase = Time.Jump}
              }
        Nothing ->
          modify $ \s ->
            s
              { stateMemInstr = instr {Time.instrBase = Time.Other}
              }
    _ -> pure ()

memory :: SimM ()
memory = do
  instr <- gets stateMemInstr
  case Time.instrBase instr of
    Time.Load {} -> do
      outputNothing
      stall Fe
    Time.Store -> do
      outputNothing
      stall Fe
    Time.Jump ->
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

  case Time.instrBase instr of
    Time.Break -> do
      outputNothing
      modify $ \s ->
        s
          { stateMemInstr = Time.nop,
            stateExInstr = Time.nop,
            stateHalt = True
          }
    Time.Load {} -> do
      stall De
    Time.Store -> do
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

circuit :: State -> Time.Out -> (State, Maybe Address)
circuit s i = fromMaybe Nothing . getFirst <$> execRWS pipe i s
