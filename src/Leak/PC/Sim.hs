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
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
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
    stateStallFetch :: Bool,
    stateStallDecode :: Bool,
    stateHalt :: Bool,
    stateFirstCycle :: Bool
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
      stateStallFetch = False,
      stateStallDecode = False,
      stateJumpAddr = Nothing,
      stateFirstCycle = True
    }

type SimM = RWS Leak.Out (First (Maybe Address)) State

stallFetch :: SimM ()
stallFetch = modify $ \s -> s {stateStallFetch = True}

stallDecode :: SimM ()
stallDecode = modify $ \s -> s {stateStallDecode = True}

outputPc :: Address -> SimM ()
outputPc addr =
  tell $ pure $ pure addr

outputNothing :: SimM ()
outputNothing = tell $ pure Nothing

fetch :: SimM ()
fetch = do
  targetPc <- gets (\s -> fromMaybe (stateFePc s) (stateJumpAddr s))
  outputPc targetPc

  ifM
    (gets stateStallFetch)
    ( modify $ \s ->
        s
          { stateFePc = targetPc,
            stateDePc = stateFePc s
          }
    )
    ( modify $ \s ->
        s
          { stateFePc = targetPc + 4,
            stateDePc = stateFePc s
          }
    )

decode :: SimM ()
decode = do
  instr <- fromMaybe Leak.nop . getFirst <$> asks Leak.outInstr
  when (Leak.isLoad instr) $ do
    stallFetch
  ifM
    ( pure (||)
        <*> gets stateStallDecode
        <*> gets stateFirstCycle
    )
    ( modify $ \s ->
        s
          { stateExInstr = Leak.nop,
            stateExPc = stateDePc s
          }
    )
    ( modify $ \s ->
        s
          { stateExInstr = instr,
            stateExPc = stateDePc s
          }
    )

execute :: SimM ()
execute = do
  instr <- gets stateExInstr
  mjmpAddr <- getFirst <$> asks Leak.outJumpAddr

  modify $ \s ->
    s
      { stateJumpAddr = mjmpAddr,
        stateMemInstr = instr
      }

  when (Leak.isLoad instr) $
    stallDecode

  case Leak.instrBase instr of
    Leak.Jump -> do
      if isJust mjmpAddr then do
        stallFetch
        stallDecode
      else do
        modify $ \s -> s {stateMemInstr = Leak.nop}
    _ -> pure ()

memory :: SimM ()
memory = do
  instr <- gets stateMemInstr
  modify $ \s -> s {stateWbInstr = instr}
  case Leak.instrBase instr of
    Leak.Load {} -> do
      outputNothing
      stallFetch
    Leak.Call {} -> do
      outputNothing
      stallFetch
    Leak.Store -> do
      outputNothing
      stallFetch
    Leak.Jump -> do
      stallDecode
    _ -> pure ()

writeback :: SimM ()
writeback = do
  instr <- gets stateWbInstr
  halted <- gets stateHalt

  when
    halted
    outputNothing

  case Leak.instrBase instr of
    Leak.Load {} -> stallDecode
    Leak.Call {} -> stallDecode
    Leak.Store -> stallDecode
    Leak.Break -> do
      outputNothing
      modify $ \s ->
        s
          { stateMemInstr = Leak.nop,
            stateExInstr = Leak.nop,
            stateHalt = True
          }
    _ -> pure ()

pipe :: SimM ()
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
            stateJumpAddr = Nothing,
            stateFirstCycle = firstCycle
          }
      void m
      modify $ \s -> s {stateFirstCycle = False}

circuit :: State -> Leak.Out -> (State, Maybe Address)
circuit s i = fromMaybe Nothing . getFirst <$> execRWS pipe i s
