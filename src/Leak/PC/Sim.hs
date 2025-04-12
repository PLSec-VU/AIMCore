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
      stateStallFetch = False,
      stateStallDecode = False,
      stateJumpAddr = Nothing
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
  outputPc =<< gets stateFePc
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

decode :: SimM ()
decode = do
  instr <- fromMaybe Leak.nop . getFirst <$> asks Leak.outInstr
  ex_ir <- gets stateExInstr
  when (Leak.isLoad instr) $ do
    stallFetch
  ifM
    ((Leak.loadHazard instr ex_ir ||) <$> gets stateStallDecode)
    (modify $ \s -> s {stateExInstr = Leak.nop})
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

  case Leak.instrBase instr of
    Leak.Jump -> do
      case mjmpAddr of
        Just {} -> do
          stallDecode
          modify $ \s ->
            s
              { stateMemInstr = instr {Leak.instrBase = Leak.Jump}
              }
        Nothing ->
          modify $ \s ->
            s
              { stateMemInstr = Leak.nop
              }
    _ -> pure ()

memory :: SimM ()
memory = do
  instr <- gets stateMemInstr
  case Leak.instrBase instr of
    Leak.Load {} -> do
      outputNothing
      stallFetch
    Leak.Store -> do
      outputNothing
      stallFetch
    Leak.Jump ->
      stallDecode
    _ -> pure ()

  modify $ \s ->
    s
      { stateWbInstr =
          -- No longer care about the jumps (matters for state equivalence)
          if isJump instr
            then Leak.nop
            else instr
      }
  where
    isJump (Leak.Instr Leak.Jump {} _) = True
    isJump _ = False

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
      stallDecode
    Leak.Store -> do
      stallDecode
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
          { stateStallFetch = False,
            stateStallDecode = False,
            stateJumpAddr = Nothing
          }

circuit :: State -> Leak.Out -> (State, Maybe Address)
circuit s i = fromMaybe Nothing . getFirst <$> execRWS pipe i s
