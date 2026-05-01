{-# LANGUAGE TypeFamilies #-}

module Correctness.Driver
  ( DriverState (..)
  , initDriverState
  , stepDriver
  ) where

import Clash.Prelude hiding (Word, Ordering(..))
import Prelude hiding (Word, (!!), not, (&&), (||))
import Types
import Instruction (Instruction, encode)
import Core 

import RegFile
import Data.Monoid (First(..))
import Data.Functor.Identity
import Util (readWord, write)

-- | Driver state: wraps the pipelined core with its own registers and memory.
data DriverState (n :: Nat) = DriverState
  { dCoreState :: Core.State Identity
  -- ^ The pipelined core's internal state.
  , dRegFile :: RegFile
  -- ^ Register file.
  , dRAM :: Vec n Byte
  -- ^ Data memory.
  , dInput :: Input Identity
  -- ^ The input to feed to the core on the next tick.
  } deriving (Show, Generic)

-- | Initialise the driver state with a given RAM.
initDriverState :: Vec n Byte -> DriverState n
initDriverState ram = DriverState
  { dCoreState = Core.init
  , dRegFile   = initRF
  , dRAM       = ram
  , dInput     = Core.initInput
  }

-- | Step the driver for one cycle.
stepDriver
  :: (KnownNat n)
  => Instruction
  -> DriverState n
  -> (DriverState n, Output Identity)
stepDriver instr ds =
  let
    (coreState', out) = Core.circuit (dCoreState ds) (dInput ds)

    rf' = case getFirst (Core.outRd out) of
      Just (idx, val) | idx /= 0 -> modifyRF idx (runIdentity val) (dRegFile ds)
      _ -> dRegFile ds

    rs1' = maybe 0 (`lookupRF` rf') $ getFirst (Core.outRs1 out)
    rs2' = maybe 0 (`lookupRF` rf') $ getFirst (Core.outRs2 out)

    (memWord, memIsI, ram') = case getFirst (Core.outMem out) of
      Just (MemAccess True _addr _size Nothing) ->
        (Identity (encode instr), True, dRAM ds)
      Just (MemAccess False addr _size Nothing) ->
        (Identity (readWord addr (dRAM ds)), False, dRAM ds)
      Just (MemAccess False addr size (Just val)) ->
        (Identity 0, False, write size addr (runIdentity val) (dRAM ds))
      Just (MemAccess True _ _ (Just _)) ->
        (Identity 0, True, dRAM ds)
      Nothing ->
        (Identity 0, False, dRAM ds)

    nextInput = Input
      { inputIsInstr = memIsI
      , inputMem     = memWord
      , inputRs1     = Identity rs1'
      , inputRs2     = Identity rs2'
      }

    ds' = DriverState
      { dCoreState = coreState'
      , dRegFile   = rf'
      , dRAM       = ram'
      , dInput     = nextInput
      }
  in (ds', out)
