{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The \"system\" that the driver and the invariant talk about.
--
-- 'Core.circuit' is only the pipeline: memory lives outside it. The core emits
-- a 'MemAccess' on its 'Output' and receives the response on the next
-- 'Input'. So a single system step is one 'Core.circuit' step plus the memory
-- service that 'Simulate.simulator' performs, and the state the proof reasons
-- about is the triple
--
-- > (Core.State f, Input f, Mem)
--
-- which is exactly the shape the driver and invariant notes use.
module Proof.Machine
  ( SysG (..),
    Sys,
    stepSys,
    stepSysOut,
    stepSysN,
    initSys,
    running,
    exInstr,
    isNopInstr,
    isBubble,
    readMemWord,
    CacheSys (..),
    initCacheSys,
    stepCached,
    stepCachedOut,
    stepCachedN,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core
import Data.Functor.Identity
import Data.Maybe (isNothing)
import Data.Monoid (getFirst)
import Instruction
import Memory.Cache (CacheOps (..))
import Memory.Types
import RegFile
import Types
import qualified Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Core state, the input it is about to consume, and memory.
data SysG r m = Sys
  { sysState :: Core.StateG r Identity,
    sysInput :: Input Identity,
    sysMem :: m
  }

-- | The concrete system the QuickCheck harness runs: @Vec@-backed throughout.
type Sys = SysG RegFile MemBytes

deriving instance (Show (r Identity), Show m) => Show (SysG r m)

-- | Structural equality. 'Input' has no 'Eq' instance in "Core" (adding one
-- there clashes with the existing @Eq Out@ in the Leak modules), so we compare
-- its fields directly.
instance (Eq (r Identity), Eq m) => Eq (SysG r m) where
  Sys s1 i1 m1 == Sys s2 i2 m2 =
    s1 == s2
      && inputIsInstr i1 == inputIsInstr i2
      && runIdentity (inputMem i1) == runIdentity (inputMem i2)
      && m1 == m2

-- | One system step: run the pipeline for a cycle, then service whatever
-- memory access it emitted. Mirrors 'Simulate.simulator', except that a halted
-- core simply stops changing instead of ending the stream.
stepSys :: (RegFileOps r, MemOps m) => SysG r m -> SysG r m
stepSys = fst . stepSysOut

-- | 'stepSys', but also returning the pipeline's 'Output' for that cycle.
--
-- The leakage proof observes the cycle-by-cycle memory traffic, which 'stepSys'
-- discards. Defining both here means what it observes is exactly the traffic
-- the memory service responds to.
stepSysOut :: (RegFileOps r, MemOps m) => SysG r m -> (SysG r m, Output Identity)
stepSysOut (Sys s i m) =
  let (s', o) = Core.circuit s i
      (i', m') = service (getFirst (outMem o)) m
   in (Sys s' i' m', o)
  where
    service (Just (MemAccess isInstr addr size mval)) mem =
      case mval of
        -- A read. Note 'Memory.Vec.ramRead' ignores the size and always reads a
        -- word; the size-dependent narrowing happens in 'Core.writeback' via
        -- 'loadExtend'. We reproduce that here.
        Nothing -> (Input isInstr (pure (memReadWord addr mem)) True, mem)
        -- A write.
        Just val -> (Input isInstr (pure 0) True, memWriteWord size addr (runIdentity val) mem)
    service Nothing mem = (Input False (pure 0) True, mem)

stepSysN :: (RegFileOps r, MemOps m) => Int -> SysG r m -> SysG r m
stepSysN n s
  | n <= 0 = s
  | otherwise = stepSysN (n - 1) (stepSys s)

-- | The system as it starts up, with @prog@ loaded at 'initPc'.
initSys :: Vec PROG_SIZE Word -> Sys
initSys prog =
  Sys
    { sysState = Core.init,
      sysInput = initInput,
      sysMem = mkRAM @PROG_SIZE @RAM_SIZE_BYTES prog
    }

running :: SysG r m -> Bool
running = isNothing . stateHalt . sysState

exInstr :: SysG r m -> Instruction
exInstr = stateExInstr . sysState

isNopInstr :: Instruction -> Bool
isNopInstr (Nop _) = True
isNopInstr _ = False

-- | Is this a pipeline bubble, as opposed to a real instruction?
--
-- The driver's stated invariant is \"step until the execute stage is no longer
-- a no-op\", but that is slightly too coarse: @Nop DecodeFail@ is what an
-- undecodable memory word decodes to, so it /is/ the architectural instruction
-- at that PC and the ISA steps over it like any other. Only the stall reasons
-- are genuine bubbles.
isBubble :: Instruction -> Bool
isBubble (Nop DecodeFail) = False
isBubble (Nop _) = True
isBubble _ = False

readMemWord :: Address -> MemBytes -> Word
readMemWord = readWord

-- | System state paired with a cache model.
data CacheSys r m c = CacheSys
  { cacheSysCore :: SysG r m,
    cacheSysCache :: c,
    -- | Address and remaining cycles for a pending load miss.
    cacheSysPending :: Maybe (Address, Int)
  }

deriving instance (Show (r Identity), Show m, Show c) => Show (CacheSys r m c)

initCacheSys :: SysG r m -> c -> CacheSys r m c
initCacheSys sys cache = CacheSys sys cache Nothing

-- | Step a system with memory serviced through a cache.
stepCached :: (RegFileOps r, MemOps m, CacheOps c) => Int -> CacheSys r m c -> CacheSys r m c
stepCached missPenalty = fst . stepCachedOut missPenalty

stepCachedOut ::
  (RegFileOps r, MemOps m, CacheOps c) =>
  Int ->
  CacheSys r m c ->
  (CacheSys r m c, Output Identity)
stepCachedOut missPenalty (CacheSys (Sys s i m) cache pending) =
  let (s', o) = Core.circuit s i
      (i', m', cache', pending') = respond (getFirst (outMem o)) m cache pending
   in (CacheSys (Sys s' i' m') cache' pending', o)
  where
    respond mAccess mem c (Just (addr, n))
      | n > 0 = (Input False (pure 0) False, mem, c, Just (addr, n - 1))
      | otherwise =
          let w = memReadWord addr mem
           in (Input False (pure w) True, mem, cacheInsert addr w c, Nothing)
    respond Nothing mem c Nothing = (Input False (pure 0) True, mem, c, Nothing)
    respond (Just (MemAccess isInstr addr size mval)) mem c Nothing = case mval of
      Just val ->
        (Input isInstr (pure 0) True, memWriteWord size addr (runIdentity val) mem, cacheInvalidate addr c, Nothing)
      Nothing | isInstr -> (Input isInstr (pure (memReadWord addr mem)) True, mem, c, Nothing)
      Nothing -> case cacheLookup addr c of
        Just w -> (Input isInstr (pure w) True, mem, c, Nothing)
        Nothing -> (Input isInstr (pure 0) False, mem, c, Just (addr, missPenalty - 1))

stepCachedN :: (RegFileOps r, MemOps m, CacheOps c) => Int -> Int -> CacheSys r m c -> CacheSys r m c
stepCachedN missPenalty n s
  | n <= 0 = s
  | otherwise = stepCachedN missPenalty (n - 1) (stepCached missPenalty s)
