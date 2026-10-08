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
    Request,
    readResp,
    idealServe,
    Cycles,
    CacheSrv (..),
    cacheServe,
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
import Memory.Cache (CacheOps (..), blockStart, fromBlock, inBlock)
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
--
-- The system is the core in a loop with 'idealServe': the core's request goes
-- to memory, and memory's response is the core's input in the next cycle.
stepSysOut :: (RegFileOps r, MemOps m) => SysG r m -> (SysG r m, Output Identity)
stepSysOut (Sys s i m) =
  let (s', o) = Core.circuit s i
      (m', i') = idealServe m (getFirst (outMem o))
   in (Sys s' i' m', o)

-- | A request on the memory bus, if any.
type Request = Maybe (MemAccess Identity)

-- | What memory answers to a read: the bytes the read asks for, zero above.
--
-- 'Core.writeback' uses no other bytes ('loadExtend'), and an instruction fetch
-- reads a whole word. Answering with exactly these bytes is what lets a cache,
-- which holds only the read's block, give the same answer as memory.
readResp :: (MemOps m) => Address -> Size -> m -> Word
readResp addr size mem = loadExtend size Unsigned (memReadWord addr mem)

-- | Ideal memory as a server: it answers every request at once.
idealServe :: (MemOps m) => m -> Request -> (m, Input Identity)
idealServe mem req = case req of
  Just (MemAccess isInstr addr size mval) -> case mval of
    Nothing -> (mem, Input isInstr (pure (readResp addr size mem)) True)
    Just val -> (memWriteWord size addr (runIdentity val) mem, Input isInstr (pure 0) True)
  Nothing -> (mem, Input False (pure 0) True)

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

-- | Cycles of a miss penalty. A bitvector rather than an 'Int', so that the
-- cache stays in the fragment of the symbolic proof that Bitwuzla reads.
type Cycles = Unsigned 8

-- | A cache in front of memory, as a server: the cache, a read waiting for
-- memory, and memory.
data CacheSrv c m = CacheSrv
  { srvCache :: c,
    -- | A read waiting for memory: its address and width, and the cycles left.
    srvPending :: Maybe (Address, Size, Cycles),
    srvMem :: m
  }

deriving instance (Show m, Show c) => Show (CacheSrv c m)

-- | The cache as a server.
--
-- Instruction fetches bypass the cache and are answered at once. A data read
-- that hits is answered at once from its block's word; one that misses is
-- answered after @missPenalty@ cycles in which the response is not ready, and
-- its block is then installed. A read that straddles two blocks is not cached: it
-- always takes the miss penalty and installs nothing. A write goes straight to
-- memory, is answered at once, and invalidates every block it touches. Reads are
-- answered with the bytes they ask for, as 'idealServe' does.
cacheServe :: (MemOps m, CacheOps c) => Cycles -> CacheSrv c m -> Request -> (CacheSrv c m, Input Identity)
cacheServe missPenalty (CacheSrv c pending mem) req = case pending of
  Just (addr, size, n)
    | n > 0 -> (CacheSrv c (Just (addr, size, n - 1)) mem, Input False (pure 0) False)
    | inBlock addr size ->
        let w = memReadWord (blockStart addr) mem
         in (CacheSrv (cacheInsert addr w c) Nothing mem, Input False (pure (fromBlockResp addr size w)) True)
    | otherwise -> (CacheSrv c Nothing mem, Input False (pure (readResp addr size mem)) True)
  Nothing -> case req of
    Nothing -> (CacheSrv c Nothing mem, Input False (pure 0) True)
    Just (MemAccess isInstr addr size mval) -> case mval of
      Just val ->
        ( CacheSrv
            (cacheInvalidate (lastByte addr size) (cacheInvalidate addr c))
            Nothing
            (memWriteWord size addr (runIdentity val) mem),
          Input isInstr (pure 0) True
        )
      Nothing
        | isInstr -> (CacheSrv c Nothing mem, Input isInstr (pure (readResp addr size mem)) True)
        | otherwise -> case cacheLookup addr c of
            Just w
              | inBlock addr size -> (CacheSrv c Nothing mem, Input isInstr (pure (fromBlockResp addr size w)) True)
            _ -> (CacheSrv c (Just (addr, size, missPenalty - 1)) mem, Input isInstr (pure 0) False)
  where
    fromBlockResp addr size w = loadExtend size Unsigned (fromBlock addr w)

    lastByte addr size = addr + case size of
      Types.Byte -> 0
      Types.Half -> 1
      Types.Word -> 3

-- | System state paired with a cache model.
data CacheSys r m c = CacheSys
  { cacheSysCore :: SysG r m,
    cacheSysCache :: c,
    -- | A read waiting for memory: its address and width, and the cycles left.
    cacheSysPending :: Maybe (Address, Size, Cycles)
  }

deriving instance (Show (r Identity), Show m, Show c) => Show (CacheSys r m c)

initCacheSys :: SysG r m -> c -> CacheSys r m c
initCacheSys sys cache = CacheSys sys cache Nothing

-- | Step a system with memory serviced through a cache: the core in a loop with
-- 'cacheServe'.
stepCached :: (RegFileOps r, MemOps m, CacheOps c) => Cycles -> CacheSys r m c -> CacheSys r m c
stepCached missPenalty = fst . stepCachedOut missPenalty

stepCachedOut ::
  (RegFileOps r, MemOps m, CacheOps c) =>
  Cycles ->
  CacheSys r m c ->
  (CacheSys r m c, Output Identity)
stepCachedOut missPenalty (CacheSys (Sys s i m) cache pending) =
  let (s', o) = Core.circuit s i
      (CacheSrv cache' pending' m', i') = cacheServe missPenalty (CacheSrv cache pending m) (getFirst (outMem o))
   in (CacheSys (Sys s' i' m') cache' pending', o)

stepCachedN :: (RegFileOps r, MemOps m, CacheOps c) => Cycles -> Int -> CacheSys r m c -> CacheSys r m c
stepCachedN missPenalty n s
  | n <= 0 = s
  | otherwise = stepCachedN missPenalty (n - 1) (stepCached missPenalty s)
