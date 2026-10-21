{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 4 KiB cache ("Memory.Cache4K") as a server between the core and memory:
-- how it answers the core's memory requests, cycle by cycle.
--
-- A read miss takes 'penalty' extra cycles and then fills the line with the
-- four words of its block; a read that leaves its word bypasses the cache with
-- the same delay. A write goes through to memory at once: inside one word, on a
-- hit, it is merged into the cached word; if it crosses a word boundary, the
-- lines it touches are invalidated instead ('Memory.Cache4K.write'). Fetches
-- bypass the cache.
module Memory.Cache4KServer
  ( Cycles,
    CacheSrv (..),
    penalty,
    serve,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log, read)
import Core (Input (..), MemAccess (..), Request)
import Data.Functor.Identity
import Memory.Cache4K
import Memory.Types (MemOps (..), readResp)
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, read, undefined, (!!), (&&), (++), (||))

-- | Cycles of a miss penalty: a small counter, sized as hardware would size it.
-- (This also keeps the symbolic proof in the bitvector fragment Bitwuzla reads.)
type Cycles = Unsigned 8

-- | Extra cycles of a read miss: the four words of a line.
penalty :: Cycles
penalty = 4

-- | The address of the last byte of an access. An access touches at most two
-- lines: the lines of its first and last byte.
data CacheSrvState = CacheSrvReady | CacheSrvStall Cycles Word
  deriving Show

-- | A cache in front of memory, as a server: the cache, a read waiting for
-- memory, and memory.
data CacheSrv m = CacheSrv
  { srvCache :: Cache4K,
    srvMem :: m,
    -- | A read waiting for memory: its address and width, and the cycles left.
    srvState :: CacheSrvState
  }

deriving instance (Show m) => Show (CacheSrv m)

-- | One cycle of the cache in front of ideal memory.
serve :: (MemOps m) => CacheSrv m -> Request -> (CacheSrv m, Input Identity)
serve (CacheSrv c mem s) req = case s of
  CacheSrvStall n w
    | n > 0 ->
        (CacheSrv c mem (CacheSrvStall (n - 1) w), Input (pure 0) False)
    | otherwise ->
        (CacheSrv c mem CacheSrvReady, Input (pure w) True)
  CacheSrvReady -> case req of
    Nothing -> (CacheSrv c mem CacheSrvReady, Input (pure 0) True)
    Just (MemAccess isInstr addr size mval) -> case mval of
      Just val ->
        ( CacheSrv
            (write addr size (runIdentity val) c)
            (memWriteWord size addr (runIdentity val) mem)
            CacheSrvReady,
          Input (pure 0) True
        )
      Nothing
        | isInstr ->
            ( CacheSrv c mem CacheSrvReady,
              Input (pure (readResp addr size mem)) True
            )
        | otherwise ->
            let off = case size of                         
                  Byte -> 0
                  Half -> 1
                  Word -> 3
            
                hit1stLine = hit addr c
                hit2ndLine = hit (addr + off) c

                penalty1stLine = if hit1stLine then 0 else 4
                penalty2ndLine = if hit2ndLine then 0 else 4
                n = penalty1stLine + penalty2ndLine

                c1 = if hit1stLine then c else fill addr mem c
                c2 = if hit2ndLine then c1 else fill (addr + off) mem c1

                w = pure (read addr size c2)

                in if penalty > 0
                  then (CacheSrv c2 mem (CacheSrvStall (n - 1) w), Input (pure 0) False)
                  else (CacheSrv c2 mem, Input (pure w) True)
