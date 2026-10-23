{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The server proof for the 4 KiB cache ("Memory.Cache4K"), served by
-- 'Memory.Cache4KServer.serve'.
--
-- The artifacts: the hop of a request ('Proof.Cache.Functional.Driver.driver'
-- cycles of 'Proof.Cache.Functional.Machine.serveN'),
-- the invariant (no miss pending, every valid line holds memory's words), the
-- flush 'srvMem', the censor 'srvTag' (the tag array), and the simulator
-- 'simStep', derived from the cache itself.
--
-- The cache holds 1024 words, so the invariant is stated at one word,
-- 'srvInvAt', and the obligations quantify over that word, as the functional
-- invariant does over a register and a byte. 'srvRefine' also assumes the
-- invariant at the word the request accesses. Both assumptions are instances of
-- the invariant at every word, so 'srvRefine' for every witness gives the
-- obligation for the whole invariant.
module Proof.Cache.Server4K
  ( coherentAt,
    srvInvAt,
    srvTag,
    simStep,
    simBus,
    srvInit,
    srvRefine,
    srvLeak,
    stallsOnReads,
    CacheSys (..),
    initCacheSys,
    stepCachedOut,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core (Input (..), MemAccess (..), Output (..), Request, circuit)
import Data.Functor.Identity
import Memory.Cache4K
import Memory.Types (MemOps (..))
import Proof.Core.Leakage.Model (Obs (..), obsReq)
import Memory.Cache4KServer
import Proof.Cache.Functional.Driver (driver)
import Proof.Cache.Functional.Machine (serveN)
import Proof.Core.Functional.Machine (SysG (..))
import Memory.Ideal (idealServe)
import Core.RegFile (RegFileOps)
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | Coherence at word @wi@ of set @s@: if the line there is valid, the word is
-- the one memory holds at its address. The cache is coherent if this holds at
-- every set and word.
coherentAt :: (MemOps m) => m -> SetIdx -> WordIdx -> Cache4K -> Bool
coherentAt mem s wi c =
  not (entryValid e) || dataRead (s ++# wi) (dataArr c) == w
  where
    e = tagRead s (tagArr c)
    w = memReadWord (wordAddr (entryTag e) s wi 0) mem

-- | The invariant at word @w@ of set @s@: no miss is pending, and if the line
-- is valid, its word @w@ is memory's.
srvInvAt :: (MemOps m) => SetIdx -> WordIdx -> CacheSrv m -> Bool
srvInvAt s w (CacheSrv c pending mem) = case pending of
  Nothing -> coherentAt mem s w c
  Just _ -> False

noMiss :: CacheSrv m -> Bool
noMiss y = case srvPending y of
  Nothing -> True
  Just _ -> False

-- | The censor: the tags.
srvTag :: CacheSrv m -> TagArr
srvTag y = tagArr (srvCache y)

-- | The simulator's memory: zeros, and writes are dropped. The simulator knows
-- no data.
data NoMem = NoMem
  deriving (Eq, Show)

instance MemOps NoMem where
  memReadWord _ _ = 0
  memWrite _ _ _ m = m
  memReadByte _ _ = 0

-- | A request with a given observation: a fetch of a word, and a write of zero.
reqOf :: Obs -> Request
reqOf o = case o of
  NoAccess -> Nothing
  Fetch addr -> Just (MemAccess True addr Types.Word Nothing)
  DataRead addr size -> Just (MemAccess False addr size Nothing)
  DataWrite addr size -> Just (MemAccess False addr size (Just (Identity 0)))

-- | The simulator: the cache's own hop, on the tags with no data, in front of
-- 'NoMem', on a request with the observation. Returns the next tags and how many
-- responses are not ready.
simStep :: TagArr -> Obs -> (TagArr, Cycles)
simStep t o = (srvTag y', k)
  where
    y0 = CacheSrv (Cache4K t dataZero) Nothing NoMem
    q = reqOf o
    n = driver y0 q
    k = fromIntegral n
    (y', _) = serveN n y0 q

-- | The simulator as it shows on the bus: the observed request, then one cycle
-- without a request per response that is not ready.
simBus :: TagArr -> Obs -> (TagArr, [Obs])
simBus t o = (t', o : P.replicate (fromIntegral k) NoAccess)
  where
    (t', k) = simStep t o

sameInput :: Input Identity -> Input Identity -> Bool
sameInput a b =
  runIdentity (inputMem a) == runIdentity (inputMem b)
    && inputMemReady a == inputMemReady b

-- | The empty cache, in front of any memory, satisfies the invariant. Its
-- censor is 'tagEmpty' for every memory by definition.
srvInit :: (MemOps m) => SetIdx -> WordIdx -> m -> Bool
srvInit s w mem = srvInvAt s w (CacheSrv emptyCache4K Nothing mem)

-- | The refinement proof, at witnesses: memory at byte @wa@ and the invariant at
-- word @ww@ of set @ws@. The hop of a request ends with ideal memory's response,
-- and with memory and the invariant as ideal memory leaves them.
srvRefine :: (MemOps m) => Address -> SetIdx -> WordIdx -> CacheSrv m -> Request -> Bool
srvRefine wa ws ww y q =
  not (srvInvAt ws ww y && accessed)
    || ( sameInput x xI
           && srvInvAt ws ww y'
           && memReadByte wa (srvMem y') == memReadByte wa mI
       )
  where
    (y', x) = serveN (driver y q) y q
    (mI, xI) = idealServe (srvMem y) q
    accessed = case q of
      Just (MemAccess _ addr _ _) -> srvInvAt (setOf addr) (wordOf addr) y
      Nothing -> True

-- | The leakage proof, at a witness set: with no miss pending, how many
-- responses of the hop are not ready, and the next tags, are what 'simStep' computes
-- from the tags and the observation of the request.
srvLeak :: (MemOps m) => SetIdx -> CacheSrv m -> Request -> Bool
srvLeak ws y q =
  not (noMiss y) || (k == fromIntegral kS && tagRead ws (srvTag y') == tagRead ws tS)
  where
    k = driver y q
    (y', _) = serveN k y q
    (tS, kS) = simStep (srvTag y) (obsReq q)

-- | The cache answers late only to a data read. With the core's patience
-- ('Proof.Cache.Obligation.waitObligation'), the core's leakage proof carries
-- over to the core behind this cache too.
stallsOnReads :: (MemOps m) => CacheSrv m -> Request -> Bool
stallsOnReads y q = not (noMiss y) || inputMemReady x || isDataRead
  where
    (_, x) = serve y q
    isDataRead = case q of
      Just (MemAccess False _ _ Nothing) -> True
      _ -> False

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

-- | The core in a loop with the cache in front of memory.
stepCachedOut :: (RegFileOps r, MemOps m) => CacheSys r m Cache4K -> (CacheSys r m Cache4K, Output Identity)
stepCachedOut (CacheSys (Sys s i m) cache pending) =
  let (s', o) = circuit s i
      (CacheSrv cache' pending' m', i') = serve (CacheSrv cache pending m) (outMem o)
   in (CacheSys (Sys s' i' m') cache' pending', o)
