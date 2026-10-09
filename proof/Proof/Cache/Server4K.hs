-- | The server proof for the 4 KiB cache ("Memory.Cache4K"), as
-- "Proof.Cache.Server" is for the four-line one.
--
-- The cache as a server is 'serveK'. A read miss takes 'penaltyK' extra cycles
-- and then fills the line with the four words of its block; a read that leaves
-- its word bypasses the cache with the same delay. A write goes through to
-- memory at once: on a hit it is merged into the cached word, and if it leaves
-- its word it drops the lines of its first and last byte. Fetches bypass the
-- cache.
--
-- The artifacts are those of "Proof.Cache.Server": the hop of a request
-- ('hopK'), the invariant (no miss pending, every valid line holds memory's
-- words), the flush 'srvMem', the censor 'srvTagK' (the tag array), and the
-- simulator 'stK', derived from the cache as before.
--
-- The cache holds 1024 words, so the invariant is stated at one word,
-- 'srvInvAtK', and the obligations quantify over that word, as the functional
-- invariant does over a register and a byte. 'srvRefineK' also assumes the
-- invariant at the word the request accesses. Both assumptions are instances of
-- the invariant at every word, so 'srvRefineK' for every witness gives the
-- obligation for the whole invariant.
module Proof.Cache.Server4K
  ( penaltyK,
    SrvK,
    serveK,
    hopK,
    srvInvAtK,
    srvTagK,
    stK,
    stBusK,
    srvInitK,
    srvRefineK,
    srvLeakK,
    stallsOnReadsK,
    stepCachedKOut,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core (Input (..), MemAccess (..), Output (..), circuit)
import Data.Functor.Identity
import Data.Monoid (getFirst)
import Instruction (Sign (..), loadExtend)
import Memory.Cache (fromBlock, inBlock)
import Memory.Cache4K
import Memory.Types (MemOps (..))
import Proof.Cache.Server (NoMem (..), reqOf, sameInput)
import Proof.Leakage.Model (Obs (..), obsReq)
import Proof.Machine (CacheSrv (..), CacheSys (..), Cycles, Request, SysG (..), idealServe, readResp)
import RegFile (RegFileOps)
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | Extra cycles of a read miss: the four words of a line, after one cycle.
penaltyK :: Cycles
penaltyK = 5

-- | The cache in front of memory.
type SrvK m = CacheSrv Cache4K m

-- | One cycle of the cache in front of memory.
serveK :: (MemOps m) => SrvK m -> Request -> (SrvK m, Input Identity)
serveK (CacheSrv c pending mem) req = case pending of
  Just (addr, size, n)
    | n > 0 -> (CacheSrv c (Just (addr, size, n - 1)) mem, Input False (pure 0) False)
    | inBlock addr size ->
        ( CacheSrv (fillK addr (rd 0) (rd 1) (rd 2) (rd 3) c) Nothing mem,
          Input False (pure (resp addr size (rd (wordOf addr)))) True
        )
    | otherwise -> (CacheSrv c Nothing mem, Input False (pure (readResp addr size mem)) True)
    where
      rd w = memReadWord (wordAddr (tagOf addr) (setOf addr) w) mem
  Nothing -> case req of
    Nothing -> (CacheSrv c Nothing mem, Input False (pure 0) True)
    Just (MemAccess isInstr addr size mval) -> case mval of
      Just val ->
        ( CacheSrv
            ( if inBlock addr size
                then updateK addr size (runIdentity val) c
                else invalidateK (lastByte addr size) (invalidateK addr c)
            )
            Nothing
            (memWriteWord size addr (runIdentity val) mem),
          Input isInstr (pure 0) True
        )
      Nothing
        | isInstr -> (CacheSrv c Nothing mem, Input isInstr (pure (readResp addr size mem)) True)
        | inBlock addr size && hitK addr c ->
            (CacheSrv c Nothing mem, Input isInstr (pure (resp addr size (wordAtK addr c))) True)
        | otherwise -> (CacheSrv c (Just (addr, size, penaltyK - 1)) mem, Input isInstr (pure 0) False)
  where
    resp addr size w = loadExtend size Unsigned (fromBlock addr w)

lastByte :: Address -> Size -> Address
lastByte addr size =
  addr + case size of
    Types.Byte -> 0
    Types.Half -> 1
    Types.Word -> 3

-- | The hop of a request: the request, then no request until the response is
-- ready. Returns the state after the hop, the number @k@ of responses that were
-- not ready (the ready bits of the hop are @k@ times 'False', then 'True'), and
-- the last response.
--
-- Unrolled to six cycles, enough for 'penaltyK'.
hopK :: (MemOps m) => SrvK m -> Request -> (SrvK m, Cycles, Input Identity)
hopK y q
  | inputMemReady x1 = (y1, 0, x1)
  | inputMemReady x2 = (y2, 1, x2)
  | inputMemReady x3 = (y3, 2, x3)
  | inputMemReady x4 = (y4, 3, x4)
  | inputMemReady x5 = (y5, 4, x5)
  | otherwise = (y6, 5, x6)
  where
    (y1, x1) = serveK y q
    (y2, x2) = serveK y1 Nothing
    (y3, x3) = serveK y2 Nothing
    (y4, x4) = serveK y3 Nothing
    (y5, x5) = serveK y4 Nothing
    (y6, x6) = serveK y5 Nothing

-- | The invariant at word @w@ of set @s@: no miss is pending, and if the line
-- is valid, its word @w@ is memory's.
srvInvAtK :: (MemOps m) => SetIdx -> WordIdx -> SrvK m -> Bool
srvInvAtK s w (CacheSrv c pending mem) = case pending of
  Nothing -> coherentAtK mem s w c
  Just _ -> False

noMiss :: SrvK m -> Bool
noMiss y = case srvPending y of
  Nothing -> True
  Just _ -> False

-- | The censor: the tags.
srvTagK :: SrvK m -> TagArr
srvTagK y = ckTags (srvCache y)

-- | The simulator: the cache's own hop, on the tags with no data, in front of
-- 'NoMem', on a request with the observation. Returns the next tags and how many
-- responses are not ready.
stK :: TagArr -> Obs -> (TagArr, Cycles)
stK t o = (srvTagK y', k)
  where
    (y', k, _) = hopK (CacheSrv (Cache4K t dataZero) Nothing NoMem) (reqOf o)

-- | The simulator as it shows on the bus: the observed request, then one cycle
-- without a request per response that is not ready.
stBusK :: TagArr -> Obs -> (TagArr, [Obs])
stBusK t o = (t', o : P.replicate (fromIntegral k) NoAccess)
  where
    (t', k) = stK t o

-- | The empty cache, in front of any memory, satisfies the invariant. Its
-- censor is 'tagEmpty' for every memory by definition.
srvInitK :: (MemOps m) => SetIdx -> WordIdx -> m -> Bool
srvInitK s w mem = srvInvAtK s w (CacheSrv emptyCache4K Nothing mem)

-- | The refinement proof, at witnesses: memory at byte @wa@ and the invariant at
-- word @ww@ of set @ws@. The hop of a request ends with ideal memory's response,
-- and with memory and the invariant as ideal memory leaves them.
srvRefineK :: (MemOps m) => Address -> SetIdx -> WordIdx -> SrvK m -> Request -> Bool
srvRefineK wa ws ww y q =
  not (srvInvAtK ws ww y && accessed)
    || ( sameInput x xI
           && srvInvAtK ws ww y'
           && memReadByte wa (srvMem y') == memReadByte wa mI
       )
  where
    (y', _, x) = hopK y q
    (mI, xI) = idealServe (srvMem y) q
    accessed = case q of
      Just (MemAccess _ addr _ _) -> srvInvAtK (setOf addr) (wordOf addr) y
      Nothing -> True

-- | The leakage proof, at a witness set: with no miss pending, how many
-- responses of the hop are not ready, and the next tags, are what 'stK' computes
-- from the tags and the observation of the request.
srvLeakK :: (MemOps m) => SetIdx -> SrvK m -> Request -> Bool
srvLeakK ws y q =
  not (noMiss y) || (k == kS && tagRead (srvTagK y') ws == tagRead tS ws)
  where
    (y', k, _) = hopK y q
    (tS, kS) = stK (srvTagK y) (obsReq q)

-- | The cache answers late only to a data read, as the four-line cache does
-- ('Proof.Cache.Server.stallsOnReads'). With the core's patience, the core's
-- leakage proof carries over to the core behind this cache too.
stallsOnReadsK :: (MemOps m) => SrvK m -> Request -> Bool
stallsOnReadsK y q = not (noMiss y) || inputMemReady x || isDataRead
  where
    (_, x) = serveK y q
    isDataRead = case q of
      Just (MemAccess False _ _ Nothing) -> True
      _ -> False

-- | The core in a loop with the cache in front of memory.
stepCachedKOut :: (RegFileOps r, MemOps m) => CacheSys r m Cache4K -> (CacheSys r m Cache4K, Output Identity)
stepCachedKOut (CacheSys (Sys s i m) cache pending) =
  let (s', o) = circuit s i
      (CacheSrv cache' pending' m', i') = serveK (CacheSrv cache pending m) (getFirst (outMem o))
   in (CacheSys (Sys s' i' m') cache' pending', o)
