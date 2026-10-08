-- | The server proof for the cache: the cache in front of memory implements
-- ideal memory, and leaks only the requests it receives.
--
-- The server is the four-line 'Memory.Cache.Cache4' in front of memory,
-- 'Proof.Machine.cacheServe'; ideal memory is 'Proof.Machine.idealServe'. The
-- proof's artifacts are
--
--   * the driver 'hop': a request, then no request until the response is
--     ready. Its hop is the /hop of the request/;
--   * the invariant 'srvInv': no miss is pending, and the cache is coherent
--     with memory;
--   * the flush 'srvMem': the memory behind the cache;
--   * the censor 'srvTag': the cache without its data;
--   * the simulator 'st': the cache's own hop, run on the censored cache in
--     front of a memory of zeros, on a request with the observed address and
--     width.
--
-- The obligations are 'srvInit', 'srvRefine' and 'srvLeak'. The cache's leakage
-- is 'Proof.Leakage.Model.obsReq', the observation of a request, which is also
-- the core's attacker ('Proof.Leakage.Model.obsOf'); the cache's own attacker
-- sees the ready bit of each response. With the core's patience
-- ('Proof.Cache.Obligation.stallObligation' and
-- 'Proof.Cache.Obligation.waitObligation', with 'stallsOnReads'), the core's
-- leakage proof behind ideal memory carries over to the core behind the cache.
module Proof.Cache.Server
  ( penalty,
    Srv,
    ReadyBits (..),
    readyList,
    hop,
    srvInv,
    srvTag,
    NoMem (..),
    reqOf,
    st,
    stBus,
    sameInput,
    srvInit,
    srvRefine,
    srvLeak,
    stallsOnReads,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core (Input (..), MemAccess (..))
import Data.Functor.Identity
import Memory.Cache (Cache4, coherent4, emptyCache4, tags4)
import Memory.Types (MemOps (..))
import Proof.Leakage.Model (Obs (..), obsReq)
import Proof.Machine (CacheSrv (..), Cycles, Request, cacheServe, idealServe)
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | The miss penalty the proof is about, as in the tests. 'hop' unrolls four
-- cycles, which covers penalties up to three.
penalty :: Cycles
penalty = 3

-- | The cache in front of memory.
type Srv m = CacheSrv Cache4 m

-- | The ready bit of each response in the hop of a request, at most four.
--
-- Four slots rather than a list because the verifier cannot execute folds, as
-- 'Proof.Leakage.Model.HopObs'.
data ReadyBits = ReadyBits Bool (Maybe Bool) (Maybe Bool) (Maybe Bool)
  deriving (Eq, Show)

readyList :: ReadyBits -> [Bool]
readyList (ReadyBits a b c d) = a : P.concatMap (P.maybe [] pure) [b, c, d]

-- | The hop of a request: the cache is sent the request, then no request until
-- its response is ready. Returns the server's state after the hop, the ready
-- bit of each response, and the last response.
--
-- Unrolled to four cycles: the verifier cannot unroll a loop whose length is
-- symbolic.
hop :: (MemOps m) => Srv m -> Request -> (Srv m, ReadyBits, Input Identity)
hop y q
  | inputMemReady x1 = (y1, ReadyBits True Nothing Nothing Nothing, x1)
  | inputMemReady x2 = (y2, ReadyBits False (Just True) Nothing Nothing, x2)
  | inputMemReady x3 = (y3, ReadyBits False (Just False) (Just True) Nothing, x3)
  | otherwise = (y4, ReadyBits False (Just False) (Just False) (Just (inputMemReady x4)), x4)
  where
    (y1, x1) = cacheServe penalty y q
    (y2, x2) = cacheServe penalty y1 Nothing
    (y3, x3) = cacheServe penalty y2 Nothing
    (y4, x4) = cacheServe penalty y3 Nothing

-- | The invariant: no miss is pending, and every valid line holds the word its
-- block starts with in memory.
srvInv :: (MemOps m) => Srv m -> Bool
srvInv (CacheSrv c pending mem) = case pending of
  Nothing -> coherent4 mem c
  Just _ -> False

-- | The censor: the cache without its data.
srvTag :: Srv m -> Cache4
srvTag y = tags4 (srvCache y)

-- | The simulator's memory: zeros, and writes are dropped. The simulator knows
-- no data.
data NoMem = NoMem
  deriving (Eq, Show)

instance MemOps NoMem where
  memReadWord _ _ = 0
  memWriteWord _ _ _ m = m
  memReadByte _ _ = 0

-- | A request with a given observation: a fetch of a word, and a write of zero.
reqOf :: Obs -> Request
reqOf o = case o of
  NoAccess -> Nothing
  Fetch addr -> Just (MemAccess True addr Types.Word Nothing)
  DataRead addr size -> Just (MemAccess False addr size Nothing)
  DataWrite addr size -> Just (MemAccess False addr size (Just (Identity 0)))

-- | The simulator: from the tags and the observation of a request, the ready
-- bits of the request's hop and the next tags.
--
-- Derived from the cache itself: its own hop, on the censored cache, in front of
-- 'NoMem', on 'reqOf' the observation.
st :: Cache4 -> Obs -> (Cache4, ReadyBits)
st t o = (srvTag y', rb)
  where
    (y', rb, _) = hop (CacheSrv t Nothing NoMem) (reqOf o)

-- | The simulator as it shows on the bus: the observed request, then one cycle
-- without a request for every response 'st' predicts not to be ready.
stBus :: Cache4 -> Obs -> (Cache4, [Obs])
stBus t o = (t', o : [NoAccess | b <- readyList rb, P.not b])
  where
    (t', rb) = st t o

sameInput :: Input Identity -> Input Identity -> Bool
sameInput a b =
  inputIsInstr a == inputIsInstr b
    && runIdentity (inputMem a) == runIdentity (inputMem b)
    && inputMemReady a == inputMemReady b

-- | The initial state: the empty cache, in front of any memory, satisfies the
-- invariant, and its tags are the same for every memory.
srvInit :: (MemOps m) => m -> Bool
srvInit mem = srvInv y && srvTag y == emptyCache4
  where
    y = CacheSrv emptyCache4 Nothing mem

-- | The refinement proof: from the invariant, the hop of a request ends with
-- ideal memory's response, back in the invariant, and with memory as ideal
-- memory leaves it. Memory is compared at a witness address, as in
-- 'Proof.Functional.Invariant.invAtFree'.
srvRefine :: (MemOps m) => Address -> Srv m -> Request -> Bool
srvRefine wa y q =
  not (srvInv y)
    || ( sameInput x xI
           && srvInv y'
           && memReadByte wa (srvMem y') == memReadByte wa mI
       )
  where
    (y', _, x) = hop y q
    (mI, xI) = idealServe (srvMem y) q

-- | The leakage proof: from the invariant, the ready bits of the hop of a
-- request and the next tags are what 'st' computes from the tags and the
-- observation of the request.
srvLeak :: (MemOps m) => Srv m -> Request -> Bool
srvLeak y q = not (srvInv y) || st (srvTag y) (obsReq q) == (srvTag y', rb)
  where
    (y', rb, _) = hop y q

-- | The cache answers late only to a data read. The core waits after every data
-- read ('Proof.Cache.Obligation.waitObligation'), so it waits after every
-- request the cache can stall on: the second half of patience.
stallsOnReads :: (MemOps m) => Srv m -> Request -> Bool
stallsOnReads y q = not (srvInv y) || inputMemReady x || isDataRead
  where
    (_, x) = cacheServe penalty y q
    isDataRead = case q of
      Just (MemAccess False _ _ Nothing) -> True
      _ -> False
