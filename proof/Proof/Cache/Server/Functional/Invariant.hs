-- | The cache's invariant, stated at one word -- the cache's counterpart of
-- "Proof.Core.Functional.Invariant". The cache holds 1024 words, so the
-- invariant is stated at a witness word, and the obligations quantify over it,
-- as the core's invariant does over a register and a byte.
module Proof.Cache.Server.Functional.Invariant
  ( coherentAt,
    srvInvAt
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Types (Address)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Coherence at word @wwi@ of set @wsi@: if the line there is valid, the word
-- is the one memory holds at its address. The cache is coherent if this holds
-- at every set and word.
coherentAt :: (MemOps m) => SetIdx -> WordIdx -> m -> Cache4K -> Bool
coherentAt wsi wwi mem c =
  not (entryValid e) || dataRead (wsi ++# wwi) (dataArr c) == w
  where
    e = tagRead wsi (tagArr c)
    w = memReadWord (wordAddr (entryTag e) wsi wwi 0) mem

-- | The invariant at byte @wa@ and at word @wwi@ of set @wsi@: no miss is
-- pending, memory holds ideal memory's byte @wa@, and if the line is valid,
-- its word @wwi@ is memory's.
srvInvAt :: (MemOps m) => Address -> SetIdx -> WordIdx -> m -> CacheSrv m -> Bool
srvInvAt wa wsi wwi ideal (CacheSrv c mem st) =
  st == CacheSrvReady &&
  memReadByte wa ideal == memReadByte wa mem &&
  coherentAt wsi wwi mem c
