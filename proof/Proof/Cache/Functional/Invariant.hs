-- | The cache's invariant, stated at one word -- the cache's counterpart of
-- "Proof.Core.Functional.Invariant". The cache holds 1024 words, so the
-- invariant is stated at a witness word, and the obligations quantify over it,
-- as the core's invariant does over a register and a byte.
module Proof.Cache.Functional.Invariant
  ( coherentAt,
    sameCoreAt,
    cacheInvAt
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core (StateG (..))
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Types (MemOps (..))
import Proof.Cache.Functional.Machine (CachedSysG (..))
import Proof.Core.Functional.Machine (SysG (..))
import Core.RegFile (RegFileOps (..))
import Types (Address, RegIdx)
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

-- | Two core states agree at register @wr@: every field is equal, except that
-- the register files are compared only at @wr@, as the core's invariant
-- compares them ('Proof.Core.Functional.Invariant.invAtFree'), and the control
-- lines not at all: the core sets them afresh every cycle before reading them.
sameCoreAt :: (RegFileOps r) => RegIdx -> StateG r -> StateG r -> Bool
sameCoreAt wr s1 s2 =
  stateFePc s1 == stateFePc s2
    && stateDePc s1 == stateDePc s2
    && stateDeExpInstr s1 == stateDeExpInstr s2
    && stateExPc s1 == stateExPc s2
    && stateExInstr s1 == stateExInstr s2
    && stateMeInstr s1 == stateMeInstr s2
    && stateMeRes s1 == stateMeRes s2
    && stateMeAddr s1 == stateMeAddr s2
    && stateWbInstr s1 == stateWbInstr s2
    && stateWbRes s1 == stateWbRes s2
    && lookupRFg wr (stateRegFile s1) == lookupRFg wr (stateRegFile s2)
    && stateHalt s1 == stateHalt s2

-- | The invariant at register @wr@, byte @wa@ and word @wwi@ of set @wsi@: the
-- cores agree at @wr@, no miss is pending, memory holds ideal memory's byte
-- @wa@, and if the line is valid, its word @wwi@ is memory's.
cacheInvAt :: (RegFileOps r, MemOps m) => RegIdx -> Address -> SetIdx -> WordIdx -> SysG r m -> CachedSysG r m -> Bool
cacheInvAt wr wa wsi wwi sys csys =
  sameCoreAt wr (cachedCore csys) (sysCore sys) &&
  cachedInput csys == sysInput sys &&
  srvState srv == CacheSrvReady &&
  memReadByte wa (srvMem srv) == memReadByte wa (sysMem sys) &&
  coherentAt wsi wwi (srvMem srv) (srvCache srv)
  where srv = cachedSrv csys
