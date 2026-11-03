-- | The invariant of link 2, relating the core behind the cache to the core
-- behind ideal memory -- the counterpart of "Proof.Core.Functional.Invariant".
--
-- The two cores are the same, and so are their inputs; the cache relates to
-- ideal memory as in the cache's own proof
-- ('Proof.Cache.Server.Functional.Invariant.srvInvAt'): ready, holding ideal
-- memory's byte @wa@, and coherent at word @wwi@ of set @wsi@. The cache holds
-- 1024 words and memory many more bytes, so the invariant is stated at
-- witnesses, and the obligations quantify over them.
module Proof.Cache.CachedCore.Functional.Invariant
  ( cachedInvAt,
    cachedInvFunctional,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Memory.Cache4K (SetIdx, WordIdx)
import Memory.Types (MemOps (..))
import Proof.Cache.Server.Functional.Invariant (srvInvAt)
import Proof.Cache.CachedCore.Functional.Machine (CachedSysG (..))
import Proof.Core.Functional.Machine (SysG (..))
import Types (Address)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The invariant at byte @wa@ and word @wwi@ of set @wsi@: the cores and their
-- inputs are equal, and the server satisfies the cache's invariant against
-- ideal memory.
cachedInvAt ::
  (Eq r, MemOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> CachedSysG r m -> Bool
cachedInvAt wa wsi wwi (Sys core inp mem) (CachedSys ccore cinp srv) =
  ccore == core
    && cinp == inp
    && srvInvAt wa wsi wwi mem srv

-- | The invariant is functional: two ideal systems it relates to the same
-- cached system are equal, at byte @wa@. Quantifying over @wa@ gives equality
-- of the systems. Follows from the server's invariant being functional
-- ('Proof.Cache.Server.Functional.Invariant.srvInvFunctional'): the cores and
-- inputs are each equal to the cached system's, and the memories are the ones
-- the server's invariant relates to the same server.
cachedInvFunctional ::
  (Eq r, MemOps m) =>
  Address -> SetIdx -> WordIdx -> SysG r m -> SysG r m -> CachedSysG r m -> Bool
cachedInvFunctional wa wsi wwi sys1 sys2 csys =
  not premises || conclusion
  where
    premises =
      cachedInvAt wa wsi wwi sys1 csys && cachedInvAt wa wsi wwi sys2 csys

    conclusion =
      sysCore sys1 == sysCore sys2
        && sysInput sys1 == sysInput sys2
        && memReadByte wa (sysMem sys1) == memReadByte wa (sysMem sys2)
