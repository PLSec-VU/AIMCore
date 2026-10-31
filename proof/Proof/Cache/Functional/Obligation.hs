{-# LANGUAGE InstanceSigs #-}
-- | The cache's proof obligations, in one place: the cache's counterpart of
-- "Proof.Core.Functional.Obligation".
--
-- They relate the cached memory to ideal memory ("Memory.Ideal") one request at
-- a time: ideal memory answers in one cycle, the cached memory in
-- 'Proof.Cache.Functional.Driver.driver' + 1 cycles. As the core's obligations
-- build the ISA state from the system ('Proof.Core.Functional.Obligation.isaAt'),
-- these take ideal memory to be the server's memory before the hop; the
-- conclusion compares the memory ideal memory steps to with the server's after
-- the hop.
--
-- From a server that satisfies the invariant the driver is 0, 'penalty' or
-- twice 'penalty' (no, one or two lines missed), so there is one step
-- obligation per hop length, named after it for @'penalty' = 4@. Split by
-- length because Pantomime unrolls recursion only when its count is concrete.
module Proof.Cache.Functional.Obligation
  ( CoherentOps (..),
    mkDataMem,
    baseCase,
    indStep0,
    indStep4,
    indStep8,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core (Input (..), Request)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Ideal (idealServe)
import Memory.Types (MemOps (..))
import Proof.Cache.Functional.Driver (driver)
import Proof.Cache.Functional.Invariant (srvInvAt)
import Proof.Cache.SMT.Raw (RawData (..))
import Proof.SMT.Array (MemArr)
import Types (Address, Word)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Memories a coherent cache can be built from. A class, as memory's
-- operations are ('MemOps'), so that the obligations stay polymorphic in memory
-- and the solver only meets the monomorphic 'mkDataMem'.
class (MemOps m) => CoherentOps m where
  -- | The data array of a coherent cache with tags @t@ in front of @mem@: a
  -- valid line holds memory's words at the address its tag gives, an invalid
  -- line the words of @raw@.
  mkData :: m -> TagArr -> RawData -> DataArr

instance CoherentOps MemArr where
  mkData :: MemArr -> TagArr -> RawData -> DataArr
  mkData = mkDataMem

-- | 'mkData' on the solver's memory. The solver does not run this body:
-- 'Proof.Cache.SMT.Axioms.coherentAxioms' replaces it by
-- 'Proof.Cache.SMT.Coherent.mkDataC'.
{-# OPAQUE mkDataMem #-}
mkDataMem :: MemArr -> TagArr -> RawData -> DataArr
mkDataMem mem t (RawData raw) = DataArr (imap word raw)
  where
    word :: Index 1024 -> Word -> Word
    word i w
      | entryValid e = memReadWord (wordAddr (entryTag e) si wi 0) mem
      | otherwise = w
      where
        idx = fromIntegral i :: BitVector 10
        si = slice d9 d2 idx
        wi = slice d1 d0 idx
        e = tagRead si t

-- | The base case: the server at start, in front of any memory, satisfies the
-- invariant against that memory.
baseCase ::
  (MemOps m) => Address -> SetIdx -> WordIdx -> m -> Bool
baseCase wa wsi wwi mem =
  srvInvAt wa wsi wwi mem (initCacheSrv mem)

-- | The one-cycle hop: a request the cache answers at once.
indStep0 ::
  (CoherentOps m) => Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStep0 wa wsi wwi mem t raw req =
  not premises || conclusion
  where
    ideal = mem
    srv = CacheSrv (Cache4K t (mkData mem t raw)) mem CacheSrvReady

    (ideal', idealAns) = idealServe ideal req

    premises =
      srvInvAt wa wsi wwi ideal srv
        && driver srv req == 0

    conclusion = conclusionN 0 wa wsi wwi ideal' idealAns srv req

-- | The hop of a read that misses one line: 'penalty' + 1 cycle.
indStep4 ::
  (CoherentOps m) => Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStep4 wa wsi wwi mem t raw req =
  not premises || conclusion
  where
    ideal = mem
    srv = CacheSrv (Cache4K t (mkData mem t raw)) mem CacheSrvReady

    (ideal', idealAns) = idealServe ideal req

    premises =
      srvInvAt wa wsi wwi ideal srv
        && driver srv req == penalty

    conclusion = conclusionN penalty wa wsi wwi ideal' idealAns srv req

-- | The hop of a read that misses both its lines: 2 * 'penalty' + 1 cycle.
indStep8 ::
  (CoherentOps m) => Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStep8 wa wsi wwi mem t raw req =
  not premises || conclusion
  where
    ideal = mem
    srv = CacheSrv (Cache4K t (mkData mem t raw)) mem CacheSrvReady

    (ideal', idealAns) = idealServe ideal req

    premises =
      srvInvAt wa wsi wwi ideal srv
        && driver srv req == 2 * penalty

    conclusion = conclusionN (2 * penalty) wa wsi wwi ideal' idealAns srv req

-- | The conclusion of a hop of @n@ + 1 cycles: serve the request, then @n@
-- cycles without one. A stalling server ignores its requests, so this covers
-- any requests in those cycles. The first @n@
-- answers are 'Nothing', the last is ideal memory's answer @idealAns@, and after it
-- the server satisfies the invariant against @ideal'@, the memory ideal memory
-- stepped to. Recursive on @n@, which each obligation fixes to a constant.
conclusionN :: (MemOps m) => Cycles -> Address -> SetIdx -> WordIdx -> m -> Input -> CacheSrv m -> Request -> Bool
conclusionN n wa wsi wwi ideal' idealAns srv req
  | n == 0 = ans == idealAns && srvInvAt wa wsi wwi ideal' srv'
  | otherwise =
      ans == Input Nothing && conclusionN (n - 1) wa wsi wwi ideal' idealAns srv' Nothing
  where (srv', ans) = serve srv req
