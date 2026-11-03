{-# LANGUAGE InstanceSigs #-}
-- | The cache's proof obligations, in one place: the cache's counterpart of
-- "Proof.Core.Functional.Obligation".
--
-- They relate the cached memory to ideal memory ("Memory.Ideal") one request at
-- a time: ideal memory answers in one cycle, the cached memory in
-- 'Proof.Memory.Cache4K.Server.Functional.Driver.srvDriver' + 1 cycles. As the
-- core's obligations build the ISA state from the system
-- ('Proof.Core.Functional.Invariant.isaOf'), these take ideal memory to be the
-- server's memory before the hop; the conclusion compares the memory ideal
-- memory steps to with the server's after the hop.
--
-- Every step starts from a ready server whose cache is coherent by
-- construction ('mkData'): valid lines hold memory's words, invalid lines
-- arbitrary ones. So coherence is not assumed but built in, as the core's ISA
-- state is built from the system.
--
-- From a server that satisfies the invariant the driver is 0, 'penalty' or
-- twice 'penalty' (no, one or two lines missed), so there is one step
-- obligation per hop length, named after it for @'penalty' = 4@. Split by
-- length because Pantomime unrolls recursion only when its count is concrete.
-- A step states only the last answer and the invariant after the hop; that the
-- answers before the last are 'Nothing' is stated apart, as a sanity check
-- ('indStepWaits4', 'indStepWaits8'), since it follows from the driver's
-- agreement with its operational reading.
module Proof.Memory.Cache4K.Server.Functional.Obligation
  ( CoherentOps (..),
    mkDataMem,
    baseCase,
    indStep0,
    indStep4,
    indStepWaits4,
    indStep8,
    indStepWaits8,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.Pipeline (Input (..), Request)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Ideal (idealServe)
import Memory.Types (MemOps (..))
import Proof.Memory.Cache4K.Server.Functional.Driver (srvDriver)
import Proof.Memory.Cache4K.Server.Functional.Invariant (srvInvAt)
import Proof.Memory.Cache4K.SMT.Raw (RawData (..))
import Proof.SMT.Array (MemArr)
import Types (Address, Word)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The coherent cache -----------------------------------------------------------

-- | Memories a coherent cache can be built from. A class, as memory's
-- operations are ('MemOps'), so that the obligations stay polymorphic in memory
-- and the solver only meets the monomorphic 'mkDataMem'.
class (MemOps m) => CoherentOps m where
  -- | The data array of a coherent cache with the tag array @tags@ in front of
  -- @mem@: a valid line holds memory's words at the address its tag gives, an
  -- invalid line the words of @raw@.
  mkData :: m -> TagArr -> RawData -> DataArr

instance CoherentOps MemArr where
  mkData :: MemArr -> TagArr -> RawData -> DataArr
  mkData = mkDataMem

-- | 'mkData' on the solver's memory. The solver does not run this body:
-- 'Proof.Memory.Cache4K.SMT.Axioms.coherentAxioms' replaces it by
-- 'Proof.Memory.Cache4K.SMT.Coherent.mkDataC'.
{-# OPAQUE mkDataMem #-}
mkDataMem :: MemArr -> TagArr -> RawData -> DataArr
mkDataMem mem tags (RawData raw) = DataArr (imap word raw)
  where
    word :: Index 1024 -> Word -> Word
    word i w
      | entryValid e = memReadWord (wordAddr (entryTag e) si wi 0) mem
      | otherwise = w
      where
        idx = fromIntegral i :: BitVector 10
        si = slice d9 d2 idx
        wi = slice d1 d0 idx
        e = tagRead si tags

-- The base case ----------------------------------------------------------------

-- | The base case: the server at start, in front of any memory, satisfies the
-- invariant against that memory.
baseCase ::
  (MemOps m) => Address -> SetIdx -> WordIdx -> m -> Bool
baseCase wa wsi wwi mem =
  srvInvAt wa wsi wwi mem (initCacheSrv mem)

-- The inductive steps ----------------------------------------------------------

-- | The one-cycle hop: a request the cache answers at once.
indStep0 ::
  (CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStep0 wa wsi wwi mem tags raw req =
  not premises || conclusion
  where
    ideal = mem
    srv = CacheSrv (Cache4K tags (mkData mem tags raw)) mem CacheSrvReady

    (ideal', idealAns) = idealServe ideal req
    (srv', ans) = serve srv req

    premises =
      srvInvAt wa wsi wwi ideal srv
        && srvDriver srv req == 0

    conclusion =
      ans == idealAns
        && srvInvAt wa wsi wwi ideal' srv'

-- | The hop of a read that misses one line: 'penalty' + 1 cycle.
indStep4 ::
  (CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStep4 wa wsi wwi mem tags raw req =
  not premises || conclusion
  where
    ideal = mem
    srv = CacheSrv (Cache4K tags (mkData mem tags raw)) mem CacheSrvReady

    (ideal', idealAns) = idealServe ideal req
    (srv', ans) = serveN (fromIntegral penalty) srv req

    premises =
      srvInvAt wa wsi wwi ideal srv
        && srvDriver srv req == fromIntegral penalty

    conclusion =
      ans == idealAns
        && srvInvAt wa wsi wwi ideal' srv'

-- | Sanity check for 'indStep4': during the hop, the first 'penalty' answers
-- are 'Nothing'. Implied by
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvDriverMatchesSpec':
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvDriverSpec' counts the
-- cycles until the first answer.
indStepWaits4 ::
  (CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStepWaits4 wa wsi wwi mem tags raw req =
  not premises || conclusion
  where
    srv = CacheSrv (Cache4K tags (mkData mem tags raw)) mem CacheSrvReady

    premises =
      srvInvAt wa wsi wwi mem srv
        && srvDriver srv req == fromIntegral penalty

    conclusion = stallsFor (fromIntegral penalty) srv req

-- | The hop of a read that misses both its lines: 2 * 'penalty' + 1 cycle.
indStep8 ::
  (CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStep8 wa wsi wwi mem tags raw req =
  not premises || conclusion
  where
    ideal = mem
    srv = CacheSrv (Cache4K tags (mkData mem tags raw)) mem CacheSrvReady

    (ideal', idealAns) = idealServe ideal req
    (srv', ans) = serveN (2 * fromIntegral penalty) srv req

    premises =
      srvInvAt wa wsi wwi ideal srv
        && srvDriver srv req == 2 * fromIntegral penalty

    conclusion =
      ans == idealAns
        && srvInvAt wa wsi wwi ideal' srv'

-- | Sanity check for 'indStep8': during the hop, the first 2 * 'penalty'
-- answers are 'Nothing'. Implied by
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvDriverMatchesSpec':
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvDriverSpec' counts the
-- cycles until the first answer.
indStepWaits8 ::
  (CoherentOps m) =>
  Address -> SetIdx -> WordIdx -> m -> TagArr -> RawData -> Request -> Bool
indStepWaits8 wa wsi wwi mem tags raw req =
  not premises || conclusion
  where
    srv = CacheSrv (Cache4K tags (mkData mem tags raw)) mem CacheSrvReady

    premises =
      srvInvAt wa wsi wwi mem srv
        && srvDriver srv req == 2 * fromIntegral penalty

    conclusion = stallsFor (2 * fromIntegral penalty) srv req

-- Helpers ----------------------------------------------------------------------

-- | Serve a request, then @n@ - 1 cycles without one: are all @n@ answers
-- 'Nothing'?
stallsFor :: (MemOps m) => Int -> CacheSrv m -> Request -> Bool
stallsFor n srv req
  | n == 0 = True
  | otherwise =
      ans == Input Nothing && stallsFor (n - 1) srv' Nothing
      where (srv', ans) = serve srv req

-- | Serve a request, then @n@ cycles without one: the server and the last
-- answer. Recursive on @n@, which each obligation fixes to a constant.
serveN :: (MemOps m) => Int -> CacheSrv m -> Request -> (CacheSrv m, Input)
serveN n srv req
  | n == 0 = (srv', ans)
  | otherwise = serveN (n - 1) srv' Nothing
  where (srv', ans) = serve srv req
