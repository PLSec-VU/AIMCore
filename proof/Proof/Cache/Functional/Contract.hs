-- | The contract between the core and the 4 KiB cache in front of memory: what
-- each side promises beyond the cache's proof against ideal memory
-- ("Proof.Cache.Functional.Obligation"), so that the core runs behind the
-- cache as it would behind ideal memory, with stutter steps inserted.
--
-- The two sides are not symmetric: the core makes the requests and the cache
-- answers them.
--
--   * The core waits for what it asked for ('waitsAfterRequest'): after a
--     request, given no answer, it makes no request and its state stays the
--     same.
--   * The cache answers 'Nothing' only while a request is outstanding: a
--     server related to ideal memory by the invariant is ready
--     ('readyUnderInv'), and a ready server asked nothing answers within the
--     cycle ('readyIdleNoWait'). While a request is outstanding, the driver and the
--     cache's step obligations say how it answers: 'Nothing' for as many cycles
--     as the driver counts, then ideal memory's answer.
--
-- The properties are checked in "Proof.Cache.Functional.Solver".
module Proof.Cache.Functional.Contract
  ( isReady,
    readyUnderInv,
    readyIdleNoWait,
    sameCoreAt,
    waitsAfterRequest,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core (Control (..), Input (..), Output (..), StateG (..), circuit)
import Data.Maybe (isNothing)
import Memory.Cache4KServer (CacheSrv (..), CacheSrvState (..))
import Memory.Types (MemOps)
import Proof.Cache.Functional.Driver (driver)
import Proof.Cache.Functional.Invariant (srvInvAt)
import Core.RegFile (RegFileOps (..))
import Types (Address, RegIdx)
import Memory.Cache4K (SetIdx, WordIdx)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- The cache's side --------------------------------------------------------------

-- | Is the server ready for a request: no read waiting for memory?
isReady :: CacheSrv m -> Bool
isReady (CacheSrv _ _ st) = st == CacheSrvReady

-- | A server related to ideal memory by the invariant is ready.
readyUnderInv :: (MemOps m) => Address -> SetIdx -> WordIdx -> m -> CacheSrv m -> Bool
readyUnderInv wa wsi wwi ideal srv = not (srvInvAt wa wsi wwi ideal srv) || isReady srv

-- | A ready server that is asked nothing answers within the cycle.
readyIdleNoWait :: CacheSrv m -> Bool
readyIdleNoWait srv = not (isReady srv) || driver srv Nothing == 0

-- The core's side ---------------------------------------------------------------

-- | Two core states agree at register @wr@: every field is equal, the register
-- file at @wr@. Quantifying over @wr@ gives equality of the states.
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
    && sameControl (stateCtrl s1) (stateCtrl s2)
    && sameMaybe (==) (stateHalt s1) (stateHalt s2)

-- | Field-by-field equality of the control lines. The derived 'Eq' on 'Maybe'
-- has an unreachable branch whose stub SymFC cannot unfold, so the 'Maybe'
-- fields go through 'sameMaybe'.
sameControl :: Control -> Control -> Bool
sameControl c1 c2 =
  sameMaybe (==) (ctrlDeLoadHazard c1) (ctrlDeLoadHazard c2)
    && sameMaybe (==) (ctrlDeStoreHazard c1) (ctrlDeStoreHazard c2)
    && sameMaybe (==) (ctrlExInstr c1) (ctrlExInstr c2)
    && sameMaybe (==) (ctrlExJumpAddr c1) (ctrlExJumpAddr c2)
    && sameMaybe (==) (ctrlExStoreAddrSize c1) (ctrlExStoreAddrSize c2)
    && ctrlMeMemInstr c1 == ctrlMeMemInstr c2
    && sameMaybe (==) (ctrlMeStoreAddrSize c1) (ctrlMeStoreAddrSize c2)
    && sameMaybe (==) (ctrlMeRegFwd c1) (ctrlMeRegFwd c2)
    && sameMaybe (==) (ctrlWbRegFwd c1) (ctrlWbRegFwd c2)
    && ctrlMemNotReady c1 == ctrlMemNotReady c2

sameMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
sameMaybe same (Just a) (Just b) = same a b
sameMaybe _ Nothing Nothing = True
sameMaybe _ _ _ = False

-- | The core waits for what it asked for: after a request, given no answer, it
-- makes no request and stays where it was (at register @wr@).
waitsAfterRequest :: (RegFileOps r) => RegIdx -> StateG r -> Input -> Bool
waitsAfterRequest wr s inp = case outMem o of
  Nothing -> True
  Just _ -> isNothing (outMem o') && sameCoreAt wr s'' s'
  where
    (s', o) = circuit s inp
    (s'', o') = circuit s' (Input Nothing)
