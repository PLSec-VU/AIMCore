-- | The cache's obligations, checked symbolically.
--
-- 'stallStep': with a load waiting in writeback and the bus not ready, a cycle
-- of the core is a stutter step ('Proof.Cache.Obligation.stallObligation').
-- 'waitStep': after a data read the core waits
-- ('Proof.Cache.Obligation.waitObligation'). Together, the core is patient. Both
-- over the same symbolic states as the functional proof, without its invariant.
--
-- 'srvInitP', 'srvRefineP' and 'srvLeakP' are the server proof for the cache
-- ("Proof.Cache.Server"): the empty cache starts in the invariant, the hop of a
-- request ends with ideal memory's response, and the ready bits of the hop
-- follow from the tags and the observed request. 'stallsOnReadsP': the cache
-- answers late only to a data read. Over every four-line cache, memory and
-- request.
module Proof.Cache.Induction
  ( stallStep,
    waitStep,
    srvInitP,
    srvRefineP,
    srvLeakP,
    stallsOnReadsP,
    results,
  )
where

import qualified Core
import Data.Functor.Identity
import Memory.Cache (Cache4)
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.Obligation (stallObligation, waitObligation)
import Proof.Cache.Server (srvInit, srvLeak, srvRefine, stallsOnReads)
import Proof.Functional.Induction (KState, sysOf)
import Proof.Machine (CacheSrv (..), SysG (..))
import Proof.SMT.Array
import Proof.SMT.Axioms (arrayAxioms)
import Proof.SMT.Logged (pantomime)
import Types
import Prelude hiding (Word)

{-# ANN stallStep (Theory arrayAxioms) #-}
stallStep :: KState -> Core.Input Identity -> RegArr -> MemArr -> RegIdx -> Pantomime.Bool
stallStep ss i ra ma wr =
  Pantomime.boolean $ stallObligation wr (sysState sys) (sysInput sys)
  where
    sys = sysOf ss i ra ma

{-# ANN waitStep (Theory arrayAxioms) #-}
waitStep :: KState -> Core.Input Identity -> RegArr -> MemArr -> Pantomime.Bool
waitStep ss i ra ma =
  Pantomime.boolean $ waitObligation (sysState sys) (sysInput sys)
  where
    sys = sysOf ss i ra ma

{-# ANN srvInitP (Theory arrayAxioms) #-}
srvInitP :: MemArr -> Pantomime.Bool
srvInitP ma = Pantomime.boolean $ srvInit ma

{-# ANN srvRefineP (Theory arrayAxioms) #-}
srvRefineP :: Address -> Cache4 -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
srvRefineP wa c ma q = Pantomime.boolean $ srvRefine wa (CacheSrv c Nothing ma) q

{-# ANN srvLeakP (Theory arrayAxioms) #-}
srvLeakP :: Cache4 -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
srvLeakP c ma q = Pantomime.boolean $ srvLeak (CacheSrv c Nothing ma) q

{-# ANN stallsOnReadsP (Theory arrayAxioms) #-}
stallsOnReadsP :: Cache4 -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
stallsOnReadsP c ma q = Pantomime.boolean $ stallsOnReads (CacheSrv c Nothing ma) q

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("stallStep", $(pantomime 'stallStep)),
    ("waitStep", $(pantomime 'waitStep)),
    ("srvInitP", $(pantomime 'srvInitP)),
    ("srvRefineP", $(pantomime 'srvRefineP)),
    ("srvLeakP", $(pantomime 'srvLeakP)),
    ("stallsOnReadsP", $(pantomime 'stallsOnReadsP))
  ]
