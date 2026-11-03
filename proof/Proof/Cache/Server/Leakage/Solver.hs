-- | The cache's leakage proof, checked symbolically: the cache's counterpart of
-- "Proof.Core.Leakage.Solver".
--
-- The statements live in "Proof.Cache.Server.Leakage.Obligation"; this module
-- only instantiates them, over every memory, request and witness set, and for
-- the step over a server with arbitrary tags, data, memory and state. Each
-- property is named after the statement it checks, with a @P@.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Cache.Server.Leakage.Solver
  ( simSrvReadyIffAnsweredP,
    simSrvDriverMatchesSpecP,
    baseCaseLeakP,
    indStepLeakP,
    results,
  )
where

import Core (Request)
import Memory.Cache4K
import Memory.Cache4KServer (CacheSrv (..), CacheSrvState)
import Proof.Cache.Server.Leakage.Simulator
  ( SimCacheSrv (..),
    SimCacheSrvState (..),
    simSrvReadyIffAnswered,
  )
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.Server.Leakage.Driver (simSrvDriverMatchesSpec)
import Proof.Cache.Server.Leakage.Obligation
import Proof.Cache.Server.Leakage.Model (Obs)
import Proof.Cache.SMT.Axioms (cacheAxioms)
import Proof.SMT.Array (MemArr)
import Proof.SMT.Logged (pantomime)

-- | After a step, the simulator is ready exactly when it has answered.
{-# ANN simSrvReadyIffAnsweredP (Theory cacheAxioms) #-}
simSrvReadyIffAnsweredP ::
  TagArr -> SimCacheSrvState -> Obs -> Pantomime.Bool
simSrvReadyIffAnsweredP tags sst obs =
  Pantomime.boolean $ simSrvReadyIffAnswered (SimCacheSrv tags sst) obs

-- | From a ready simulator, its driver agrees with its operational reading.
{-# ANN simSrvDriverMatchesSpecP (Theory cacheAxioms) #-}
simSrvDriverMatchesSpecP ::
  TagArr -> SimCacheSrvState -> Obs -> Pantomime.Bool
simSrvDriverMatchesSpecP tags sst obs =
  Pantomime.boolean $ simSrvDriverMatchesSpec (SimCacheSrv tags sst) obs

-- | The base case: the server and the simulator at start agree.
{-# ANN baseCaseLeakP (Theory cacheAxioms) #-}
baseCaseLeakP :: SetIdx -> MemArr -> Pantomime.Bool
baseCaseLeakP wsi ma = Pantomime.boolean $ baseCaseLeak wsi ma

-- | The step: in one cycle, the simulator answers exactly when the server does,
-- and the two agree afterwards.
{-# ANN indStepLeakP (Theory cacheAxioms) #-}
indStepLeakP ::
  SetIdx -> TagArr -> DataArr -> MemArr -> CacheSrvState -> Request ->
  Pantomime.Bool
indStepLeakP wsi tags dat ma st req =
  Pantomime.boolean $ indStepLeak wsi (CacheSrv (Cache4K tags dat) ma st) req

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("simSrvReadyIffAnsweredP", $(pantomime 'simSrvReadyIffAnsweredP)),
    ("simSrvDriverMatchesSpecP", $(pantomime 'simSrvDriverMatchesSpecP)),
    ("baseCaseLeakP", $(pantomime 'baseCaseLeakP)),
    ("indStepLeakP", $(pantomime 'indStepLeakP))
  ]
