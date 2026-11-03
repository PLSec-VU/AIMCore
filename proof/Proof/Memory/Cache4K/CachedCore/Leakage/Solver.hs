-- | Link 2's leakage half, checked symbolically: the counterpart of
-- "Proof.Memory.Cache4K.CachedCore.Functional.Solver".
--
-- The statements live in "Proof.Memory.Cache4K.CachedCore.Leakage.Driver" and
-- "Proof.Memory.Cache4K.CachedCore.Leakage.Obligation"; this module only
-- instantiates them, over every core state, input, register file, memory, tag
-- array and witness. Each property is named after the statement it checks, with
-- a @P@:
--
--   * the simulator's driver against its operational reading
--     ('simCachedDriverMatchesSpecP');
--   * the base case, and one step per hop length (0, 'penalty',
--     2 * 'penalty'), each starting from a cache coherent by construction.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Memory.Cache4K.CachedCore.Leakage.Solver
  ( simCachedDriverMatchesSpecP,
    baseCaseLeakP,
    indStepLeak0P,
    indStepLeak4P,
    indStepLeak8P,
    results,
  )
where

import qualified Core.Pipeline as Core
import Memory.Cache4K
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Memory.Cache4K.CachedCore.Leakage.Driver (simCachedDriverMatchesSpec)
import Proof.Memory.Cache4K.CachedCore.Leakage.Simulator (SimCachedSys (..))
import Proof.Core.Leakage.Model (Obs)
import Proof.Memory.Cache4K.Server.Leakage.Simulator
  ( SimCacheSrv (..),
    SimCacheSrvState,
  )
import Proof.Memory.Cache4K.CachedCore.Leakage.Obligation
import Proof.Memory.Cache4K.SMT.Axioms (cacheAxioms, coherentAxioms)
import Proof.Memory.Cache4K.SMT.Raw (RawData)
import Proof.Core.Functional.System (KState, sysOf)
import Proof.Core.SMT.Array (RegArr, RegArrF (..))
import Proof.SMT.Array (MemArr)
import Proof.SMT.Logged (pantomime)
import Types

-- The driver -------------------------------------------------------------------

-- | From a ready simulator, its driver agrees with its operational reading, for
-- the observation of any request.
{-# ANN simCachedDriverMatchesSpecP (Theory cacheAxioms) #-}
simCachedDriverMatchesSpecP ::
  TagArr -> SimCacheSrvState -> Bool -> Obs -> Pantomime.Bool
simCachedDriverMatchesSpecP tags sst answered obs =
  Pantomime.boolean $ simCachedDriverMatchesSpec sim obs
  where
    sim = SimCachedSys (SimCacheSrv tags sst) answered

-- The base case ----------------------------------------------------------------

-- | The simulator at start agrees with the system at start, for any start
-- PC, register file and memory.
{-# ANN baseCaseLeakP (Theory cacheAxioms) #-}
baseCaseLeakP :: SetIdx -> Address -> RegArr -> MemArr -> Pantomime.Bool
baseCaseLeakP wsi pc ra ma =
  Pantomime.boolean $ baseCaseLeak wsi pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStepLeak0P (Theory coherentAxioms) #-}
indStepLeak0P ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> RawData -> Pantomime.Bool
indStepLeak0P wa wsi wwi ss i ra ma tags raw =
  Pantomime.boolean $ indStepLeak0 wa wsi wwi (sysOf ss i ra ma) tags raw

-- | The hop of a read that misses one line.
{-# ANN indStepLeak4P (Theory coherentAxioms) #-}
indStepLeak4P ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> RawData -> Pantomime.Bool
indStepLeak4P wa wsi wwi ss i ra ma tags raw =
  Pantomime.boolean $ indStepLeak4 wa wsi wwi (sysOf ss i ra ma) tags raw

-- | The hop of a read that misses both its lines.
{-# ANN indStepLeak8P (Theory coherentAxioms) #-}
indStepLeak8P ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> RawData -> Pantomime.Bool
indStepLeak8P wa wsi wwi ss i ra ma tags raw =
  Pantomime.boolean $ indStepLeak8 wa wsi wwi (sysOf ss i ra ma) tags raw

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("simCachedDriverMatchesSpecP", $(pantomime 'simCachedDriverMatchesSpecP)),
    ("baseCaseLeakP", $(pantomime 'baseCaseLeakP)),
    ("indStepLeak0P", $(pantomime 'indStepLeak0P)),
    ("indStepLeak4P", $(pantomime 'indStepLeak4P)),
    ("indStepLeak8P", $(pantomime 'indStepLeak8P))
  ]
