-- | The proof that the core behind the cache refines the core behind ideal
-- memory, checked symbolically: the counterpart of
-- "Proof.Core.Functional.Solver".
--
-- The statements live in "Proof.Memory.Cache4K.CachedCore.Functional.Driver"
-- and "Proof.Memory.Cache4K.CachedCore.Functional.Obligation"; this module only
-- instantiates them, over every core state, input, register file, memory, tag
-- array, request and witness. Each property is named after the statement it
-- checks, with a @P@:
--
--   * the driver against its operational reading
--     ('cachedDriverMatchesSpecP'), and its values, the hop lengths of the
--     steps ('cachedDriverValuesP');
--   * the invariant being functional ('cachedInvFunctionalP');
--   * the base case, and one step per hop length (0, 'penalty',
--     2 * 'penalty'), each starting from a cache coherent by construction
--     ('Proof.Memory.Cache4K.Server.Functional.Obligation.mkData').
--
-- The array embeddings these rely on are checked in
-- "Proof.Memory.Cache4K.Server.Functional.Solver".
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Memory.Cache4K.CachedCore.Functional.Solver
  ( cachedDriverMatchesSpecP,
    cachedDriverValuesP,
    cachedInvFunctionalP,
    baseCaseP,
    indStep0P,
    indStep4P,
    indStep8P,
    results,
  )
where

import qualified Core.Pipeline as Core
import Memory.Cache4K
import Memory.Cache4KServer (CacheSrv (..), CacheSrvState)
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Memory.Cache4K.SMT.Axioms (cacheAxioms, coherentAxioms)
import Proof.Memory.Cache4K.SMT.Raw (RawData)
import Proof.Memory.Cache4K.CachedCore.Functional.Driver
  ( cachedDriverMatchesSpec,
    cachedDriverValues,
  )
import Proof.Memory.Cache4K.CachedCore.Functional.Invariant (cachedInvFunctional)
import Proof.Memory.Cache4K.CachedCore.Functional.Machine (CachedSysG (..))
import Proof.Memory.Cache4K.CachedCore.Functional.Obligation
import Proof.Core.Functional.Machine (SysG (..))
import Proof.Core.Functional.System (KState, sysOf)
import Proof.Core.SMT.Array (RegArr, RegArrF (..))
import Proof.SMT.Array (MemArr)
import Proof.SMT.Logged (pantomime)
import Types

-- The driver -------------------------------------------------------------------
--
-- The cached system has the ideal system's core and input, and a cache with
-- arbitrary tags, data, memory and state.

-- | On systems the invariant relates to an ideal system, the driver agrees with
-- its operational reading.
{-# ANN cachedDriverMatchesSpecP (Theory cacheAxioms) #-}
cachedDriverMatchesSpecP ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> DataArr -> MemArr -> CacheSrvState ->
  Pantomime.Bool
cachedDriverMatchesSpecP wa wsi wwi ss i ra ma tags dat cma st =
  Pantomime.boolean $ cachedDriverMatchesSpec wa wsi wwi sys csys
  where
    sys = sysOf ss i ra ma
    csys =
      CachedSys
        (sysCore sys)
        (sysInput sys)
        (CacheSrv (Cache4K tags dat) cma st)

-- | The driver is 0, 'penalty' or twice 'penalty', on any system.
{-# ANN cachedDriverValuesP (Theory cacheAxioms) #-}
cachedDriverValuesP ::
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> DataArr -> MemArr -> CacheSrvState ->
  Pantomime.Bool
cachedDriverValuesP ss i ra ma tags dat cma st =
  Pantomime.boolean $ cachedDriverValues csys
  where
    sys = sysOf ss i ra ma
    csys =
      CachedSys
        (sysCore sys)
        (sysInput sys)
        (CacheSrv (Cache4K tags dat) cma st)

-- The invariant ----------------------------------------------------------------

-- | The invariant is functional: two ideal systems related to the same cached
-- system are equal. The cached system's core and input come from a third
-- symbolic system; its cache is arbitrary.
{-# ANN cachedInvFunctionalP (Theory cacheAxioms) #-}
cachedInvFunctionalP ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  KState -> Core.Input -> RegArr -> MemArr ->
  KState -> Core.Input -> RegArr ->
  TagArr -> DataArr -> MemArr -> CacheSrvState ->
  Pantomime.Bool
cachedInvFunctionalP
  wa wsi wwi ss1 i1 ra1 ma1 ss2 i2 ra2 ma2 ss i ra tags dat cma st =
    Pantomime.boolean $ cachedInvFunctional wa wsi wwi sys1 sys2 csys
  where
    sys1 = sysOf ss1 i1 ra1 ma1
    sys2 = sysOf ss2 i2 ra2 ma2
    csys0 = sysOf ss i ra cma
    csys =
      CachedSys
        (sysCore csys0)
        (sysInput csys0)
        (CacheSrv (Cache4K tags dat) cma st)

-- The base case ----------------------------------------------------------------

-- | Under 'cacheAxioms': the initial cache is the empty one
-- ('Memory.Cache4K.dataZero'), not a constructed one.
{-# ANN baseCaseP (Theory cacheAxioms) #-}
baseCaseP ::
  Address -> SetIdx -> WordIdx -> Address -> RegArr -> MemArr -> Pantomime.Bool
baseCaseP wa wsi wwi pc ra ma =
  Pantomime.boolean $ baseCase wa wsi wwi pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------
--
-- The ideal system is built from symbolic parts as in the core's proof
-- ('Proof.Core.Functional.System.sysOf'); the cache adds its tags and the
-- contents of its invalid lines.

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0P (Theory coherentAxioms) #-}
indStep0P ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> RawData -> Pantomime.Bool
indStep0P wa wsi wwi ss i ra ma tags raw =
  Pantomime.boolean $ indStep0 wa wsi wwi (sysOf ss i ra ma) tags raw

-- | The hop of a read that misses one line.
{-# ANN indStep4P (Theory coherentAxioms) #-}
indStep4P ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> RawData -> Pantomime.Bool
indStep4P wa wsi wwi ss i ra ma tags raw =
  Pantomime.boolean $ indStep4 wa wsi wwi (sysOf ss i ra ma) tags raw

-- | The hop of a read that misses both its lines.
{-# ANN indStep8P (Theory coherentAxioms) #-}
indStep8P ::
  Address -> SetIdx -> WordIdx ->
  KState -> Core.Input -> RegArr -> MemArr ->
  TagArr -> RawData -> Pantomime.Bool
indStep8P wa wsi wwi ss i ra ma tags raw =
  Pantomime.boolean $ indStep8 wa wsi wwi (sysOf ss i ra ma) tags raw

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("cachedDriverMatchesSpecP", $(pantomime 'cachedDriverMatchesSpecP)),
    ("cachedDriverValuesP", $(pantomime 'cachedDriverValuesP)),
    ("cachedInvFunctionalP", $(pantomime 'cachedInvFunctionalP)),
    ("baseCaseP", $(pantomime 'baseCaseP)),
    ("indStep0P", $(pantomime 'indStep0P)),
    ("indStep4P", $(pantomime 'indStep4P)),
    ("indStep8P", $(pantomime 'indStep8P))
  ]
