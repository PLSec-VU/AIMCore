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
  )
where

import Memory.Cache4K
import Memory.Cache4KServer (CacheSrv (..))
import Proof.Core.Functional.Machine (SysG (..))
import Proof.Core.Functional.System (freshSys)
import Proof.Core.SMT.Array (RegArrF (..), freshRA)
import Proof.Memory.Cache4K.CachedCore.Functional.Driver
  ( cachedDriverMatchesSpec,
    cachedDriverValues,
  )
import Proof.Memory.Cache4K.CachedCore.Functional.Invariant (cachedInvFunctional)
import Proof.Memory.Cache4K.CachedCore.Functional.Machine (CachedSysG (..))
import Proof.Memory.Cache4K.CachedCore.Functional.Obligation
import Proof.Memory.Cache4K.SMT.Array (freshData, freshRaw, freshTag)
import Proof.Memory.Cache4K.SMT.Axioms (cacheEmbeddings, coherentEmbeddings)
import Proof.Memory.Cache4K.SMT.Raw (RawData)
import Proof.SMT.Array (MemArr, freshMA)
import Proof.SMT.Symbolic (expectValid)
import SymFC (SymFC (..))
import qualified SymFC.BuiltIn as SymFC
import Types

-- The driver -------------------------------------------------------------------
--
-- The cached system has a symbolic system's core and input, and a cache with
-- arbitrary tags, data, memory and state.

-- | A cached system: the core and input of a fresh system, behind a cache with
-- arbitrary tags, data, memory and state.
freshCachedSys :: SymFC.Sym (CachedSysG RegArrF MemArr)
freshCachedSys = do
  sys <- freshSys
  tags <- freshTag
  dat <- freshData
  cma <- freshMA
  st <- SymFC.fresh
  pure $
    CachedSys (sysCore sys) (sysInput sys) (CacheSrv (Cache4K tags dat) cma st)

-- | On systems the invariant relates to an ideal system, the driver agrees with
-- its operational reading.
{-# ANN cachedDriverMatchesSpecP (SymFC ['cacheEmbeddings]) #-}
cachedDriverMatchesSpecP :: SymFC.Sym ()
cachedDriverMatchesSpecP = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  sys <- freshSys
  tags <- freshTag
  dat <- freshData
  cma <- freshMA
  st <- SymFC.fresh
  let csys =
        CachedSys (sysCore sys) (sysInput sys) (CacheSrv (Cache4K tags dat) cma st)
  expectValid $ cachedDriverMatchesSpec wa wsi wwi sys csys

-- | The driver is 0, 'penalty' or twice 'penalty', on any system.
{-# ANN cachedDriverValuesP (SymFC ['cacheEmbeddings]) #-}
cachedDriverValuesP :: SymFC.Sym ()
cachedDriverValuesP = expectValid . cachedDriverValues =<< freshCachedSys

-- The invariant ----------------------------------------------------------------

-- | The invariant is functional: two ideal systems related to the same cached
-- system are equal. The cached system's core and input come from a third
-- symbolic system; its cache is arbitrary.
{-# ANN cachedInvFunctionalP (SymFC ['cacheEmbeddings]) #-}
cachedInvFunctionalP :: SymFC.Sym ()
cachedInvFunctionalP = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  sys1 <- freshSys
  sys2 <- freshSys
  csys <- freshCachedSys
  expectValid $ cachedInvFunctional wa wsi wwi sys1 sys2 csys

-- The base case ----------------------------------------------------------------

-- | Under 'cacheEmbeddings': the initial cache is the empty one
-- ('Memory.Cache4K.dataZero'), not a constructed one.
{-# ANN baseCaseP (SymFC ['cacheEmbeddings]) #-}
baseCaseP :: SymFC.Sym ()
baseCaseP = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  pc <- SymFC.fresh
  ra <- freshRA
  ma <- freshMA
  expectValid $ baseCase wa wsi wwi pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------
--
-- The ideal system is a fresh system as in the core's proof
-- ('Proof.Core.Functional.System.freshSys'); the cache adds its tags and the
-- contents of its invalid lines.

type HopObligation =
  Address -> SetIdx -> WordIdx -> SysG RegArrF MemArr -> TagArr -> RawData -> Bool

holdsOnEveryHop :: HopObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  sys <- freshSys
  tags <- freshTag
  raw <- freshRaw
  expectValid $ obligation wa wsi wwi sys tags raw

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0P (SymFC ['coherentEmbeddings]) #-}
indStep0P :: SymFC.Sym ()
indStep0P = holdsOnEveryHop indStep0

-- | The hop of a read that misses one line.
{-# ANN indStep4P (SymFC ['coherentEmbeddings]) #-}
indStep4P :: SymFC.Sym ()
indStep4P = holdsOnEveryHop indStep4

-- | The hop of a read that misses both its lines.
{-# ANN indStep8P (SymFC ['coherentEmbeddings]) #-}
indStep8P :: SymFC.Sym ()
indStep8P = holdsOnEveryHop indStep8
