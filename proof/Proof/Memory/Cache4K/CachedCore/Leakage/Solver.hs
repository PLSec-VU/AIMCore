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
    cachedSimInvFunctionalP,
    baseCaseLeakP,
    indStepLeak0P,
    indStepLeak4P,
    indStepLeak8P,
  )
where

import Memory.Cache4K
import Memory.Cache4KServer (CacheSrv (..))
import Proof.Core.Functional.Machine (SysG (..))
import Proof.Core.Functional.System (freshSys)
import Proof.Core.SMT.Array (RegArrF (..), freshRA)
import Proof.Memory.Cache4K.CachedCore.Functional.Machine (CachedSysG (..))
import Proof.Memory.Cache4K.CachedCore.Leakage.Driver (simCachedDriverMatchesSpec)
import Proof.Memory.Cache4K.CachedCore.Leakage.Invariant (cachedSimInvFunctional)
import Proof.Memory.Cache4K.CachedCore.Leakage.Obligation
import Proof.Memory.Cache4K.CachedCore.Leakage.Simulator (SimCachedSys (..))
import Proof.Memory.Cache4K.SMT.Array (freshData, freshRaw, freshTag)
import Proof.Memory.Cache4K.SMT.Axioms (cacheEmbeddings, coherentEmbeddings)
import Proof.Memory.Cache4K.SMT.Raw (RawData)
import Proof.Memory.Cache4K.Server.Leakage.Simulator (SimCacheSrv (..))
import Proof.SMT.Array (MemArr, freshMA)
import Proof.SMT.Symbolic (expectValid)
import SymFC (SymFC (..))
import qualified SymFC.BuiltIn as SymFC
import Types

-- | A simulator with arbitrary tags, state and answer flag.
freshSimCachedSys :: SymFC.Sym SimCachedSys
freshSimCachedSys =
  SimCachedSys <$> (SimCacheSrv <$> freshTag <*> SymFC.fresh) <*> SymFC.fresh

-- The driver -------------------------------------------------------------------

-- | From a ready simulator, its driver agrees with its operational reading, for
-- the observation of any request.
{-# ANN simCachedDriverMatchesSpecP (SymFC ['cacheEmbeddings]) #-}
simCachedDriverMatchesSpecP :: SymFC.Sym ()
simCachedDriverMatchesSpecP = do
  sim <- freshSimCachedSys
  obs <- SymFC.fresh
  expectValid $ simCachedDriverMatchesSpec sim obs

-- The invariant ----------------------------------------------------------------

-- | The invariant is functional: two simulators it relates to the same cached
-- system agree at the witness set. The cached system is a symbolic system
-- behind an arbitrary cache.
{-# ANN cachedSimInvFunctionalP (SymFC ['cacheEmbeddings]) #-}
cachedSimInvFunctionalP :: SymFC.Sym ()
cachedSimInvFunctionalP = do
  wsi <- SymFC.fresh
  sim1 <- freshSimCachedSys
  sim2 <- freshSimCachedSys
  sys <- freshSys
  tags <- freshTag
  dat <- freshData
  st <- SymFC.fresh
  let csys =
        CachedSys
          (sysCore sys)
          (sysInput sys)
          (CacheSrv (Cache4K tags dat) (sysMem sys) st)
  expectValid $ cachedSimInvFunctional wsi sim1 sim2 csys

-- The base case ----------------------------------------------------------------

-- | The simulator at start agrees with the system at start, for any start
-- PC, register file and memory.
{-# ANN baseCaseLeakP (SymFC ['cacheEmbeddings]) #-}
baseCaseLeakP :: SymFC.Sym ()
baseCaseLeakP = do
  wsi <- SymFC.fresh
  pc <- SymFC.fresh
  ra <- freshRA
  ma <- freshMA
  expectValid $ baseCaseLeak wsi pc (RegArrF ra) ma

-- The inductive steps ----------------------------------------------------------

type HopLeakObligation =
  Address -> SetIdx -> WordIdx -> SysG RegArrF MemArr -> TagArr -> RawData -> Bool

holdsOnEveryHop :: HopLeakObligation -> SymFC.Sym ()
holdsOnEveryHop obligation = do
  wa <- SymFC.fresh
  wsi <- SymFC.fresh
  wwi <- SymFC.fresh
  sys <- freshSys
  tags <- freshTag
  raw <- freshRaw
  expectValid $ obligation wa wsi wwi sys tags raw

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStepLeak0P (SymFC ['coherentEmbeddings]) #-}
indStepLeak0P :: SymFC.Sym ()
indStepLeak0P = holdsOnEveryHop indStepLeak0

-- | The hop of a read that misses one line.
{-# ANN indStepLeak4P (SymFC ['coherentEmbeddings]) #-}
indStepLeak4P :: SymFC.Sym ()
indStepLeak4P = holdsOnEveryHop indStepLeak4

-- | The hop of a read that misses both its lines.
{-# ANN indStepLeak8P (SymFC ['coherentEmbeddings]) #-}
indStepLeak8P :: SymFC.Sym ()
indStepLeak8P = holdsOnEveryHop indStepLeak8
