-- | The cache's leakage proof, checked symbolically: the cache's counterpart of
-- "Proof.Core.Leakage.Solver".
--
-- The statements live in "Proof.Memory.Cache4K.Server.Leakage.Obligation"; this
-- module only instantiates them, over every memory, request and witness set,
-- and for the step over a server with arbitrary tags, data, memory and state.
-- Each property is named after the statement it checks, with a @P@.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Memory.Cache4K.Server.Leakage.Solver
  ( simSrvReadyIffAnsweredP,
    simSrvDriverMatchesSpecP,
    srvSimInvFunctionalP,
    baseCaseLeakP,
    indStepLeakP,
  )
where

import Memory.Cache4K
import Memory.Cache4KServer (CacheSrv (..))
import Proof.Memory.Cache4K.SMT.Array (freshData, freshTag)
import Proof.Memory.Cache4K.SMT.Axioms (cacheEmbeddings)
import Proof.Memory.Cache4K.Server.Leakage.Driver (simSrvDriverMatchesSpec)
import Proof.Memory.Cache4K.Server.Leakage.Invariant (srvSimInvFunctional)
import Proof.Memory.Cache4K.Server.Leakage.Obligation
import Proof.Memory.Cache4K.Server.Leakage.Simulator
  ( SimCacheSrv (..),
    simSrvReadyIffAnswered,
  )
import Proof.SMT.Array (MemArr, freshMA)
import Proof.SMT.Symbolic (expectValid)
import SymFC (SymFC (..))
import qualified SymFC.BuiltIn as SymFC

-- | A simulator with arbitrary tags and state.
freshSimServer :: SymFC.Sym SimCacheSrv
freshSimServer = SimCacheSrv <$> freshTag <*> SymFC.fresh

-- | A server with arbitrary tags, data, memory and state.
freshServer :: SymFC.Sym (CacheSrv MemArr)
freshServer = do
  tags <- freshTag
  dat <- freshData
  ma <- freshMA
  st <- SymFC.fresh
  pure (CacheSrv (Cache4K tags dat) ma st)

-- | After a step, the simulator is ready exactly when it has answered.
{-# ANN simSrvReadyIffAnsweredP (SymFC ['cacheEmbeddings]) #-}
simSrvReadyIffAnsweredP :: SymFC.Sym ()
simSrvReadyIffAnsweredP = do
  sim <- freshSimServer
  obs <- SymFC.fresh
  expectValid $ simSrvReadyIffAnswered sim obs

-- | From a ready simulator, its driver agrees with its operational reading.
{-# ANN simSrvDriverMatchesSpecP (SymFC ['cacheEmbeddings]) #-}
simSrvDriverMatchesSpecP :: SymFC.Sym ()
simSrvDriverMatchesSpecP = do
  sim <- freshSimServer
  obs <- SymFC.fresh
  expectValid $ simSrvDriverMatchesSpec sim obs

-- | The invariant is functional: two simulators it relates to the same server
-- agree at the witness set.
{-# ANN srvSimInvFunctionalP (SymFC ['cacheEmbeddings]) #-}
srvSimInvFunctionalP :: SymFC.Sym ()
srvSimInvFunctionalP = do
  wsi <- SymFC.fresh
  sim1 <- freshSimServer
  sim2 <- freshSimServer
  srv <- freshServer
  expectValid $ srvSimInvFunctional wsi sim1 sim2 srv

-- | The base case: the server and the simulator at start agree.
{-# ANN baseCaseLeakP (SymFC ['cacheEmbeddings]) #-}
baseCaseLeakP :: SymFC.Sym ()
baseCaseLeakP = do
  wsi <- SymFC.fresh
  ma <- freshMA
  expectValid $ baseCaseLeak wsi ma

-- | The step: in one cycle, the simulator answers exactly when the server does,
-- and the two agree afterwards.
{-# ANN indStepLeakP (SymFC ['cacheEmbeddings]) #-}
indStepLeakP :: SymFC.Sym ()
indStepLeakP = do
  wsi <- SymFC.fresh
  srv <- freshServer
  req <- SymFC.fresh
  expectValid $ indStepLeak wsi srv req
