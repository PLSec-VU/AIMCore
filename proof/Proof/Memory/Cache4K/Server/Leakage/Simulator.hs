-- | The simulator for the cache's leakage proof: what an attacker on the bus
-- between core and server can reconstruct of the server. It knows only the
-- cache's tags, and it is driven only by the observations of the requests
-- ('Proof.Core.Leakage.Model.obsReq'), never by data.
--
-- It runs cycle by cycle, as the server does ('simServe' against
-- 'Memory.Cache4KServer.serve'): from the tags and the observation of a cycle's
-- request it computes whether the server answers in that cycle, and the tags
-- and stall counter afterwards. The leakage proof
-- ("Proof.Memory.Cache4K.Server.Leakage.Obligation") shows that the server
-- agrees, so the stalls -- the only thing the cache adds to the bus trace --
-- depend on the observations alone.
--
-- Readiness is defined by behaviour, as for the server ('simSrvReady'), with
-- its property ('simSrvReadyIffAnswered').
module Proof.Memory.Cache4K.Server.Leakage.Simulator
  ( SimCacheSrvState (..),
    SimCacheSrv (..),
    simInitCacheSrv,
    simServe,
    simSrvReady,
    simSrvReadyIffAnswered,
    tagHit,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Memory.Cache4K
import Memory.Cache4KServer (Cycles, penalty)
import Proof.Core.Leakage.Model (Obs (..))
import Types (Address, Size (..))
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | The simulator's counterpart of 'Memory.Cache4KServer.CacheSrvState': ready,
-- or waiting for memory with the cycles left. Unlike the server, it does not
-- know the word it will answer.
data SimCacheSrvState = SimCacheSrvReady | SimCacheSrvStall Cycles
  deriving (Show, Eq)

-- | The simulator's counterpart of 'Memory.Cache4KServer.CacheSrv': the cache's
-- tags, without its data or memory.
data SimCacheSrv = SimCacheSrv
  { -- | The tag entries, one per set, as in the cache.
    simTagArr :: TagArr,
    -- | Ready, or waiting for memory with the cycles left.
    simSrvState :: SimCacheSrvState
  }
  deriving (Show, Eq)

-- | The simulator at start, as 'Memory.Cache4KServer.initCacheSrv': no line
-- valid and no read waiting.
simInitCacheSrv :: SimCacheSrv
simInitCacheSrv = SimCacheSrv tagEmpty SimCacheSrvReady

-- | One cycle, as 'Memory.Cache4KServer.serve', from the observation of the
-- cycle's request ('Nothing' for no input): whether the server answers in this
-- cycle ('Proof.Memory.Cache4K.Server.Leakage.Model.obsAns'), and the simulator
-- afterwards. A read that misses fills the line's tag and stalls 'penalty'
-- cycles per missed line; everything else is answered at once.
simServe :: SimCacheSrv -> Obs -> (SimCacheSrv, Bool)
simServe (SimCacheSrv t st) obs =
  case st of
    SimCacheSrvStall n
      | n > 0 ->
          (SimCacheSrv t (SimCacheSrvStall (n - 1)), False)
      | otherwise ->
          (SimCacheSrv t SimCacheSrvReady, True)
    SimCacheSrvReady -> case obs of
      DataRead addr size ->
        let off =
              case size of
                Byte -> 0
                Half -> 1
                Word -> 3

            hit1stLine = tagHit addr t
            penalty1stLine = if hit1stLine then 0 else penalty
            t1 = if hit1stLine then t else tagFill addr t

            hit2ndLine = tagHit (addr + off) t1
            penalty2ndLine = if hit2ndLine then 0 else penalty
            t2 = if hit2ndLine then t1 else tagFill (addr + off) t1

            n = penalty1stLine + penalty2ndLine

        in if n > 0
             then (SimCacheSrv t2 (SimCacheSrvStall (n - 1)), False)
             else (SimCacheSrv t2 SimCacheSrvReady, True)
      _ -> (SimCacheSrv t SimCacheSrvReady, True)

-- | Does the line of an address hold its block?
tagHit :: Address -> TagArr -> Bool
tagHit addr t = entryValid e && entryTag e == tagOf addr
  where
    e = tagRead (setOf addr) t

-- | Mark the line of an address as holding its block: valid, with its tag.
{- HLINT ignore tagFill "Eta reduce" -}
tagFill :: Address -> TagArr -> TagArr
tagFill addr t = tagWrite (setOf addr) ((1 :: BitVector 1) ++# tagOf addr) t

-- | Is the simulator ready for a request? As
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvReady', defined by its
-- behaviour: given no input, it answers within the cycle and stays the same.
simSrvReady :: SimCacheSrv -> Bool
simSrvReady sim =
  let (sim', ans) = simServe sim NoAccess
  in sim' == sim && ans

-- | After a step, the simulator is ready exactly when it has answered: the
-- counterpart of
-- 'Proof.Memory.Cache4K.Server.Functional.Contract.srvReadyIffAnswered'.
simSrvReadyIffAnswered :: SimCacheSrv -> Obs -> Bool
simSrvReadyIffAnswered sim obs =
  let (sim', ans) = simServe sim obs
  in simSrvReady sim' == ans
