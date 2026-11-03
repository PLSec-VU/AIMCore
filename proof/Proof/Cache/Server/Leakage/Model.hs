-- | What an attacker sees of the cache server: the cache's counterpart of
-- "Proof.Core.Leakage.Model".
--
-- The attacker watches the bus between the core and the server. In one cycle
-- it sees:
--
--   * the request without its data ('obsReq', the core's observation: the same
--     bus), and
--   * whether the server answers, not the answer itself ('obsAns').
--
-- The server's internals -- the cache's contents, line fills, write-through to
-- memory, the stall counter -- are hidden. What the cache adds to the bus is
-- timing: the cycles in which it does not answer.
module Proof.Cache.Server.Leakage.Model
  ( Obs (..),
    obsReq,
    obsAns,
  )
where

import Core (Input (..))
import Data.Maybe (isJust)
import Proof.Core.Leakage.Model (Obs (..), obsReq)

-- | What the attacker sees of the server's answer in one cycle: whether there
-- is one.
obsAns :: Input -> Bool
obsAns ans = isJust (inputMem ans)
