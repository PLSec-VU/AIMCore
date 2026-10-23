-- | Ideal memory: the memory the core is specified against, as the ISA is the
-- core it is specified against. It answers every request in the next cycle,
-- and applies a write at once.
module Memory.Ideal
  ( idealServe,
  )
where

import Core (Input (..), MemAccess (..))
import Memory.Types (MemOps (..))
import Prelude

-- | One cycle of ideal memory: the memory after the request, and the core's
-- input for the next cycle.
idealServe :: (MemOps m) => m -> Maybe MemAccess -> (m, Input)
idealServe mem req = case req of
  Just (MemFetch addr) -> (mem, Input (memReadWord addr mem))
  -- A read. Note 'Memory.Vec.ramRead' ignores the size and always reads a
  -- word; the size-dependent narrowing happens in 'Core.writeback' via
  -- 'loadExtend'. We reproduce that here.
  Just (MemRead addr _) -> (mem, Input (memReadWord addr mem))
  Just (MemWrite addr size val) -> (memWrite addr size val mem, Input 0)
  Nothing -> (mem, Input 0)
