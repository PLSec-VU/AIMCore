-- | Ideal memory: the memory the core is specified against, as the ISA is the
-- core it is specified against. It answers every request in the next cycle,
-- and applies a write at once.
module Memory.Ideal
  ( idealServe,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core (Input (..), MemAccess (..))
import Memory.Types (MemOps (..))
import Types (Address, Size (..), Word)
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | One cycle of ideal memory: the memory after the request, and the core's
-- input for the next cycle.
idealServe :: (MemOps m) => m -> Maybe MemAccess -> (m, Input)
idealServe mem req = case req of
  Just (MemFetch addr) -> (mem, Input (Just (memReadWord addr mem)))
  -- A read answers exactly the bytes it asks for.
  Just (MemRead addr size) -> (mem, Input (Just (readSized addr size mem)))
  Just (MemWrite addr size val) -> (memWrite addr size val mem, Input (Just 0))
  Nothing -> (mem, Input (Just 0))

-- | The @size@ bytes at @addr@, the byte at the address lowest, zero-extended.
readSized :: (MemOps m) => Address -> Size -> m -> Word
readSized addr size mem = case size of
  Byte -> zeroExtend b0
  Half -> zeroExtend (b1 ++# b0)
  Word -> b3 ++# b2 ++# b1 ++# b0
  where
    b0 = memReadByte addr mem
    b1 = memReadByte (addr + 1) mem
    b2 = memReadByte (addr + 2) mem
    b3 = memReadByte (addr + 3) mem
