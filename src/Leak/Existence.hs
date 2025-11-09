module Leak.Existence (Fiber, simulatorFor) where

import Pantomime

-- | Fiber is a function such that:
--
-- @
-- presim = \(si, i) ->
--   let (sl, ss) = projection si
--       (sl', x) = leakage sl i
--   in (ss, x)
-- @
--
-- For all x : (si, i), we have presim(fiber(presim(x))) = presim(x)
--
-- In the paper, this is constructed as the Tick function, where
-- fiber(y) = first element of {x ∈ (si, i) | presim(x) = y},
-- but in practice you would want to write one that is more efficient.
type Fiber si i ss l = (ss , l) -> (si , i)

-- | Given a non-interference instance that satisfies
-- tickStateCorrespondence and projectionCoherence,
-- and a fiber function, we can construct a simulator
-- as per lemma 3.18 in the paper.
simulatorFor
  :: NonInterference si sl ss i l o
  -> Fiber si i ss l
  -> (ss , l) -> (ss , o)
simulatorFor NonInterference{..} fiber (ss , l) =
  let (si , i) = fiber (ss , l)
      (si', o) = implementation si i
      (_, ss') = projection si'
  in  (ss', o)