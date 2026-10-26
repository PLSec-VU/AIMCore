-- | The cache's functional proof, checked symbolically: the cache's counterpart
-- of "Proof.Core.Functional.Induction".
--
-- 'baseCase' plus one property per hop length: on a hop of that length, the
-- corresponding obligation in "Proof.Cache.Server.Functional.Obligation" holds. The
-- statements live there; this module only instantiates them, over every tag
-- array, data array, memory, request and witness.
--
-- The server's state is symbolic too: every state is covered, not only the
-- ready one the invariant requires.
--
-- == Running them
--
-- Under Bitwuzla, not Z3:
--
-- > SBV_Z3=/usr/local/bin/bitwuzla SBV_Z3_OPTIONS="--produce-models" stack build
module Proof.Cache.Server.Functional.Induction
  ( cacheArrays,
    baseCase,
    indStep0,
    indStep4,
    indStep8,
    results,
  )
where

import Clash.Prelude (BitVector)
import Core (Request)
import Memory.Cache4K
import Memory.Cache4KServer (CacheSrv (..), CacheSrvState)
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.SMT.Axioms (cacheAxioms)
import Proof.Cache.Server.Functional.Obligation
import Proof.SMT.Array (MemArr)
import Proof.SMT.Logged (pantomime)
import Types
import Prelude hiding (Word)

-- Sanity probe for the trusted embeddings --------------------------------------

-- | The tag and data array embeddings, which are trusted, against facts a
-- broken one would get wrong.
{-# ANN cacheArrays (Theory cacheAxioms) #-}
cacheArrays :: TagArr -> DataArr -> SetIdx -> BitVector 21 -> BitVector 10 -> Word -> Pantomime.Bool
cacheArrays t d s e i w =
  Pantomime.boolean $
    tagRead s (tagWrite s e t) == e
      && dataRead i (dataWrite i w d) == w
      && tagRead s tagEmpty == 0
      && dataRead i dataZero == 0

-- The base case ----------------------------------------------------------------

{-# ANN baseCase (Theory cacheAxioms) #-}
baseCase :: MemArr -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
baseCase ma wa wsi wwi = Pantomime.boolean $ baseCaseObligation wa wsi wwi ma

-- The inductive steps ----------------------------------------------------------

-- | The one-cycle hop: a request the cache answers at once.
{-# ANN indStep0 (Theory cacheAxioms) #-}
indStep0 :: TagArr -> DataArr -> MemArr -> CacheSrvState -> Request -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
indStep0 t d ma st req wa wsi wwi =
  Pantomime.boolean $ indStepObligation0 wa wsi wwi req (CacheSrv (Cache4K t d) ma st)

-- | The hop of a read that misses one line.
{-# ANN indStep4 (Theory cacheAxioms) #-}
indStep4 :: TagArr -> DataArr -> MemArr -> CacheSrvState -> Request -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
indStep4 t d ma st req wa wsi wwi =
  Pantomime.boolean $ indStepObligation4 wa wsi wwi req (CacheSrv (Cache4K t d) ma st)

-- | The hop of a read that misses both its lines.
{-# ANN indStep8 (Theory cacheAxioms) #-}
indStep8 :: TagArr -> DataArr -> MemArr -> CacheSrvState -> Request -> Address -> SetIdx -> WordIdx -> Pantomime.Bool
indStep8 t d ma st req wa wsi wwi =
  Pantomime.boolean $ indStepObligation8 wa wsi wwi req (CacheSrv (Cache4K t d) ma st)

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("cacheArrays", $(pantomime 'cacheArrays)),
    ("baseCase", $(pantomime 'baseCase)),
    ("indStep0", $(pantomime 'indStep0)),
    ("indStep4", $(pantomime 'indStep4)),
    ("indStep8", $(pantomime 'indStep8))
  ]
