-- | The server proof for the 4 KiB cache ("Proof.Cache.Server4K"), checked
-- symbolically, over every tag array, data array, memory and request, with the
-- witnesses quantified. The array embeddings are checked in
-- 'Proof.Cache.Server.Functional.Induction.cacheArrays'.
module Proof.Cache.Induction4K
  ( srvInitP,
    srvRefineP,
    srvLeakP,
    stallsOnReadsP,
    results,
  )
where

import qualified Core
import Data.Functor.Identity
import Memory.Cache4K
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.Server4K (srvInit, srvLeak, srvRefine, stallsOnReads)
import Memory.Cache4KServer (CacheSrv (..))
import Proof.SMT.Array (MemArr)
import Proof.Cache.SMT.Axioms (cacheAxioms)
import Proof.SMT.Logged (pantomime)
import Clash.Prelude (BitVector)
import Types
import Prelude hiding (Word)

{-# ANN srvInitP (Theory cacheAxioms) #-}
srvInitP :: SetIdx -> WordIdx -> MemArr -> Pantomime.Bool
srvInitP s w ma = Pantomime.boolean $ srvInit s w ma

{-# ANN srvRefineP (Theory cacheAxioms) #-}
srvRefineP :: Address -> SetIdx -> WordIdx -> TagArr -> DataArr -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
srvRefineP wa ws ww t d ma q = Pantomime.boolean $ srvRefine wa ws ww (CacheSrv (Cache4K t d) Nothing ma) q

{-# ANN srvLeakP (Theory cacheAxioms) #-}
srvLeakP :: SetIdx -> TagArr -> DataArr -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
srvLeakP ws t d ma q = Pantomime.boolean $ srvLeak ws (CacheSrv (Cache4K t d) Nothing ma) q

{-# ANN stallsOnReadsP (Theory cacheAxioms) #-}
stallsOnReadsP :: TagArr -> DataArr -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
stallsOnReadsP t d ma q = Pantomime.boolean $ stallsOnReads (CacheSrv (Cache4K t d) Nothing ma) q

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("srvInitP", $(pantomime 'srvInitP)),
    ("srvRefineP", $(pantomime 'srvRefineP)),
    ("srvLeakP", $(pantomime 'srvLeakP)),
    ("stallsOnReadsP", $(pantomime 'stallsOnReadsP))
  ]
