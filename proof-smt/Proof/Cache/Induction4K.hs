-- | The server proof for the 4 KiB cache ("Proof.Cache.Server4K"), checked
-- symbolically, over every tag array, data array, memory and request, with the
-- witnesses quantified. 'cacheArrays' checks the array embeddings, which are
-- trusted, against facts a broken one would get wrong.
module Proof.Cache.Induction4K
  ( cacheArrays,
    srvInitKP,
    srvRefineKP,
    srvLeakKP,
    stallsOnReadsKP,
    results,
  )
where

import qualified Core
import Data.Functor.Identity
import Memory.Cache4K
import Pantomime (Theory (..))
import qualified Pantomime.BuiltIn as Pantomime
import Proof.Cache.Server4K (srvInitK, srvLeakK, srvRefineK, stallsOnReadsK)
import Proof.Machine (CacheSrv (..))
import Proof.SMT.Array (MemArr)
import Proof.SMT.CacheAxioms (cacheAxioms)
import Proof.SMT.Logged (pantomime)
import Clash.Prelude (BitVector)
import Types
import Prelude hiding (Word)

{-# ANN cacheArrays (Theory cacheAxioms) #-}
cacheArrays :: TagArr -> DataArr -> SetIdx -> BitVector 21 -> BitVector 10 -> Word -> Pantomime.Bool
cacheArrays t d s e i w =
  Pantomime.boolean $
    tagRead (tagWrite t s e) s == e
      && dataRead (dataWrite d i w) i == w
      && tagRead tagEmpty s == 0
      && dataRead dataZero i == 0

{-# ANN srvInitKP (Theory cacheAxioms) #-}
srvInitKP :: SetIdx -> WordIdx -> MemArr -> Pantomime.Bool
srvInitKP s w ma = Pantomime.boolean $ srvInitK s w ma

{-# ANN srvRefineKP (Theory cacheAxioms) #-}
srvRefineKP :: Address -> SetIdx -> WordIdx -> TagArr -> DataArr -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
srvRefineKP wa ws ww t d ma q = Pantomime.boolean $ srvRefineK wa ws ww (CacheSrv (Cache4K t d) Nothing ma) q

{-# ANN srvLeakKP (Theory cacheAxioms) #-}
srvLeakKP :: SetIdx -> TagArr -> DataArr -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
srvLeakKP ws t d ma q = Pantomime.boolean $ srvLeakK ws (CacheSrv (Cache4K t d) Nothing ma) q

{-# ANN stallsOnReadsKP (Theory cacheAxioms) #-}
stallsOnReadsKP :: TagArr -> DataArr -> MemArr -> Maybe (Core.MemAccess Identity) -> Pantomime.Bool
stallsOnReadsKP t d ma q = Pantomime.boolean $ stallsOnReadsK (CacheSrv (Cache4K t d) Nothing ma) q

-- | The verdicts, spliced in by the plugin at compile time: 'Nothing' when the
-- property is valid.
results :: [(String, Maybe String)]
results =
  [ ("cacheArrays", $(pantomime 'cacheArrays)),
    ("srvInitKP", $(pantomime 'srvInitKP)),
    ("srvRefineKP", $(pantomime 'srvRefineKP)),
    ("srvLeakKP", $(pantomime 'srvLeakKP)),
    ("stallsOnReadsKP", $(pantomime 'stallsOnReadsKP))
  ]
