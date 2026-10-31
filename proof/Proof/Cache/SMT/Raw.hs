-- | A raw array of 1024 words, indexed like the cache's data array: the
-- arbitrary contents of a coherent cache's invalid lines
-- ('Proof.Cache.Functional.Obligation.cachedOf'). The term axioms in
-- "Proof.Cache.SMT.Axioms" embed it as 'Proof.Cache.SMT.Array.DataArrSMT'.
--
-- In a module of its own, without the solver's types, so that the obligations
-- can mention it and still build without the solver.
module Proof.Cache.SMT.Raw
  ( RawData (..),
    rawRead,
    rawWrite,
    rawZero,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Types (Word)
import Prelude hiding (Ordering (..), Word, init, log, not, repeat, undefined, (!!), (&&), (++), (||))

newtype RawData = RawData (Vec 1024 Word)

{-# OPAQUE rawRead #-}
rawRead :: BitVector 10 -> RawData -> Word
rawRead i (RawData v) = v !! i

{-# OPAQUE rawWrite #-}
rawWrite :: BitVector 10 -> Word -> RawData -> RawData
rawWrite i w (RawData v) = RawData (replace i w v)

{-# OPAQUE rawZero #-}
rawZero :: RawData
rawZero = RawData (repeat 0)
