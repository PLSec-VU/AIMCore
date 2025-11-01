module Leak.SecretPC.Leak (circuit) where

import Access
import Core (Input (..))
import Data.Functor.Identity
import Types
import Prelude hiding (Word)

type Public = Identity

circuit :: () -> Input PubSec -> ((), Input Public)
circuit _ input =
  ((), output)
  where
    censor :: PubSec Word -> Public Word
    censor = pure . fromPubSec 0

    output :: Input Public
    output =
      input
        { inputMem = censor $ inputMem input,
          inputRs1 = censor $ inputRs1 input,
          inputRs2 = censor $ inputRs2 input
        }
