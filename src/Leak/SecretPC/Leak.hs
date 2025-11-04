module Leak.SecretPC.Leak
  ( circuit
  , In
  , Out
  ) where

import Access
import Core (Input (..))
import Data.Functor.Identity
import Prelude hiding (Word)

type In = Input PubSec
type Out = Input Identity

circuit :: () -> In -> ((), Out)
circuit _ input =
  ((), output)
  where
    output :: Out
    output =
      input
        { inputMem = censor $ inputMem input,
          inputRs1 = censor $ inputRs1 input,
          inputRs2 = censor $ inputRs2 input
        }
