module Leak.SecretPC.Leak
  ( circuit,
    init,
    In,
    Out,
    State,
  )
where

import Access
import Core (Input (..))
import Prelude hiding (Word, init)

type In = Input PubSec

type State = ()

type Out = Input PubSec

circuit :: State -> In -> (State, Out)
circuit _ input =
  ((), output)
  where
    output :: Out
    output =
      input
        { inputMem = censor $ inputMem input
        }

init :: State
init = ()
