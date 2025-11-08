{-# LANGUAGE StandaloneDeriving #-}

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
import Data.Functor.Identity
import Prelude hiding (Word, init)

type In = Input PubSec

type State = ()

type Out = Input PubSec

deriving instance Eq Out

circuit :: State -> In -> (State, Out)
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

init :: State
init = ()
