module Leak where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Instruction hiding (decode, halt)
import qualified Instruction
import Pipe
import Types
import Prelude hiding (Ordering (..), Word, init, not, undefined, (&&), (||))

-- `fromJust` is safe here because the `fetch` stage unconditionally always reads
-- from memory (and its read may just be superseded by the `memory` stage).
obs :: Output -> Address
obs = memAddress . fromJust . getFirst . outMem
