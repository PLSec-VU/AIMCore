{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}

module Stepper
  (
  ) where

import ISA (IsaStateG (..), isaStep, StepG (Next))
-- import RegFile (RegFile, initRF)
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import SymFC.Base qualified as Base
import SymFC.Clash qualified as Clash
import Stepper.Memory (Memory, RegFile)

type IsaSys = IsaStateG RegFile (Memory 32 8)

-- initial :: IsaSys
-- initial = IsaState
--   { isaPc = 0
--   , isaRegFile = RegFile Memory.empty
--   , isaMem = Memory.empty
--   }

{-# ANN theory (SymFC $ Base.embeddings <> Clash.embeddings) #-}
theory :: SymFC.Sym ()
theory = do
  isa0 <- SymFC.fresh @IsaSys
  let isa1 = isaStep isa0
  let prop = Next isa0 == isa1

  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.Instance -> pure ()
    _ -> SymFC.halt
