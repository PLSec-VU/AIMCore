{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Fresh symbolic values of the pipeline's types, for the induction proofs.
--
-- Each instance uses the 'Generic' default of 'SymFC.Symbolic'. They live here
-- rather than beside the types so that the synthesisable modules stay free of
-- the plugin.
module Proof.SMT.Symbolic
  ( expectValid,
  )
where

import Clash.Prelude (Generic)
import qualified Core
import Core.Instruction
import Memory.Cache4KServer (CacheSrvState (..))
import Proof.Core.Leakage.Model (Leak)
import Proof.Core.Leakage.Simulator (SimControl, SimExInstrType, SimState)
import qualified SymFC.Base ()
import qualified SymFC.BuiltIn as SymFC
import qualified SymFC.Clash ()
import Types (Size)

instance SymFC.Symbolic Size

instance SymFC.Symbolic Arith

instance SymFC.Symbolic Comparison

instance SymFC.Symbolic Sign

instance SymFC.Symbolic UBase

instance SymFC.Symbolic Env

instance SymFC.Symbolic IOperation

instance SymFC.Symbolic Reason4Stall

instance SymFC.Symbolic Instruction

instance SymFC.Symbolic Core.HaltState

instance SymFC.Symbolic Core.Control

instance SymFC.Symbolic Core.Input

instance SymFC.Symbolic Core.MemAccess

deriving instance Generic CacheSrvState

instance SymFC.Symbolic CacheSrvState

instance SymFC.Symbolic Leak

instance SymFC.Symbolic SimExInstrType

instance SymFC.Symbolic SimControl

instance SymFC.Symbolic SimState

-- | Fail the build unless the property holds for every value of its fresh
-- inputs.
expectValid :: Bool -> SymFC.Sym ()
expectValid prop = do
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.None -> pure ()
    _ -> SymFC.halt
