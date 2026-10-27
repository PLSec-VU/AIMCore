{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper
  ( stepChangesState
  , addiSetsX1
  , addiWrongValueFails
  , chainUnrolled
  , chainUnrolledWrongValueFails
  , chainFuel
  , chainFuelWrongValueFails
  , chainIntFuel
  , chainIntFuelWrongValueFails
  ) where

import ISA (IsaStateG (..), StepG (..), isaStep)
import Memory.Types (MemOps (..))
import RegFile (RegFileOps (..))
import Stepper (Fuel (..), IsaSys, IsaSysStep)
import SymFC (SymFC (..))
import SymFC.Base qualified as Base
import SymFC.BuiltIn qualified as SymFC
import SymFC.Clash qualified as Clash
import Types (Address, Size (..), Word)
import Prelude hiding (Word)

expectValid :: Bool -> SymFC.Sym ()
expectValid prop = do
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.None -> pure ()
    _ -> SymFC.halt

expectCounterexample :: Bool -> SymFC.Sym ()
expectCounterexample prop = do
  result <- SymFC.valid $ SymFC.boolean prop
  case result of
    SymFC.Instance -> pure ()
    _ -> SymFC.halt

{-# ANN stepChangesState (SymFC $ Base.embeddings <> Clash.embeddings) #-}
stepChangesState :: SymFC.Sym ()
stepChangesState = do
  isa0 <- SymFC.fresh @IsaSys
  expectCounterexample $ Next isa0 == isaStep isa0


addiX1X0Imm5 :: Word
addiX1X0Imm5 = 0x00500093

addiAtZero :: IsaSys -> IsaSys
addiAtZero isa = isa {isaPc = 0, isaMem = memWriteWord Word 0 addiX1X0Imm5 (isaMem isa)}

stepSetsX1To :: Word -> IsaSys -> Bool
stepSetsX1To value isa =
  isaStep isa == Next isa {isaPc = 4, isaRegFile = modifyRFg 1 (pure value) (isaRegFile isa)}

{-# ANN addiSetsX1 (SymFC $ Base.embeddings <> Clash.embeddings) #-}
addiSetsX1 :: SymFC.Sym ()
addiSetsX1 = do
  isa <- addiAtZero <$> SymFC.fresh @IsaSys
  expectValid $ stepSetsX1To 5 isa

{-# ANN addiWrongValueFails (SymFC $ Base.embeddings <> Clash.embeddings) #-}
addiWrongValueFails :: SymFC.Sym ()
addiWrongValueFails = do
  isa <- addiAtZero <$> SymFC.fresh @IsaSys
  expectCounterexample $ stepSetsX1To 6 isa


addiX2X1Imm3 :: Word
addiX2X1Imm3 = 0x00308113

addX3X1X2 :: Word
addX3X1X2 = 0x002081B3

chainEnd :: Address
chainEnd = 12

chainAtZero :: IsaSys -> IsaSys
chainAtZero isa = isa {isaPc = 0, isaMem = program}
  where
    program =
      memWriteWord Word 8 addX3X1X2 $
        memWriteWord Word 4 addiX2X1Imm3 $
          memWriteWord Word 0 addiX1X0Imm5 (isaMem isa)

chainSetsX3To :: Word -> IsaSys -> IsaSysStep
chainSetsX3To x3 isa = Next isa {isaPc = chainEnd, isaRegFile = registers}
  where
    registers =
      modifyRFg 3 (pure x3) $
        modifyRFg 2 (pure 8) $
          modifyRFg 1 (pure 5) (isaRegFile isa)

andThenStep :: IsaSysStep -> IsaSysStep
andThenStep = \case
  Next isa -> isaStep isa
  IsaHalted -> IsaHalted

threeStepsUnrolled :: IsaSys -> IsaSysStep
threeStepsUnrolled = andThenStep . andThenStep . isaStep

{-# ANN chainUnrolled (SymFC $ Base.embeddings <> Clash.embeddings) #-}
chainUnrolled :: SymFC.Sym ()
chainUnrolled = do
  isa <- chainAtZero <$> SymFC.fresh @IsaSys
  expectValid $ threeStepsUnrolled isa == chainSetsX3To 13 isa

{-# ANN chainUnrolledWrongValueFails (SymFC $ Base.embeddings <> Clash.embeddings) #-}
chainUnrolledWrongValueFails :: SymFC.Sym ()
chainUnrolledWrongValueFails = do
  isa <- chainAtZero <$> SymFC.fresh @IsaSys
  expectCounterexample $ threeStepsUnrolled isa == chainSetsX3To 14 isa

threeSteps :: Fuel
threeSteps = More (More (More Done))

stepWithFuel :: Fuel -> IsaSys -> IsaSysStep
stepWithFuel Done isa = Next isa
stepWithFuel (More fuel) isa = case isaStep isa of
  Next isa' -> stepWithFuel fuel isa'
  IsaHalted -> IsaHalted

{-# ANN chainFuel (SymFC $ Base.embeddings <> Clash.embeddings) #-}
chainFuel :: SymFC.Sym ()
chainFuel = do
  isa <- chainAtZero <$> SymFC.fresh @IsaSys
  expectValid $ stepWithFuel threeSteps isa == chainSetsX3To 13 isa

{-# ANN chainFuelWrongValueFails (SymFC $ Base.embeddings <> Clash.embeddings) #-}
chainFuelWrongValueFails :: SymFC.Sym ()
chainFuelWrongValueFails = do
  isa <- chainAtZero <$> SymFC.fresh @IsaSys
  expectCounterexample $ stepWithFuel threeSteps isa == chainSetsX3To 14 isa

stepTimes :: Int -> IsaSys -> IsaSysStep
stepTimes n isa
  | n <= 0 = Next isa
  | otherwise = case isaStep isa of
      Next isa' -> stepTimes (n - 1) isa'
      IsaHalted -> IsaHalted

{-# ANN chainIntFuel (SymFC $ Base.embeddings <> Clash.embeddings) #-}
chainIntFuel :: SymFC.Sym ()
chainIntFuel = do
  isa <- chainAtZero <$> SymFC.fresh @IsaSys
  expectValid $ stepTimes 3 isa == chainSetsX3To 13 isa

{-# ANN chainIntFuelWrongValueFails (SymFC $ Base.embeddings <> Clash.embeddings) #-}
chainIntFuelWrongValueFails :: SymFC.Sym ()
chainIntFuelWrongValueFails = do
  isa <- chainAtZero <$> SymFC.fresh @IsaSys
  expectCounterexample $ stepTimes 3 isa == chainSetsX3To 14 isa
