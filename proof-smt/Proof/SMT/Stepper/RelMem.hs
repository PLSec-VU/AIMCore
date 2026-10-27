{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.RelMem
  ( layeredMatchesArray
  , layeredMissingWriteFails
  , layeredPairMatchesArrays
  ) where

import Stepper.Concrete (stepperEmbeddings)
import Stepper.Memory (Memory)
import Stepper.Memory qualified as Memory
import Stepper.Rel (Rel (..), left, right)
import Stepper.RelMem
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Byte)

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

firstAddr :: Address
firstAddr = 0x100

secondAddr :: Address
secondAddr = 0x104

data Writes = Writes
  { symbolicAddr :: Address
  , firstByte :: Byte
  , symbolicByte :: Byte
  , secondByte :: Byte
  , overwriteByte :: Byte
  }

freshWrites :: SymFC.Sym Writes
freshWrites =
  Writes
    <$> SymFC.fresh @Address
    <*> SymFC.fresh @Byte
    <*> SymFC.fresh @Byte
    <*> SymFC.fresh @Byte
    <*> SymFC.fresh @Byte

applyLayered :: Writes -> RelMem -> RelMem
applyLayered w =
  writeByte secondAddr (Same (overwriteByte w))
    . writeByte secondAddr (Same (secondByte w))
    . writeByte (symbolicAddr w) (Same (symbolicByte w))
    . writeByte firstAddr (Same (firstByte w))

applyPlain :: Writes -> Memory 32 8 -> Memory 32 8
applyPlain w =
  Memory.write secondAddr (overwriteByte w)
    . Memory.write secondAddr (secondByte w)
    . Memory.write (symbolicAddr w) (symbolicByte w)
    . Memory.write firstAddr (firstByte w)

applyPlainWithoutSymbolic :: Writes -> Memory 32 8 -> Memory 32 8
applyPlainWithoutSymbolic w =
  Memory.write secondAddr (overwriteByte w)
    . Memory.write secondAddr (secondByte w)
    . Memory.write firstAddr (firstByte w)

readsAgree :: Address -> RelMem -> Memory 32 8 -> Bool
readsAgree probe layered plain =
  left (readByte probe layered) == Memory.read probe plain
    && left (readByte firstAddr layered) == Memory.read firstAddr plain
    && left (readByte secondAddr layered) == Memory.read secondAddr plain
    && leftMem layered == plain

{-# ANN layeredMatchesArray (SymFC stepperEmbeddings) #-}
layeredMatchesArray :: SymFC.Sym ()
layeredMatchesArray = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  w <- freshWrites
  expectValid $ readsAgree probe (applyLayered w (sameMem base)) (applyPlain w base)

{-# ANN layeredMissingWriteFails (SymFC stepperEmbeddings) #-}
layeredMissingWriteFails :: SymFC.Sym ()
layeredMissingWriteFails = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  w <- freshWrites
  expectCounterexample $ readsAgree probe (applyLayered w (sameMem base)) (applyPlainWithoutSymbolic w base)

secretAddr :: Address
secretAddr = 0x200

{-# ANN layeredPairMatchesArrays (SymFC stepperEmbeddings) #-}
layeredPairMatchesArrays :: SymFC.Sym ()
layeredPairMatchesArrays = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  secretA <- SymFC.fresh @Byte
  secretB <- SymFC.fresh @Byte
  w <- freshWrites
  let layered = applyLayered w (writeByte secretAddr (Pair secretA secretB) (sameMem base))
      plainA = applyPlain w (Memory.write secretAddr secretA base)
      plainB = applyPlain w (Memory.write secretAddr secretB base)
  expectValid $
    left (readByte probe layered) == Memory.read probe plainA
      && right (readByte probe layered) == Memory.read probe plainB
      && leftMem layered == plainA
      && rightMem layered == plainB
