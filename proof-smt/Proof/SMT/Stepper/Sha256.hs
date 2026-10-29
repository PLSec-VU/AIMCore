{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.Sha256
  ( sha256SecretBlockNoLeak
  , sha256SecretBlockOneStepShortOutOfFuel
  , sha256SecretStateNoLeak
  , sha256SecretBlockPointerLeaks
  , sha256DecodeAgrees
  , sha256DecodeRejectsWrongInstruction
  ) where

import Clash.Prelude (bitCoerce)
import Stepper (Fuel (..), IsaSys)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.ElfCode (elfCode, expectDecodeAgreement, expectDecodeDisagreement, withWrongFirstInstruction)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Size (..), Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_sha256"
    [ ("sha256Transform", ["SHA256_Transform", "memcpy"])
    ])

steps :: Int -> Fuel
steps 0 = Done
steps n = More (steps (n - 1))

plusFuel :: Fuel -> Fuel -> Fuel
plusFuel Done later = later
plusFuel (More fuel) later = More (plusFuel fuel later)

tenfold :: Fuel -> Fuel
tenfold fuel = plusFuel eightfold twofold
  where
    twofold = plusFuel fuel fuel
    fourfold = plusFuel twofold twofold
    eightfold = plusFuel fourfold fourfold

decimalSteps :: [Int] -> Fuel
decimalSteps = digitsAfter Done
  where
    digitsAfter sofar [] = sofar
    digitsAfter sofar (digit : rest) = digitsAfter (plusFuel (steps digit) (tenfold sofar)) rest

exactSteps :: Fuel
exactSteps = decimalSteps [4, 0, 9, 9]

oneStepShort :: Fuel
oneStepShort = decimalSteps [4, 0, 9, 8]

stateAddr, blockAddr, scheduleAddr, scratchAddr, stackTop, returnOutsideCode :: Address
stateAddr = 0x1000
blockAddr = 0x2000
scheduleAddr = 0x3000
scratchAddr = 0x4000
stackTop = 0x8000
returnOutsideCode = 0

otherBlockAddr :: Address
otherBlockAddr = 0x5000

stateWords, blockWords :: Int
stateWords = 8
blockWords = 16

wordBytes :: Address
wordBytes = 4

ra, sp, a0, a1, a2, a3 :: RegIdx
ra = 1
sp = 2
a0 = 10
a1 = 11
a2 = 12
a3 = 13

pointer :: Address -> Rel Word
pointer = Same . bitCoerce

withArguments :: Rel Word -> RelState -> SymFC.Sym RelState
withArguments blockPointer st = do
  regs <-
    writeReg a0 (pointer stateAddr)
      =<< writeReg a1 blockPointer
      =<< writeReg a2 (pointer scheduleAddr)
      =<< writeReg a3 (pointer scratchAddr)
      =<< writeReg sp (pointer stackTop)
      =<< writeReg ra (pointer returnOutsideCode) (relRegs st)
  pure st {relRegs = regs}

withSecretWords :: Address -> Int -> RelState -> SymFC.Sym RelState
withSecretWords _ 0 st = pure st
withSecretWords base count st = do
  wordA <- SymFC.fresh @Word
  wordB <- SymFC.fresh @Word
  next <- withSecretWord base wordA wordB st
  withSecretWords (base + wordBytes) (count - 1) next

withPublicWords :: Address -> Int -> RelState -> SymFC.Sym RelState
withPublicWords _ 0 st = pure st
withPublicWords base count st = do
  value <- SymFC.fresh @Word
  mem <- writeMem (relPath st) Word base (Same value) (relMem st)
  withPublicWords (base + wordBytes) (count - 1) st {relMem = mem}

transformCall :: Fuel -> Rel Word -> Bool -> SymFC.Sym Verdict
transformCall fuel blockPointer secretState = do
  isa <- SymFC.fresh @IsaSys
  let start = (sameState sha256TransformCode isa) {relPc = sha256TransformEntry}
  withBlock <- withSecretWords blockAddr blockWords =<< withArguments blockPointer start
  withState <-
    if secretState
      then withSecretWords stateAddr stateWords withBlock
      else withPublicWords stateAddr stateWords withBlock
  relRun fuel withState

secretBlockPublicState :: Fuel -> SymFC.Sym Verdict
secretBlockPublicState fuel = transformCall fuel (pointer blockAddr) False

secretBlockSecretState :: Fuel -> SymFC.Sym Verdict
secretBlockSecretState fuel = transformCall fuel (pointer blockAddr) True

secretBlockPointer :: SymFC.Sym Verdict
secretBlockPointer = transformCall exactSteps (Pair (bitCoerce blockAddr) (bitCoerce otherBlockAddr)) False

expectNoLeak :: SymFC.Sym Verdict -> SymFC.Sym ()
expectNoLeak run = do
  verdict <- run
  case verdict of
    NoLeak -> pure ()
    _ -> SymFC.halt

expectLeakAt :: Address -> LeakKind -> SymFC.Sym Verdict -> SymFC.Sym ()
expectLeakAt expectedPc expectedKind run = do
  verdict <- run
  case verdict of
    Leaks pc kind | pc == expectedPc && kind == expectedKind -> pure ()
    _ -> SymFC.halt

expectOutOfFuel :: SymFC.Sym Verdict -> SymFC.Sym ()
expectOutOfFuel run = do
  verdict <- run
  case verdict of
    OutOfFuel -> pure ()
    _ -> SymFC.halt

firstBlockByteLoad :: Address
firstBlockByteLoad = 0x10594

{-# ANN sha256SecretBlockNoLeak (SymFCWith 'stepperEmbeddings) #-}
sha256SecretBlockNoLeak :: SymFC.Sym ()
sha256SecretBlockNoLeak = expectNoLeak $ secretBlockPublicState exactSteps

{-# ANN sha256SecretBlockOneStepShortOutOfFuel (SymFCWith 'stepperEmbeddings) #-}
sha256SecretBlockOneStepShortOutOfFuel :: SymFC.Sym ()
sha256SecretBlockOneStepShortOutOfFuel = expectOutOfFuel $ secretBlockPublicState oneStepShort

{-# ANN sha256SecretStateNoLeak (SymFCWith 'stepperEmbeddings) #-}
sha256SecretStateNoLeak :: SymFC.Sym ()
sha256SecretStateNoLeak = expectNoLeak $ secretBlockSecretState exactSteps

{-# ANN sha256SecretBlockPointerLeaks (SymFCWith 'stepperEmbeddings) #-}
sha256SecretBlockPointerLeaks :: SymFC.Sym ()
sha256SecretBlockPointerLeaks = expectLeakAt firstBlockByteLoad LeakLoadAddress secretBlockPointer

{-# ANN sha256DecodeAgrees (SymFCWith 'stepperEmbeddings) #-}
sha256DecodeAgrees :: SymFC.Sym ()
sha256DecodeAgrees = expectDecodeAgreement [sha256TransformDecoded]

{-# ANN sha256DecodeRejectsWrongInstruction (SymFCWith 'stepperEmbeddings) #-}
sha256DecodeRejectsWrongInstruction :: SymFC.Sym ()
sha256DecodeRejectsWrongInstruction = expectDecodeDisagreement [withWrongFirstInstruction sha256TransformDecoded]
