{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.Blake2b
  ( blake2bSecretBlockNoLeak
  , blake2bSecretBlockOneStepShortOutOfFuel
  , blake2bSecretChainingValueNoLeak
  , blake2bSecretBlockPointerLeaks
  ) where

import Clash.Prelude (bitCoerce)
import Stepper (Fuel (..), IsaSys)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.ElfCode (elfCode)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Size (..), Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_blake2b"
    [ ("blake2bCompress", ["blake2b_compress_ref", "memcpy"])
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
exactSteps = decimalSteps [7, 3, 5, 3]

oneStepShort :: Fuel
oneStepShort = decimalSteps [7, 3, 5, 2]

stateAddr, countersAddr, blockAddr, stackTop, returnOutsideCode :: Address
stateAddr = 0x1000
countersAddr = 0x1040
blockAddr = 0x2000
stackTop = 0x8000
returnOutsideCode = 0

otherBlockAddr :: Address
otherBlockAddr = 0x5000

chainingValueWords, counterWords, blockWords :: Int
counterWords = 8
chainingValueWords = 16
blockWords = 32

wordBytes :: Address
wordBytes = 4

ra, sp, a0, a1 :: RegIdx
ra = 1
sp = 2
a0 = 10
a1 = 11

pointer :: Address -> Rel Word
pointer = Same . bitCoerce

withArguments :: Rel Word -> RelState -> RelState
withArguments blockPointer st =
  st
    { relRegs =
        writeReg a0 (pointer stateAddr)
          . writeReg a1 blockPointer
          . writeReg sp (pointer stackTop)
          . writeReg ra (pointer returnOutsideCode)
          $ relRegs st
    }

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

compressCall :: Fuel -> Rel Word -> Bool -> SymFC.Sym Verdict
compressCall fuel blockPointer secretChainingValue = do
  isa <- SymFC.fresh @IsaSys
  let start = (sameState blake2bCompressCode isa) {relPc = blake2bCompressEntry}
  withBlock <- withSecretWords blockAddr blockWords (withArguments blockPointer start)
  withCounters <- withPublicWords countersAddr counterWords withBlock
  withChainingValue <-
    if secretChainingValue
      then withSecretWords stateAddr chainingValueWords withCounters
      else withPublicWords stateAddr chainingValueWords withCounters
  relRun fuel withChainingValue

secretBlockPublicChainingValue :: Fuel -> SymFC.Sym Verdict
secretBlockPublicChainingValue fuel = compressCall fuel (pointer blockAddr) False

secretBlockSecretChainingValue :: Fuel -> SymFC.Sym Verdict
secretBlockSecretChainingValue fuel = compressCall fuel (pointer blockAddr) True

secretBlockPointer :: SymFC.Sym Verdict
secretBlockPointer = compressCall exactSteps (Pair (bitCoerce blockAddr) (bitCoerce otherBlockAddr)) False

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
firstBlockByteLoad = 0x17d28

{-# ANN blake2bSecretBlockNoLeak (SymFC stepperEmbeddings) #-}
blake2bSecretBlockNoLeak :: SymFC.Sym ()
blake2bSecretBlockNoLeak = expectNoLeak $ secretBlockPublicChainingValue exactSteps

{-# ANN blake2bSecretBlockOneStepShortOutOfFuel (SymFC stepperEmbeddings) #-}
blake2bSecretBlockOneStepShortOutOfFuel :: SymFC.Sym ()
blake2bSecretBlockOneStepShortOutOfFuel = expectOutOfFuel $ secretBlockPublicChainingValue oneStepShort

{-# ANN blake2bSecretChainingValueNoLeak (SymFC stepperEmbeddings) #-}
blake2bSecretChainingValueNoLeak :: SymFC.Sym ()
blake2bSecretChainingValueNoLeak = expectNoLeak $ secretBlockSecretChainingValue exactSteps

{-# ANN blake2bSecretBlockPointerLeaks (SymFC stepperEmbeddings) #-}
blake2bSecretBlockPointerLeaks :: SymFC.Sym ()
blake2bSecretBlockPointerLeaks = expectLeakAt firstBlockByteLoad LeakLoadAddress secretBlockPointer
