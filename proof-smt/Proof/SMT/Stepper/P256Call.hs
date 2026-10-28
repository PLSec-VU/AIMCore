{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.P256Call
  ( decimalSteps
  , pointer
  , callAt
  , withSecretWords
  , withPublicWords
  , withKnownWords
  , expectNoLeak
  , expectLeakAt
  , expectOutOfFuel
  ) where

import Clash.Prelude (bitCoerce)
import Stepper (Fuel (..), IsaSys)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Size (..), Word)
import Prelude hiding (Word)

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

stackTop, returnOutsideCode, wordBytes :: Address
stackTop = 0x8000
returnOutsideCode = 0
wordBytes = 4

ra, sp, firstArgument :: RegIdx
ra = 1
sp = 2
firstArgument = 10

pointer :: Address -> Rel Word
pointer = Same . bitCoerce

withArgumentsFrom :: RegIdx -> [Rel Word] -> RelRegs -> RelRegs
withArgumentsFrom _ [] regs = regs
withArgumentsFrom reg (argument : rest) regs = withArgumentsFrom (reg + 1) rest (writeReg reg argument regs)

callAt :: Code -> Address -> [Rel Word] -> SymFC.Sym RelState
callAt code entry arguments = do
  isa <- SymFC.fresh @IsaSys
  let start = (sameState code isa) {relPc = entry}
  pure
    start
      { relRegs =
          writeReg sp (pointer stackTop)
            . writeReg ra (pointer returnOutsideCode)
            . withArgumentsFrom firstArgument arguments
            $ relRegs start
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

withKnownWords :: Address -> [Word] -> RelState -> SymFC.Sym RelState
withKnownWords _ [] st = pure st
withKnownWords base (value : rest) st = do
  mem <- writeMem (relPath st) Word base (Same value) (relMem st)
  withKnownWords (base + wordBytes) rest st {relMem = mem}

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
