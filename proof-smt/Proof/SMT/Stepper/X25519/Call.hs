{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.X25519.Call
  ( decimalSteps
  , pointer
  , ra
  , sp
  , a0
  , a1
  , a2
  , stackTop
  , returnOutsideCode
  , callFrom
  , withPublicWord
  , withPublicWords
  , withSecretWords
  , withStackGuardPointer
  , expectNoLeak
  , expectOutOfFuel
  , expectLeakAt
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

ra, sp, a0, a1, a2 :: RegIdx
ra = 1
sp = 2
a0 = 10
a1 = 11
a2 = 12

stackTop, returnOutsideCode :: Address
stackTop = 0x8000
returnOutsideCode = 0

stackGuardSlot, stackGuard :: Address
stackGuardSlot = 0x5b020
stackGuard = 0x5bc04

pointer :: Address -> Rel Word
pointer = Same . bitCoerce

callFrom :: Code -> Address -> Rel Word -> Rel Word -> Rel Word -> SymFC.Sym RelState
callFrom code entry first second third = do
  isa <- SymFC.fresh @IsaSys
  let start = (sameState code isa) {relPc = entry}
  regs <-
    writeReg a0 first
      =<< writeReg a1 second
      =<< writeReg a2 third
      =<< writeReg sp (pointer stackTop)
      =<< writeReg ra (pointer returnOutsideCode) (relRegs start)
  pure start {relRegs = regs}

withPublicWord :: Address -> Word -> RelState -> SymFC.Sym RelState
withPublicWord addr value st = do
  mem <- writeMem (relPath st) Word addr (Same value) (relMem st)
  pure st {relMem = mem}

withPublicWords :: Address -> [Word] -> RelState -> SymFC.Sym RelState
withPublicWords _ [] st = pure st
withPublicWords addr (value : rest) st = withPublicWord addr value st >>= withPublicWords (addr + 4) rest

withSecretWords :: Address -> Int -> RelState -> SymFC.Sym RelState
withSecretWords _ 0 st = pure st
withSecretWords addr remaining st = do
  secretA <- SymFC.fresh @Word
  secretB <- SymFC.fresh @Word
  withSecretWord addr secretA secretB st >>= withSecretWords (addr + 4) (remaining - 1)

withStackGuardPointer :: RelState -> SymFC.Sym RelState
withStackGuardPointer = withPublicWord stackGuardSlot (bitCoerce stackGuard)

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
