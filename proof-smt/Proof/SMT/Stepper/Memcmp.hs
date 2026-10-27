{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.Memcmp
  ( memcmpLeaks
  , sodiumMemcmpNoLeak
  , sodiumMemcmpShortRunOutOfFuel
  ) where

import Clash.Prelude (bitCoerce)
import Proof.SMT.Bench.Memcmp
import Stepper (Fuel (..), IsaSys)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Word)
import Prelude hiding (Word)

steps :: Int -> Fuel
steps 0 = Done
steps n = More (steps (n - 1))

stepLimit :: Fuel
stepLimit = steps 200

tooFewSteps :: Fuel
tooFewSteps = steps 20

secretBuf, publicBuf, stackTop, returnOutsideCode :: Address
secretBuf = 0x1000
publicBuf = 0x2000
stackTop = 0x8000
returnOutsideCode = 0

bufLen :: Word
bufLen = 8

ra, sp, a0, a1, a2 :: RegIdx
ra = 1
sp = 2
a0 = 10
a1 = 11
a2 = 12

pointer :: Address -> Rel Word
pointer = Same . bitCoerce

withArguments :: RelState -> RelState
withArguments st =
  st
    { relRegs =
        writeReg a0 (pointer secretBuf)
          . writeReg a1 (pointer publicBuf)
          . writeReg a2 (Same bufLen)
          . writeReg sp (pointer stackTop)
          . writeReg ra (pointer returnOutsideCode)
          $ relRegs st
    }

data SecretBuffer = SecretBuffer Word Word Word Word

freshSecretBuffer :: SymFC.Sym SecretBuffer
freshSecretBuffer =
  SecretBuffer
    <$> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word

withSecretBuffer :: SecretBuffer -> RelState -> RelState
withSecretBuffer (SecretBuffer lowA lowB highA highB) =
  withSecretWord (secretBuf + 4) highA highB . withSecretWord secretBuf lowA lowB

callFunction :: Fuel -> Code -> Address -> SymFC.Sym Verdict
callFunction fuel code entry = do
  isa <- SymFC.fresh @IsaSys
  secret <- freshSecretBuffer
  let start = (sameState code isa) {relPc = entry}
  relRun fuel $ withSecretBuffer secret $ withArguments start

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

memcmpWordCompare :: Address
memcmpWordCompare = 0x42838

{-# ANN memcmpLeaks (SymFC stepperEmbeddings) #-}
memcmpLeaks :: SymFC.Sym ()
memcmpLeaks = expectLeakAt memcmpWordCompare LeakBranch $ callFunction stepLimit memcmpCode memcmpEntry

{-# ANN sodiumMemcmpNoLeak (SymFC stepperEmbeddings) #-}
sodiumMemcmpNoLeak :: SymFC.Sym ()
sodiumMemcmpNoLeak = expectNoLeak $ callFunction stepLimit sodiumMemcmpCode sodiumMemcmpEntry

{-# ANN sodiumMemcmpShortRunOutOfFuel (SymFC stepperEmbeddings) #-}
sodiumMemcmpShortRunOutOfFuel :: SymFC.Sym ()
sodiumMemcmpShortRunOutOfFuel = expectOutOfFuel $ callFunction tooFewSteps sodiumMemcmpCode sodiumMemcmpEntry
