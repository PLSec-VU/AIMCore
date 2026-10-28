{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.Chacha
  ( chachaBlockNoLeak
  , chachaShortRunOutOfFuel
  , chachaOneStepShortOutOfFuel
  , chachaSecretLengthLeaks
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

$(elfCode "benchmark/bench_chacha20"
    [ ("chachaEncrypt", ["chacha20_encrypt_bytes.part.0", "memset"])
    ])

steps :: Int -> Fuel
steps 0 = Done
steps n = More (steps (n - 1))

oneBlockSteps :: Fuel
oneBlockSteps = steps 2065

oneStepShort :: Fuel
oneStepShort = steps 2064

tooFewSteps :: Fuel
tooFewSteps = steps 200

contextAddr, inputBuf, outputBuf, stackTop, returnOutsideCode :: Address
contextAddr = 0x1000
inputBuf = 0x2000
outputBuf = 0x3000
stackTop = 0x8000
returnOutsideCode = 0

stackGuardSlot, stackGuard :: Address
stackGuardSlot = 0x4e020
stackGuard = 0x4eba0

blockBytes :: Word
blockBytes = 64

ra, sp, a0, a1, a2, a3, a4 :: RegIdx
ra = 1
sp = 2
a0 = 10
a1 = 11
a2 = 12
a3 = 13
a4 = 14

pointer :: Address -> Rel Word
pointer = Same . bitCoerce

withArguments :: Rel Word -> RelState -> RelState
withArguments lengthLow st =
  st
    { relRegs =
        writeReg a0 (pointer contextAddr)
          . writeReg a1 (pointer inputBuf)
          . writeReg a2 (pointer outputBuf)
          . writeReg a3 lengthLow
          . writeReg a4 (Same 0)
          . writeReg sp (pointer stackTop)
          . writeReg ra (pointer returnOutsideCode)
          $ relRegs st
    }

withPublicWord :: Address -> Word -> RelState -> SymFC.Sym RelState
withPublicWord addr value st = do
  mem <- writeMem (relPath st) Word addr (Same value) (relMem st)
  pure st {relMem = mem}

withStackGuardPointer :: RelState -> SymFC.Sym RelState
withStackGuardPointer = withPublicWord stackGuardSlot (bitCoerce stackGuard)

contextWord :: Address -> Address
contextWord index = contextAddr + 4 * index

expandConstant :: Int -> Word
expandConstant 0 = 0x61707865
expandConstant 1 = 0x3320646e
expandConstant 2 = 0x79622d32
expandConstant _ = 0x6b206574

withConstants :: RelState -> SymFC.Sym RelState
withConstants st =
  withPublicWord (contextWord 0) (expandConstant 0) st
    >>= withPublicWord (contextWord 1) (expandConstant 1)
    >>= withPublicWord (contextWord 2) (expandConstant 2)
    >>= withPublicWord (contextWord 3) (expandConstant 3)

data SecretKey = SecretKey Word Word Word Word Word Word Word Word

freshSecretKey :: SymFC.Sym SecretKey
freshSecretKey =
  SecretKey
    <$> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word

withSecretKeys :: SecretKey -> SecretKey -> RelState -> SymFC.Sym RelState
withSecretKeys (SecretKey ka0 ka1 ka2 ka3 ka4 ka5 ka6 ka7) (SecretKey kb0 kb1 kb2 kb3 kb4 kb5 kb6 kb7) st =
  withSecretWord (contextWord 4) ka0 kb0 st
    >>= withSecretWord (contextWord 5) ka1 kb1
    >>= withSecretWord (contextWord 6) ka2 kb2
    >>= withSecretWord (contextWord 7) ka3 kb3
    >>= withSecretWord (contextWord 8) ka4 kb4
    >>= withSecretWord (contextWord 9) ka5 kb5
    >>= withSecretWord (contextWord 10) ka6 kb6
    >>= withSecretWord (contextWord 11) ka7 kb7

encryptCall :: Fuel -> Rel Word -> SymFC.Sym Verdict
encryptCall fuel lengthLow = do
  isa <- SymFC.fresh @IsaSys
  keyA <- freshSecretKey
  keyB <- freshSecretKey
  let start = (sameState chachaEncryptCode isa) {relPc = chachaEncryptEntry}
  ready <-
    withConstants (withArguments lengthLow start)
      >>= withSecretKeys keyA keyB
      >>= withStackGuardPointer
  relRun fuel ready

encryptBlock :: Fuel -> SymFC.Sym Verdict
encryptBlock fuel = encryptCall fuel (Same blockBytes)

encryptSecretLength :: SymFC.Sym Verdict
encryptSecretLength = do
  lengthA <- SymFC.fresh @Word
  lengthB <- SymFC.fresh @Word
  encryptCall oneBlockSteps (Pair lengthA lengthB)

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

shortTailCheck :: Address
shortTailCheck = 0x111ac

{-# ANN chachaBlockNoLeak (SymFC stepperEmbeddings) #-}
chachaBlockNoLeak :: SymFC.Sym ()
chachaBlockNoLeak = expectNoLeak $ encryptBlock oneBlockSteps

{-# ANN chachaShortRunOutOfFuel (SymFC stepperEmbeddings) #-}
chachaShortRunOutOfFuel :: SymFC.Sym ()
chachaShortRunOutOfFuel = expectOutOfFuel $ encryptBlock tooFewSteps

{-# ANN chachaOneStepShortOutOfFuel (SymFC stepperEmbeddings) #-}
chachaOneStepShortOutOfFuel :: SymFC.Sym ()
chachaOneStepShortOutOfFuel = expectOutOfFuel $ encryptBlock oneStepShort

{-# ANN chachaSecretLengthLeaks (SymFC stepperEmbeddings) #-}
chachaSecretLengthLeaks :: SymFC.Sym ()
chachaSecretLengthLeaks = expectLeakAt shortTailCheck LeakBranch encryptSecretLength
