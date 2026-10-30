{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.Memcmp
  ( memcmpLeaks
  , sodiumMemcmpNoLeak
  , sodiumMemcmpShortRunOutOfFuel
  , memcmpFetchesKnownWords
  , sodiumMemcmpFetchesKnownWords
  , memcmpFetchRejectsWrongWord
  , memcmpDecodeAgrees
  , memcmpFetchesByAddress
  , memcmpDecodeRejectsWrongInstruction
  ) where

import Clash.Prelude (bitCoerce)
import Stepper (Fuel (..), IsaSys)
import Stepper.Concrete (stepperEmbeddings)
import Stepper.ElfCode (elfCode, expectDecodeAgreement, expectFetchByAddress, expectDecodeDisagreement, withWrongFirstInstruction)
import Stepper.Rel (Rel (..))
import Stepper.RelState
import Stepper.Relational
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Word)
import Prelude hiding (Word)

$(elfCode "benchmark/bench_vuln_memcmp"
    [ ("memcmp", ["memcmp"])
    , ("sodiumMemcmp", ["sodium_memcmp", "_sodium_dummy_symbol_to_prevent_memcmp_lto"])
    ])

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

withArguments :: RelState -> SymFC.Sym RelState
withArguments st = do
  regs <-
    writeReg a0 (pointer secretBuf)
      =<< writeReg a1 (pointer publicBuf)
      =<< writeReg a2 (Same bufLen)
      =<< writeReg sp (pointer stackTop)
      =<< writeReg ra (pointer returnOutsideCode) (relRegs st)
  pure st {relRegs = regs}

data SecretBuffer = SecretBuffer Word Word Word Word

freshSecretBuffer :: SymFC.Sym SecretBuffer
freshSecretBuffer =
  SecretBuffer
    <$> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word
    <*> SymFC.fresh @Word

withSecretBuffer :: SecretBuffer -> RelState -> SymFC.Sym RelState
withSecretBuffer (SecretBuffer lowA lowB highA highB) st =
  withSecretWord secretBuf lowA lowB st >>= withSecretWord (secretBuf + 4) highA highB

callFunction :: Fuel -> Code -> Address -> SymFC.Sym Verdict
callFunction fuel code entry = do
  isa <- SymFC.fresh @IsaSys
  secret <- freshSecretBuffer
  let start = (sameState code isa) {relPc = entry}
  relRun fuel =<< withSecretBuffer secret =<< withArguments start

expectNoLeak :: SymFC.Sym Verdict -> SymFC.Sym ()
expectNoLeak run = do
  verdict <- run
  case verdict of
    NoLeak -> pure ()
    _ -> SymFC.halt

expectLeakAt :: Address -> SymFC.Sym Verdict -> SymFC.Sym ()
expectLeakAt expectedPc run = do
  verdict <- run
  case verdict of
    Leaks pc | pc == expectedPc -> pure ()
    _ -> SymFC.halt

expectOutOfFuel :: SymFC.Sym Verdict -> SymFC.Sym ()
expectOutOfFuel run = do
  verdict <- run
  case verdict of
    OutOfFuel -> pure ()
    _ -> SymFC.halt

memcmpWordCompare :: Address
memcmpWordCompare = 0x442f0

{-# ANN memcmpLeaks (SymFC ['stepperEmbeddings]) #-}
memcmpLeaks :: SymFC.Sym ()
memcmpLeaks = expectLeakAt memcmpWordCompare $ callFunction stepLimit memcmpCode memcmpEntry

{-# ANN sodiumMemcmpNoLeak (SymFC ['stepperEmbeddings]) #-}
sodiumMemcmpNoLeak :: SymFC.Sym ()
sodiumMemcmpNoLeak = expectNoLeak $ callFunction stepLimit sodiumMemcmpCode sodiumMemcmpEntry

{-# ANN sodiumMemcmpShortRunOutOfFuel (SymFC ['stepperEmbeddings]) #-}
sodiumMemcmpShortRunOutOfFuel :: SymFC.Sym ()
sodiumMemcmpShortRunOutOfFuel = expectOutOfFuel $ callFunction tooFewSteps sodiumMemcmpCode sodiumMemcmpEntry

expectFetch :: Code -> Address -> Word -> SymFC.Sym ()
expectFetch code addr expectedWord = case codeFetch code addr of
  Just here | fetchedWord here == expectedWord -> pure ()
  _ -> SymFC.halt

expectNoFetchOf :: Code -> Address -> Word -> SymFC.Sym ()
expectNoFetchOf code addr unexpectedWord = case codeFetch code addr of
  Just here | fetchedWord here == unexpectedWord -> SymFC.halt
  _ -> pure ()

{-# ANN memcmpFetchesKnownWords (SymFC ['stepperEmbeddings]) #-}
memcmpFetchesKnownWords :: SymFC.Sym ()
memcmpFetchesKnownWords = do
  expectFetch memcmpCode memcmpEntry 0x00300693
  expectFetch memcmpCode memcmpWordCompare 0x00f71c63

{-# ANN sodiumMemcmpFetchesKnownWords (SymFC ['stepperEmbeddings]) #-}
sodiumMemcmpFetchesKnownWords :: SymFC.Sym ()
sodiumMemcmpFetchesKnownWords = do
  expectFetch sodiumMemcmpCode sodiumMemcmpEntry 0xfd010113
  expectFetch sodiumMemcmpCode (sodiumMemcmpEntry + 4) 0x02812423

{-# ANN memcmpFetchRejectsWrongWord (SymFC ['stepperEmbeddings]) #-}
memcmpFetchRejectsWrongWord :: SymFC.Sym ()
memcmpFetchRejectsWrongWord = expectNoFetchOf memcmpCode memcmpEntry 0x00300694

{-# ANN memcmpDecodeAgrees (SymFC ['stepperEmbeddings]) #-}
memcmpDecodeAgrees :: SymFC.Sym ()
memcmpDecodeAgrees = expectDecodeAgreement [memcmpDecoded, sodiumMemcmpDecoded]

{-# ANN memcmpFetchesByAddress (SymFC ['stepperEmbeddings]) #-}
memcmpFetchesByAddress :: SymFC.Sym ()
memcmpFetchesByAddress = expectFetchByAddress [memcmpCode, sodiumMemcmpCode]

{-# ANN memcmpDecodeRejectsWrongInstruction (SymFC ['stepperEmbeddings]) #-}
memcmpDecodeRejectsWrongInstruction :: SymFC.Sym ()
memcmpDecodeRejectsWrongInstruction = expectDecodeDisagreement [withWrongFirstInstruction memcmpDecoded, sodiumMemcmpDecoded]
