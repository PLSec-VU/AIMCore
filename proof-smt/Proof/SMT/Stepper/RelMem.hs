{-# LANGUAGE ImportQualifiedPost #-}

module Proof.SMT.Stepper.RelMem
  ( layeredMatchesArray
  , layeredMissingWriteFails
  , layeredPairMatchesArrays
  , confinedWriteMatchesArray
  , confinedWriteKeepsKnownWords
  , aliasingWriteMatchesArray
  , aliasingMissingWriteFails
  , repeatedKnownLoadXorsToZero
  , recordedLoadThenAliasingWriteMatchesArray
  , recordedLoadThenConfinedWriteMatchesArray
  ) where

import Clash.Prelude (bitCoerce)
import Control.Monad ((>=>))
import Data.Bits (xor)
import Memory.Types (MemOps (..))
import Stepper.Concrete (concreteAddress, stepperEmbeddings)
import Stepper.Memory (Memory)
import Stepper.Memory qualified as Memory
import Stepper.Rel (Rel (..), left, right)
import Stepper.RelMem
import SymFC (SymFC (..))
import SymFC.BuiltIn qualified as SymFC
import Types (Address, Byte, Size (..), Word)
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

applyLayered :: Writes -> RelMem -> SymFC.Sym RelMem
applyLayered w =
  writeByte True firstAddr (Same (firstByte w))
    >=> writeByte True (symbolicAddr w) (Same (symbolicByte w))
    >=> writeByte True secondAddr (Same (secondByte w))
    >=> writeByte True secondAddr (Same (overwriteByte w))

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
  layered <- applyLayered w (sameMem base)
  expectValid $ readsAgree probe layered (applyPlain w base)

{-# ANN layeredMissingWriteFails (SymFC stepperEmbeddings) #-}
layeredMissingWriteFails :: SymFC.Sym ()
layeredMissingWriteFails = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  w <- freshWrites
  layered <- applyLayered w (sameMem base)
  expectCounterexample $ readsAgree probe layered (applyPlainWithoutSymbolic w base)

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
  layered <- writeByte True secretAddr (Pair secretA secretB) (sameMem base) >>= applyLayered w
  let plainA = applyPlain w (Memory.write secretAddr secretA base)
      plainB = applyPlain w (Memory.write secretAddr secretB base)
  expectValid $
    left (readByte probe layered) == Memory.read probe plainA
      && right (readByte probe layered) == Memory.read probe plainB
      && leftMem layered == plainA
      && rightMem layered == plainB

farAddr :: Address
farAddr = 0x2000

confinedBetweenKeys :: Address -> Bool
confinedBetweenKeys addr = 0x1000 <= addr && addr < farAddr

mayAliasKnownKeys :: Address -> Bool
mayAliasKnownKeys addr = firstAddr <= addr && addr <= secondAddr

data UnknownWrite = UnknownWrite
  { unknownAddr :: Address
  , unknownByte :: Byte
  , firstKnown :: Byte
  , secondKnown :: Byte
  , farKnown :: Byte
  }

freshUnknownWrite :: SymFC.Sym UnknownWrite
freshUnknownWrite =
  UnknownWrite
    <$> SymFC.fresh @Address
    <*> SymFC.fresh @Byte
    <*> SymFC.fresh @Byte
    <*> SymFC.fresh @Byte
    <*> SymFC.fresh @Byte

knownThenUnknown :: Bool -> UnknownWrite -> RelMem -> SymFC.Sym RelMem
knownThenUnknown path w =
  writeByte path firstAddr (Same (firstKnown w))
    >=> writeByte path secondAddr (Same (secondKnown w))
    >=> writeByte path farAddr (Same (farKnown w))
    >=> writeByte path (unknownAddr w) (Same (unknownByte w))

knownWritesOnly :: UnknownWrite -> Memory 32 8 -> Memory 32 8
knownWritesOnly w =
  Memory.write farAddr (farKnown w)
    . Memory.write secondAddr (secondKnown w)
    . Memory.write firstAddr (firstKnown w)

knownThenUnknownPlain :: UnknownWrite -> Memory 32 8 -> Memory 32 8
knownThenUnknownPlain w = Memory.write (unknownAddr w) (unknownByte w) . knownWritesOnly w

agreesAtKeysAndProbe :: Address -> RelMem -> Memory 32 8 -> Bool
agreesAtKeysAndProbe probe layered plain =
  agreesAt probe && agreesAt firstAddr && agreesAt secondAddr && agreesAt farAddr
    && Memory.read probe (leftMem layered) == Memory.read probe plain
  where
    agreesAt addr = left (readByte addr layered) == Memory.read addr plain

{-# ANN confinedWriteMatchesArray (SymFC stepperEmbeddings) #-}
confinedWriteMatchesArray :: SymFC.Sym ()
confinedWriteMatchesArray = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  w <- freshUnknownWrite
  let path = confinedBetweenKeys (unknownAddr w)
  layered <- knownThenUnknown path w (sameMem base)
  expectValid $ not path || agreesAtKeysAndProbe probe layered (knownThenUnknownPlain w base)

savedWord :: Word
savedWord = 0x00042838

exactWordAt :: Address -> RelMem -> Maybe Address
exactWordAt addr mem = concreteAddress (bitCoerce (left (readMemWord addr mem)))

{-# ANN confinedWriteKeepsKnownWords (SymFC stepperEmbeddings) #-}
confinedWriteKeepsKnownWords :: SymFC.Sym ()
confinedWriteKeepsKnownWords = do
  base <- SymFC.fresh @(Memory 32 8)
  addr <- SymFC.fresh @Address
  byte <- SymFC.fresh @Byte
  let path = confinedBetweenKeys addr
  layered <-
    ( writeMem path Word firstAddr (Same savedWord)
        >=> writeMem path Word farAddr (Same savedWord)
        >=> writeByte path addr (Same byte)
    )
      (sameMem base)
  case (exactWordAt firstAddr layered, exactWordAt farAddr layered) of
    (Just first, Just far) -> expectValid $ first == bitCoerce savedWord && far == bitCoerce savedWord
    _ -> SymFC.halt

{-# ANN aliasingWriteMatchesArray (SymFC stepperEmbeddings) #-}
aliasingWriteMatchesArray :: SymFC.Sym ()
aliasingWriteMatchesArray = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  w <- freshUnknownWrite
  let path = mayAliasKnownKeys (unknownAddr w)
  layered <- knownThenUnknown path w (sameMem base)
  expectValid $ not path || agreesAtKeysAndProbe probe layered (knownThenUnknownPlain w base)

{-# ANN aliasingMissingWriteFails (SymFC stepperEmbeddings) #-}
aliasingMissingWriteFails :: SymFC.Sym ()
aliasingMissingWriteFails = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  w <- freshUnknownWrite
  let path = mayAliasKnownKeys (unknownAddr w)
  layered <- knownThenUnknown path w (sameMem base)
  expectCounterexample $ not path || agreesAtKeysAndProbe probe layered (knownWritesOnly w base)

knownLoad :: Address -> RelMem -> SymFC.Sym (Rel Word, RelMem)
knownLoad addr mem = maybe SymFC.halt pure (loadKnownWord addr mem)

{-# ANN repeatedKnownLoadXorsToZero (SymFC stepperEmbeddings) #-}
repeatedKnownLoadXorsToZero :: SymFC.Sym ()
repeatedKnownLoadXorsToZero = do
  base <- SymFC.fresh @(Memory 32 8)
  (first, recorded) <- knownLoad firstAddr (sameMem base)
  (second, _) <- knownLoad firstAddr recorded
  case concreteAddress (bitCoerce (left first `xor` left second)) of
    Just difference -> expectValid $ difference == 0 && left first == memReadWord firstAddr base
    Nothing -> SymFC.halt

recordedLoadThenWriteMatchesArray :: (Address -> Bool) -> SymFC.Sym ()
recordedLoadThenWriteMatchesArray onPath = do
  base <- SymFC.fresh @(Memory 32 8)
  probe <- SymFC.fresh @Address
  addr <- SymFC.fresh @Address
  byte <- SymFC.fresh @Byte
  (loaded, recorded) <- knownLoad firstAddr (sameMem base)
  let path = onPath addr
      plain = Memory.write addr byte base
  written <- writeByte path addr (Same byte) recorded
  expectValid $
    not path
      || ( left loaded == memReadWord firstAddr base
             && left (readMemWord firstAddr written) == memReadWord firstAddr plain
             && left (readByte probe written) == Memory.read probe plain
             && leftMem written == plain
         )

{-# ANN recordedLoadThenAliasingWriteMatchesArray (SymFC stepperEmbeddings) #-}
recordedLoadThenAliasingWriteMatchesArray :: SymFC.Sym ()
recordedLoadThenAliasingWriteMatchesArray = recordedLoadThenWriteMatchesArray mayAliasKnownKeys

{-# ANN recordedLoadThenConfinedWriteMatchesArray (SymFC stepperEmbeddings) #-}
recordedLoadThenConfinedWriteMatchesArray :: SymFC.Sym ()
recordedLoadThenConfinedWriteMatchesArray = recordedLoadThenWriteMatchesArray confinedBetweenKeys
