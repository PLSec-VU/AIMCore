{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.RelState
  ( RelRegs
  , readReg
  , writeReg
  , RelMem
  , sameMem
  , pairMem
  , readMemWord
  , loadKnownWord
  , loadMemWord
  , writeMem
  , leftMem
  , rightMem
  , RegSlot
  , slotFor
  , readSlot
  , writeSlot
  , funcSlots
  , knownOperands
  , Operands (..)
  , Fetched (..)
  , fetched
  , fetchedDecoded
  , Code (..)
  , CodeAddresses (..)
  , noCode
  , withInstruction
  , codeFromMemory
  , RelState (..)
  , sameState
  , pairState
  , withSecretReg
  , withSecretWord
  ) where

import Data.Functor.Identity (runIdentity)
import Clash.Prelude (bitCoerce)
import ISA (DepReg (..), Func, Instr (..), IsaStateG (..), PC, interp)
import Instruction (Instruction, decode')
import Memory.Types (MemOps (..))
import RegFile (RegFileOps (..))
import Stepper (IsaSys)
import Stepper.Concrete (concreteAddress)
import Stepper.Memory (Memory)
import Stepper.Rel (Rel (..))
import Stepper.RelMem
import Stepper.RelRegs
import SymFC.BuiltIn qualified as SymFC
import Types (Address, RegIdx, Size (..), Word)
import Prelude hiding (Word)

data CodeAddresses
  = EveryAddress
  | KnownAddresses [Address]

funcSlots :: Func a -> SymFC.Sym (RegSlot, RegSlot)
funcSlots f = (,) <$> sourceSlot (fst (deps f)) <*> sourceSlot (snd (deps f))

knownFuncSlots :: Func a -> (RegSlot, RegSlot)
knownFuncSlots f = (knownSourceSlot (fst (deps f)), knownSourceSlot (snd (deps f)))
  where
    knownSourceSlot = maybe NoRegister knownSlot

data Operands = Operands
  { sourceSlots :: (RegSlot, RegSlot)
  , registerFieldSlot :: RegSlot
  }

knownOperands :: Instr Func -> Operands
knownOperands instr = case instr of
  Reg rd f -> Operands (knownFuncSlots f) (knownSlot rd)
  Load _ _ rd f -> Operands (knownFuncSlots f) (knownSlot rd)
  Jump rd _ target -> Operands (knownFuncSlots target) (knownSlot rd)
  Store _ addr rs2 -> Operands (knownFuncSlots addr) (knownSlot rs2)
  Branch cond _ -> Operands (knownFuncSlots cond) NoRegister
  Nop -> unused
  Break -> unused
  Syscall -> unused
  where
    unused = Operands (NoRegister, NoRegister) NoRegister

data Fetched = Fetched
  { fetchedAddress :: Address
  , fetchedWord :: Word
  , fetchedDecodedWord :: Instruction
  , fetchedInstr :: Instr Func
  , fetchedOperands :: SymFC.Sym (Maybe Operands)
  , fetchedFollowing :: Maybe Fetched
  }

fetched :: Address -> Word -> Maybe Fetched -> Fetched
fetched addr word = fetchedDecoded addr word (decode' word)

fetchedDecoded :: Address -> Word -> Instruction -> Maybe Fetched -> Fetched
fetchedDecoded addr word decoded following =
  Fetched
    { fetchedAddress = addr
    , fetchedWord = word
    , fetchedDecodedWord = decoded
    , fetchedInstr = instr
    , fetchedOperands = pure (Just (knownOperands instr))
    , fetchedFollowing = following >>= sittingAfter addr
    }
  where
    instr = interp decoded

sittingAfter :: Address -> Fetched -> Maybe Fetched
sittingAfter addr next = if fetchedAddress next == addr + 4 then Just next else Nothing

fetchedFromMemory :: Address -> Word -> Fetched
fetchedFromMemory addr word = literal {fetchedOperands = operandsIfKnown}
  where
    literal = fetched addr word Nothing
    operandsIfKnown = (knownOperands (fetchedInstr literal) <$) <$> concreteAddress (bitCoerce word)

data Code = Code
  { codeFetch :: Address -> Maybe Fetched
  , codeAddresses :: CodeAddresses
  }

noCode :: Code
noCode = Code {codeFetch = const Nothing, codeAddresses = KnownAddresses []}

withInstruction :: Address -> Word -> Code -> Code
withInstruction addr word code =
  Code
    { codeFetch = \pc -> if pc == addr then Just (fetched addr word Nothing) else codeFetch code pc
    , codeAddresses = withAddress (codeAddresses code)
    }
  where
    withAddress EveryAddress = EveryAddress
    withAddress (KnownAddresses addrs) = KnownAddresses (addr : addrs)

codeFromMemory :: Memory 32 8 -> Code
codeFromMemory mem = Code {codeFetch = \pc -> Just (fetchedFromMemory pc (memReadWord pc mem)), codeAddresses = EveryAddress}

data RelState = RelState
  { relPath :: Bool
  , relCode :: Code
  , relPc :: PC
  , relFetchHint :: Maybe Fetched
  , relRegs :: RelRegs
  , relMem :: RelMem
  }

sameState :: Code -> IsaSys -> RelState
sameState code isa =
  RelState
    { relPath = True
    , relCode = code
    , relPc = isaPc isa
    , relFetchHint = Nothing
    , relRegs = regsFrom (\idx -> Same (runIdentity (lookupRFg idx (isaRegFile isa))))
    , relMem = sameMem (isaMem isa)
    }

pairState :: Code -> IsaSys -> IsaSys -> RelState
pairState code first second =
  (sameState code first)
    { relRegs = regsFrom (\idx -> Pair (registerOf first idx) (registerOf second idx))
    , relMem = pairMem (isaMem first) (isaMem second)
    }
  where
    registerOf isa idx = runIdentity (lookupRFg idx (isaRegFile isa))

withSecretReg :: RegIdx -> Word -> Word -> RelState -> SymFC.Sym RelState
withSecretReg idx secretA secretB st = do
  regs <- writeReg idx (Pair secretA secretB) (relRegs st)
  pure st {relRegs = regs}

withSecretWord :: Address -> Word -> Word -> RelState -> SymFC.Sym RelState
withSecretWord addr secretA secretB st = do
  mem <- writeMem (relPath st) Word addr (Pair secretA secretB) (relMem st)
  pure st {relMem = mem}
