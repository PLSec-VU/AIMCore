{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.RelState
  ( RelRegs
  , readReg
  , writeReg
  , RelMem
  , sameMem
  , readMemWord
  , loadKnownWord
  , writeMem
  , leftMem
  , rightMem
  , RegSlot
  , slotFor
  , readSlot
  , writeSlot
  , funcSlots
  , Operands (..)
  , Fetched (..)
  , fetched
  , Code (..)
  , CodeAddresses (..)
  , noCode
  , withInstruction
  , codeFromMemory
  , RelState (..)
  , sameState
  , withSecretReg
  , withSecretWord
  ) where

import Data.Functor.Identity (runIdentity)
import Clash.Prelude (bitCoerce)
import ISA (DepReg (..), Func, Instr (..), IsaStateG (..), PC, interp)
import Instruction (decode')
import Memory.Types (MemOps (..))
import RegFile (RegFileOps (..))
import Stepper (IsaSys)
import Stepper.Concrete (concreteAddress, knownBool)
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

funcSlots :: Func a -> (RegSlot, RegSlot)
funcSlots f = (sourceSlot (fst (deps f)), sourceSlot (snd (deps f)))

data Operands = Operands
  { sourceSlots :: (RegSlot, RegSlot)
  , registerFieldSlot :: RegSlot
  }

operandsOf :: Instr Func -> Operands
operandsOf instr = case instr of
  Reg rd f -> Operands (funcSlots f) (slotFor rd)
  Load _ _ rd f -> Operands (funcSlots f) (slotFor rd)
  Jump rd _ target -> Operands (funcSlots target) (slotFor rd)
  Store _ addr rs2 -> Operands (funcSlots addr) (slotFor rs2)
  Branch cond _ -> Operands (funcSlots cond) (sourceSlot Nothing)
  Nop -> unused
  Break -> unused
  Syscall -> unused
  where
    unused = Operands (sourceSlot Nothing, sourceSlot Nothing) (sourceSlot Nothing)

data Fetched = Fetched
  { fetchedAddress :: Address
  , fetchedWord :: Word
  , fetchedInstr :: Instr Func
  , fetchedOperands :: Maybe Operands
  , fetchedFollowing :: Maybe Fetched
  }

fetched :: Address -> Word -> Maybe Fetched -> Fetched
fetched addr word following =
  Fetched
    { fetchedAddress = addr
    , fetchedWord = word
    , fetchedInstr = instr
    , fetchedOperands = operandsOf instr <$ concreteAddress (bitCoerce word)
    , fetchedFollowing = following >>= sittingAfter addr
    }
  where
    instr = interp (decode' word)

sittingAfter :: Address -> Fetched -> Maybe Fetched
sittingAfter addr next = case knownBool (fetchedAddress next == addr + 4) of
  Just True -> Just next
  _ -> Nothing

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
codeFromMemory mem = Code {codeFetch = \pc -> Just (fetched pc (memReadWord pc mem) Nothing), codeAddresses = EveryAddress}

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

withSecretReg :: RegIdx -> Word -> Word -> RelState -> RelState
withSecretReg idx secretA secretB st =
  st {relRegs = writeReg idx (Pair secretA secretB) (relRegs st)}

withSecretWord :: Address -> Word -> Word -> RelState -> SymFC.Sym RelState
withSecretWord addr secretA secretB st = do
  mem <- writeMem (relPath st) Word addr (Pair secretA secretB) (relMem st)
  pure st {relMem = mem}
