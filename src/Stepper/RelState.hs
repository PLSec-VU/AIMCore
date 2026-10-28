{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.RelState
  ( RelRegs
  , readReg
  , writeReg
  , RelMem
  , sameMem
  , readMemWord
  , writeMem
  , leftMem
  , rightMem
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
import ISA (IsaStateG (..), PC)
import Memory.Types (MemOps (..))
import RegFile (RegFileOps (..))
import Stepper (IsaSys)
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

data Code = Code
  { codeFetch :: Address -> Maybe Word
  , codeAddresses :: CodeAddresses
  }

noCode :: Code
noCode = Code {codeFetch = const Nothing, codeAddresses = KnownAddresses []}

withInstruction :: Address -> Word -> Code -> Code
withInstruction addr word code =
  Code
    { codeFetch = \pc -> if pc == addr then Just word else codeFetch code pc
    , codeAddresses = withAddress (codeAddresses code)
    }
  where
    withAddress EveryAddress = EveryAddress
    withAddress (KnownAddresses addrs) = KnownAddresses (addr : addrs)

codeFromMemory :: Memory 32 8 -> Code
codeFromMemory mem = Code {codeFetch = \pc -> Just (memReadWord pc mem), codeAddresses = EveryAddress}

data RelState = RelState
  { relPath :: Bool
  , relCode :: Code
  , relPc :: PC
  , relRegs :: RelRegs
  , relMem :: RelMem
  }

sameState :: Code -> IsaSys -> RelState
sameState code isa =
  RelState
    { relPath = True
    , relCode = code
    , relPc = isaPc isa
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
