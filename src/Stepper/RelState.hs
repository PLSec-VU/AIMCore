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
  , Code
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
import Types (Address, RegIdx, Size (..), Word)
import Prelude hiding (Word)

type RelRegs = RegIdx -> Rel Word

readReg :: RelRegs -> Maybe RegIdx -> Rel Word
readReg regs = maybe (Same 0) regs

writeReg :: RegIdx -> Rel Word -> RelRegs -> RelRegs
writeReg 0 _ regs = regs
writeReg rd value regs = \idx -> if idx == rd then value else regs idx

type Code = Address -> Maybe Word

noCode :: Code
noCode = const Nothing

withInstruction :: Address -> Word -> Code -> Code
withInstruction addr word code = \pc -> if pc == addr then Just word else code pc

codeFromMemory :: Memory 32 8 -> Code
codeFromMemory mem = \pc -> Just (memReadWord pc mem)

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
    , relRegs = \idx -> Same (runIdentity (lookupRFg idx (isaRegFile isa)))
    , relMem = sameMem (isaMem isa)
    }

withSecretReg :: RegIdx -> Word -> Word -> RelState -> RelState
withSecretReg idx secretA secretB st =
  st {relRegs = writeReg idx (Pair secretA secretB) (relRegs st)}

withSecretWord :: Address -> Word -> Word -> RelState -> RelState
withSecretWord addr secretA secretB st =
  st {relMem = writeMem Word addr (Pair secretA secretB) (relMem st)}
