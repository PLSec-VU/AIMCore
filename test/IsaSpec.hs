{-# LANGUAGE ScopedTypeVariables #-}

-- | Direct validation of the ISA specification against the official RISC-V
-- conformance suite.
--
-- The refinement proof is stated against "ISA", so nothing in the proof can
-- tell us whether "ISA" itself is right -- it is the thing being proved
-- against. The @rv32ui@ programs in @test/rv32ui@ are an external reference, so
-- running the ISA model on them and checking the suite's own pass condition is
-- evidence about the specification that does not come from the implementation.
--
-- Without this, the only check on "ISA" is transitive: the core passes
-- @rv32ui@ (see "BenchmarkSpec") and the proof says the core refines
-- "ISA". That validates the specification /through/ the implementation,
-- which is the wrong direction.
--
-- The pass condition is the one "BenchmarkSpec" applies to the core: at the
-- @ecall@ that ends the program, @gp@ (x3) is 1 and @a0@ (x10) is 0.
module IsaSpec (isaConformanceTests) where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as M
import Elf.ElfLoader (baseAddr, getElfSegments, readElf, startAddr)
import ISA (IsaStateG (..), IsaState, StepG (..), Step, isaStep, isaStepDecoded, isaRun, isaInstrAt)
import Memory.Types (MemOps (..))
import RegFile
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | Sparse memory over the full 32-bit space.
--
-- 'Memory.Types.MemBytes' is a 'Vec' sized for the proof harness and far too
-- small for these binaries, and 'Memory.Types.MemFn' builds a closure chain one
-- link per byte written, which does not survive a few thousand stores. A map is
-- neither of those things.
newtype MemMap = MemMap (M.Map Address Byte)

instance MemOps MemMap where
  memReadByte a (MemMap m) = M.findWithDefault 0 a m

  memReadWord a m =
    memReadByte (a + 3) m ++# memReadByte (a + 2) m ++# memReadByte (a + 1) m ++# memReadByte a m

  memWriteWord size a w (MemMap m) =
    MemMap $ case size of
      Byte -> put a b0 m
      Half -> put (a + 1) b1 (put a b0 m)
      Word -> put (a + 3) b3 (put (a + 2) b2 (put (a + 1) b1 (put a b0 m)))
    where
      b0 = slice d7 d0 w
      b1 = slice d15 d8 w
      b2 = slice d23 d16 w
      b3 = slice d31 d24 w
      put = M.insert

type IsaSys = IsaStateG RegFile MemMap

-- | Step until the ISA halts. 'Left' if it runs past @fuel@ instructions, which
-- means the program never reached its terminating @ecall@.
runToHalt :: Int -> IsaSys -> Either String IsaSys
runToHalt fuel st
  | fuel <= 0 = Left "fuel exhausted before the program halted"
  | otherwise = case isaStep st of
      IsaHalted -> Right st
      Next st' -> runToHalt (fuel - 1) st'

loadProgram :: FilePath -> IO IsaSys
loadProgram path = do
  elf <- readElf path
  entry <- startAddr elf
  base <- baseAddr elf
  let segments = getElfSegments elf
      bytes =
        M.fromList
          [ (addr + fromIntegral i, fromIntegral b)
            | (addr, bs) <- segments,
              (i, b) <- P.zip [(0 :: Int) ..] (BS.unpack bs)
          ]
      -- The stack pointer the core harness uses; some tests touch the stack.
      sp = fromIntegral base + 0x1000000 - 0x1000
  P.pure
    IsaState
      { isaPc = fromIntegral entry,
        isaRegFile = modifyRF 2 (Identity sp) initRF,
        isaMem = MemMap bytes
      }

-- | The suite's pass condition, read off the architectural register file.
checkPassed :: IsaSys -> Either String ()
checkPassed st
  | gp P./= 1 = Left ("gp = " P.++ show gp P.++ ", expected 1")
  | a0 P./= 0 = Left ("a0 = " P.++ show a0 P.++ ", expected 0")
  | otherwise = Right ()
  where
    gp = runIdentity (lookupRFg 3 (isaRegFile st))
    a0 = runIdentity (lookupRFg 10 (isaRegFile st))

mkIsaTest :: String -> TestTree
mkIsaTest name =
  testCase name $ do
    st0 <- loadProgram ("test/rv32ui/" P.++ name)
    case runToHalt 1000000 st0 P.>>= checkPassed of
      Right () -> P.pure ()
      Left e -> assertFailure e

-- | The same programs "BenchmarkSpec" runs against the core, run against the
-- ISA model instead. @fence_i@ is excluded for the same reason it is excluded
-- there: it is self-modifying code.
isaConformanceTests :: TestTree
isaConformanceTests =
  testGroup
    "ISA specification vs rv32ui"
    (P.map (mkIsaTest . ("rv32ui-p-" P.++)) programs)
  where
    programs =
      [ "add", "addi", "and", "andi", "auipc",
        "beq", "bge", "bgeu", "blt", "bltu", "bne",
        "jal", "jalr",
        "lb", "lbu", "ld_st", "lh", "lhu", "lui", "lw",
        "ma_data",
        "or", "ori",
        "sb", "sh", "simple", "sll", "slli", "slt", "slti", "sltiu", "sltu",
        "sra", "srai", "srl", "srli", "st_ld", "sub", "sw",
        "xor", "xori"
      ]
