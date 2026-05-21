
{-# LANGUAGE PackageImports #-}
module Main where

import qualified "aimcore" Core as Core
import "aimcore" Instruction
import "aimcore" Types
import qualified "aimcore" Leak.PC.PC as PC
import qualified "aimcore" Leak.PC.Leak as Leak
import qualified "aimcore" Leak.PC.Sim as Sim
import qualified Pantomime as P
import Data.Functor.Identity
import Test.Tasty.QuickCheck
import Clash.Prelude (BitVector, Unsigned, pack, unpack)

main :: IO ()
main = do
  -- arg_X0 from first block
  let s1 = Core.State
        { Core.stateFePc = 0x2fd36ffc
        , Core.stateDePc = 0
        , Core.stateExPc = 0x05d36000
        , Core.stateExInstr = RType ADD 0 0 0
        , Core.stateMemInstr = Nop BranchFirstCycle
        , Core.stateMemRes = Identity 0x002d2002
        , Core.stateMemVal = Identity 0
        , Core.stateWbInstr = RType ADD 19 0 0
        , Core.stateWbRes = Identity 0xffff240f
        , Core.stateCtrl = Core.initCtrl -- close enough
        , Core.stateHalt = Core.SecurityViolation
        }
  
  -- arg_X2 from first block
  let s2 = Core.State
        { Core.stateFePc = 0x2fd36ffc
        , Core.stateDePc = 0
        , Core.stateExPc = 0x05d36000
        , Core.stateExInstr = Nop LoadHazardFirstCycle
        , Core.stateMemInstr = IType Jump 12 17 0
        , Core.stateMemRes = Identity 0x03cafcc1
        , Core.stateMemVal = Identity 0
        , Core.stateWbInstr = IType (Load Word Unsigned) 22 0 0
        , Core.stateWbRes = Identity 0x00070073
        , Core.stateCtrl = Core.initCtrl
        , Core.stateHalt = Core.SecurityViolation
        }

  let i1 = Core.Input True (Identity 0x0180a0ef) (Identity 0xfffe248f) (Identity 0xa8c0028e)
  let i2 = Core.Input True (Identity 0x000000ef) (Identity 0x18aafcc1) (Identity 0x18aafcc1)

  print $ "proj s1 == proj s2: " ++ show (PC.proj s1 == PC.proj s2)
  let ((sl1, ss1), o1) = PC.circuit (PC.proj s1) i1
  let ((sl2, ss2), o2) = PC.circuit (PC.proj s2) i2
  print $ "leakage s1 i1 == leakage s2 i2: " ++ show (o1 == o2)
  
  let (s1', core_o1) = PC.implementation s1 i1
  let (s2', core_o2) = PC.implementation s2 i2
  print $ "proj s1' == proj s2': " ++ show (PC.proj s1' == PC.proj s2')
