module Leak.SimplePC.SimpleLeak (
  Leakage (..),
  (^&^),
)
where

import Control.Monad.Identity
import qualified Core
import Instruction
import Types

-- A leakage is a mealy machine with state sl, input ii, output ol,
-- equipped with a state projection from si to sl.
data Leakage si ii sl ol = Leakage ((sl, ii) -> (sl, ol)) (si -> sl)

-- Combine two leakages into a leakage that leaks both.
(^&^) :: Leakage si ii sl1 ol1 -> Leakage si ii sl2 ol2 -> Leakage si ii (sl1, sl2) (ol1, ol2)
(Leakage f1 p1) ^&^ (Leakage f2 p2) = Leakage leak (\s -> (p1 s, p2 s))
 where
  leak ((sl1, sl2), i) = ((sl1', sl2'), (o1, o2))
   where
    (sl1', o1) = f1 (sl1, i)
    (sl2', o2) = f2 (sl2, i)

---------

data BaseInstr
  = Jump'
  | Load' RegIdx
  | Store'
  | Other'
  | Call'
  | Break'
  deriving (Show, Eq)

data Instr = Instr BaseInstr (Maybe RegIdx) (Maybe RegIdx)
  deriving (Show, Eq)

leakInstructionType :: Leakage (Core.State Identity) (Core.Input Identity) () Instr
leakInstructionType = Leakage (\((), i) -> ((), leak i)) (const ())
 where
  leak :: Core.Input Identity -> Instr
  leak input | Core.inputIsInstr input = convert $ decode' $ runIdentity $ Core.inputMem input
  leak _ = convert nop
  convert :: Instruction -> Instr
  convert instr = Instr (mkInstr instr) (getRs1 instr >>= nonZero) (getRs2 instr >>= nonZero)
  nonZero :: RegIdx -> Maybe RegIdx
  nonZero 0 = Nothing
  nonZero r = Just r
  mkInstr :: Instruction -> BaseInstr
  mkInstr instr =
    case instr of
      RType{} -> Other'
      IType iop rd _ _ -> case iop of
        Arith{} -> Other'
        Load{} -> Load' rd
        Jump -> Jump'
        Env Break -> Break'
        Env Call -> Call'
      SType{} -> Store'
      BType{} -> Jump'
      UType Zero _ _ -> Other'
      UType PC _ _ -> Other'
      JType{} -> Jump'
