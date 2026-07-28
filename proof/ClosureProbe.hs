-- | A ladder of toy properties testing how far the symbolic executor gets with
-- closures, ending at exactly the shape 'ISA.interp'' uses.
--
-- 'ISA.interp'' does not terminate under symbolic execution (see "Verify"), and
-- the only structure left in the frame once decode, memory and the containers
-- were ruled out is that it builds an @'ISA.Instr' 'ISA.Func'@ -- a sum type
-- whose fields are records containing /closures/ -- by casing on a symbolic
-- instruction, and then applies those closures.
--
-- Each rung adds one ingredient.
--
-- RESULT: every rung terminates, most in seconds. Closures are NOT the
-- problem, including rung 5, which is exactly the 'ISA.Func' shape. Nor is
-- 'ISA.interp'': rung 6 (one instruction shape, symbolic ALU op) verifies, and
-- rung 7 (fully symbolic 'Instruction') terminates in ~9s.
--
-- Since the premise side of 'Verify.indStep0' is also fast on its own, and the
-- two together do not terminate, what remains is ordinary multiplicative path
-- explosion from composing them -- not non-termination, and not any single
-- function.
module ClosureProbe
  ( Shape (..),
    clo1,
    clo2,
    clo3,
    clo4,
    clo5,
    clo6,
    clo7,
    clo8,
    results,
  )
where

import ArrayRF
import Axioms (arrayAxioms, axioms)
import ISAStep
import Core (alu)
import Data.Functor.Identity
import qualified ISA
import Instruction
import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Pantomime (Theory (..), pantomime)
import qualified Pantomime.BuiltIn as Pantomime
import Types
import Prelude hiding (Ordering (..), Word, init, log, not, undefined, (!!), (&&), (++), (||))

-- | Rung 1: a bare lambda, applied immediately.
{-# ANN clo1 (Theory axioms) #-}
clo1 :: Word -> Pantomime.Bool
clo1 v = Pantomime.boolean $ (\x -> x + 1) v == v + 1

-- | Rung 2: a closure returned from a function, capturing a symbolic value.
mkAdd :: Word -> (Word -> Word)
mkAdd k = \x -> x + k

{-# ANN clo2 (Theory axioms) #-}
clo2 :: Word -> Word -> Pantomime.Bool
clo2 k v = Pantomime.boolean $ mkAdd k v == v + k

-- | Rung 3: a closure stored in an ADT field, then projected out and applied.
newtype Box = Box (Word -> Word)

{-# ANN clo3 (Theory axioms) #-}
clo3 :: Word -> Word -> Pantomime.Bool
clo3 k v = Pantomime.boolean $ case Box (\x -> x + k) of Box f -> f v == v + k

-- | Rung 4: /which/ closure is boxed is decided by a case on a symbolic
-- scrutinee. This is the first rung where the executor cannot know statically
-- which function it is applying.
data Shape = A | B | C
  deriving (Generic, Eq)

pick :: Shape -> Box
pick A = Box (\x -> x + 1)
pick B = Box (\x -> x + 2)
pick C = Box (\x -> x + 3)

{-# ANN clo4 (Theory axioms) #-}
clo4 :: Shape -> Word -> Pantomime.Bool
clo4 s v =
  Pantomime.boolean $
    case pick s of
      Box f -> case s of
        A -> f v == v + 1
        B -> f v == v + 2
        C -> f v == v + 3

-- | Rung 5: exactly the 'ISA.Func' shape -- a record holding a closure /and/
-- ordinary data, carried inside a sum type, selected by casing on a symbolic
-- scrutinee, then applied.
data Fun = Fun
  { funF :: Word -> Word -> Word,
    funD :: Maybe RegIdx
  }

data Shaped
  = SReg RegIdx Fun
  | SBr Fun
  | SNone

toyInterp :: Shape -> Shaped
toyInterp A = SReg 1 (Fun (\a b -> a + b) (Just 1))
toyInterp B = SBr (Fun (\a b -> a - b) Nothing)
toyInterp C = SNone

{-# ANN clo5 (Theory axioms) #-}
clo5 :: Shape -> Word -> Word -> Pantomime.Bool
clo5 s a b =
  Pantomime.boolean $
    case toyInterp s of
      SReg _ f -> funF f a b == a + b
      SBr f -> funF f a b == a - b
      SNone -> True

-- | Rung 6: the real 'ISA.interp'', but on a single /concrete/ instruction
-- shape with symbolic fields. If this terminates, the cost is the fan-out
-- across 'Instruction' constructors rather than anything in one path.
{-# ANN clo6 (Theory axioms) #-}
clo6 :: Arith -> RegIdx -> RegIdx -> RegIdx -> Word -> Word -> Address -> Pantomime.Bool
clo6 op rd s1 s2 r1 r2 pc =
  Pantomime.boolean $
    case ISA.interp' (RType op rd s1 s2) of
      ISA.Reg rd' f ->
        rd' == rd
          && ISA.unDone (ISA.apply f r1 r2 pc)
            == runIdentity (alu op (Identity r1) (Identity r2))
      _ -> False

-- | Rung 7: 'ISA.interp'' on a fully symbolic 'Instruction', standalone --
-- no invariant, no premises. Deliberately false, so a prompt counterexample
-- crash means it terminated; a hang means the constructor fan-out alone is
-- enough to defeat the executor.
-- RESULT: terminates in ~9s (reaches 'sat', then hits the known
-- counterexample crash). Left un-annotated so the build stays green.
-- {-# ANN clo7 (Theory axioms) #-}
clo7 :: Instruction -> Word -> Word -> Address -> Pantomime.Bool
clo7 ir r1 r2 pc =
  Pantomime.boolean $
    case ISA.interp' ir of
      ISA.Nop -> True
      ISA.Break -> True
      ISA.Syscall -> True
      ISA.Reg _ f -> ISA.unDone (ISA.apply f r1 r2 pc) == 0
      ISA.Load _ _ _ f -> ISA.unDone (ISA.apply f r1 r2 pc) == 0
      ISA.Jump _ l _ -> ISA.unDone (ISA.apply l r1 r2 pc) == 0
      ISA.Store _ a _ -> ISA.unDone (ISA.apply a r1 r2 pc) == 0
      ISA.Branch c _ -> ISA.unDone (ISA.apply c r1 r2 pc)

-- | Rung 8: the whole of 'ISAStep.isaStep' on a flat symbolic ISA state, with
-- NO premises -- the gap in the earlier bisection, which always carried the
-- premises along. 'isaStep' adds, over rung 7: a memory read plus 'decode'',
-- two register reads, and a register or memory write.
--
-- Deliberately false (jumps and taken branches do not go to @pc + 4@), so a
-- prompt counterexample crash means it terminated.
-- RESULT: terminates (reaches 'sat', then the counterexample crash).
-- {-# ANN clo8 (Theory arrayAxioms) #-}
clo8 :: RegArr -> MemArr -> Address -> Pantomime.Bool
clo8 ra ma pc =
  Pantomime.boolean $
    case isaStep (IsaState pc (RegArrF ra) ma) of
      Next st' -> isaPc st' == pc + 4
      IsaHalted -> True

results :: [(String, Maybe String)]
results =
  [ ("clo1", $(pantomime 'clo1)),
    ("clo2", $(pantomime 'clo2)),
    ("clo3", $(pantomime 'clo3)),
    ("clo4", $(pantomime 'clo4)),
    ("clo5", $(pantomime 'clo5)),
    ("clo6", $(pantomime 'clo6))
  ]
