module Simulate where

import Clash.Prelude hiding (Ordering (..), Word, def, init)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Data.Monoid
import qualified Debug.Trace as DB
import Instruction hiding (decode)
import Pipe
import Regfile
import Types
import Prelude hiding (Ordering (..), Word, init, map, not, repeat, undefined, (!!), (&&), (++), (||))

type MemM n = State (Vec n Word)

traceM :: (Applicative f) => String -> f ()
traceM = DB.traceM -- const $ pure ()

memRead :: (KnownNat n) => Word -> MemM n Word
memRead w = gets (!! w)

memWrite :: (KnownNat n) => Address -> Word -> MemM n ()
memWrite addr w =
  modify $ replace addr w

simMemStep :: (KnownNat n) => Output -> MemM n Input
simMemStep (Output addr val)
  | Just addr' <- getLast addr,
    Just val' <- getLast val = do
      memWrite addr' val'
      pure $ Input 0
  | Just addr' <- getLast addr,
    Nothing <- getLast val =
      Input <$> memRead (bitCoerce addr')
  | otherwise = pure $ Input 0

simStep :: (KnownNat n) => Input -> Pipe -> MemM n (Input, Pipe)
simStep i s = do
  let (s', o) = simPipe s i
  traceM $
    unlines
      [ "simStep",
        "i",
        show i,
        "s",
        show s,
        "o",
        show o,
        "s'",
        show s'
      ]
  i' <- simMemStep o
  pure (i', s')

simPipe :: Pipe -> Input -> (Pipe, Output)
simPipe = flip $ execRWS simPipeM
  where
    simPipeM :: CPUM ()
    simPipeM = do
      s_fetch <- localS fetch
      s_decode <- localS decode
      s_execute <- localS execute
      s_memory <- localS memory
      s_writeback <- localS writeback
      s <- get
      traceM $
        unlines
          [ "s_writeback",
            show s_writeback
          ]

      put $ Prelude.foldl1 (combinePipes s) [s_fetch, s_decode, s_execute, s_memory, s_writeback]

    combinePipes orig p1 p2 =
      Pipe
        { rf = combine (rf orig) (rf p1) (rf p2),
          fePc = combine (fePc orig) (fePc p1) (fePc p2),
          dePc = combine (dePc orig) (dePc p1) (dePc p2),
          exPc = combine (exPc orig) (exPc p1) (exPc p2),
          exIr = combine (exIr orig) (exIr p1) (exIr p2),
          meIr = combine (meIr orig) (meIr p1) (meIr p2),
          meRe = combine (meRe orig) (meRe p1) (meRe p2),
          wbIr = combine (wbIr orig) (wbIr p1) (wbIr p2),
          wbRe = combine (wbRe orig) (wbRe p1) (wbRe p2)
        }
    combine orig a b
      | orig == b = a
      | orig == a = b
      | otherwise = error "multiple writes to same value"
    localS m = do
      s <- get
      m
      s' <- get
      put s
      pure s'

simulate :: (KnownNat n) => Int -> Vec n Word -> Vec n Word
simulate cycles =
  execState $ simulate' initInput initPipe cycles
  where
    simulate' i s cycles
      | cycles <= 0 = pure ()
      | otherwise = do
          (i', s') <- simStep i s
          simulate' i' s' $ cycles - 1

    initInput =
      Input
        { inputMem = 0
        }
    initPipe =
      Pipe
        { rf = initRF,
          fePc = 25,
          dePc = 0,
          exPc = 0,
          exIr = nop,
          meIr = nop,
          meRe = 0,
          wbIr = nop,
          wbRe = 0
        }

mkRAM :: ((n + m) ~ 25, KnownNat m) => Vec n Word -> Vec 50 Word
mkRAM prog =
  (repeat 0 :: Vec 25 Word)
    ++ prog
    ++ repeat 0

prog1 :: Vec 2 Word
prog1 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2
      ]

prog2 :: Vec 2 Word
prog2 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- mem[1 + r0] := r2
        SType Word 1 0 2
      ]

prog3 :: Vec 7 Word
prog3 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- r3 := mem[r0 + 0]
        IType (Load Word Signed) 3 0 0,
        -- r4 := r3 + r3
        RType ADD 4 3 3,
        -- r5 := r4 - r3
        RType SUB 5 4 3,
        -- r6 := r4 + r5
        RType ADD 6 4 5,
        -- mem[1 + r0] := r6
        SType Word 1 0 6
        ---- r7 := mem[r0 + 1]
        -- IType (Load Word Signed) 7 0 1,
        ---- r8 := r7 + r3
        -- RType ADD 8 7 3,
        ---- mem[2 + r0] := r8
        -- SType Word 2 0 8
      ]
