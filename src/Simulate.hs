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
import qualified Prelude

data Mem n = Mem
  { memRAM :: Vec n Word,
    memRf :: Regfile
  }
  deriving (Eq, Show, Generic, NFDataX)

type MemM n = State (Mem n)

traceM :: (Applicative f) => String -> f ()
traceM = DB.traceM -- const $ pure ()

ramRead :: (KnownNat n) => Address -> MemM n Word
ramRead addr = gets ((!! addr) . memRAM)

ramWrite :: (KnownNat n) => Address -> Word -> MemM n ()
ramWrite addr w =
  modify $ \s -> s {memRAM = replace addr w $ memRAM s}

simMemStep :: (KnownNat n) => Output -> MemM n Input
simMemStep (Output mem rs1 rs2 rd) = do
  (rs1', rs2') <- doRegFile
  mem_in <- doMemory
  pure $
    Input
      { inputMem = mem_in,
        inputRs1 = rs1',
        inputRs2 = rs2'
      }
  where
    doRegFile = do
      maybe (pure ()) (uncurry writeReg) $ getFirst rd
      rs1' <- maybe (pure 0) readReg $ getFirst rs1
      rs2' <- maybe (pure 0) readReg $ getFirst rs2
      pure (rs1', rs2')

    readReg idx = do
      gets $ lookupRF idx . memRf

    writeReg idx val = do
      modify $ \s -> s {memRf = modifyRF idx val $ memRf s}

    doMemory
      | Just (MemAccess addr mval) <- getFirst mem =
          case mval of
            Nothing -> ramRead addr
            Just val -> do
              ramWrite addr val
              pure 0
      | otherwise = pure 0

simStep :: (KnownNat n) => Int -> Input -> Pipe -> MemM n (Input, Pipe)
simStep cycle i s = do
  let (ctrl', s', o) = simPipe s i
  traceM $
    unlines
      [ "Step: " Prelude.++ show cycle,
        "-------------------- ",
        "",
        "Input:",
        "--------------------",
        show i,
        "",
        "State:",
        "--------------------",
        show s,
        "",
        "Output:",
        "--------------------",
        show o,
        "",
        "Control:",
        "--------------------",
        show ctrl',
        "",
        "",
        "",
        ""
      ]
  i' <- simMemStep o
  pure (i', s')

simPipe :: Pipe -> Input -> (Control, Pipe, Output)
simPipe = flip $ runRWS simPipeM
  where
    simPipeM :: CPUM Control
    simPipeM = do
      writeback
      memory
      execute
      decode
      fetch
      ctrl <- gets pipeCtrl
      resetCtrl
      pure ctrl

simulate :: (KnownNat n) => Int -> Vec n Word -> Mem n
simulate cycles =
  execState (simulate' initInput initPipe cycles) . flip Mem initRF
  where
    simulate' i s cycles'
      | cycles' <= 0 = pure ()
      | otherwise = do
          (i', s') <- simStep (cycles - cycles') i s
          simulate' i' s' $ cycles' - 1

    initInput =
      Input
        { inputMem = 0,
          inputRs1 = 0,
          inputRs2 = 0
        }
    initPipe =
      Pipe
        { fePc = 25,
          dePc = 0,
          exPc = 0,
          exIr = nop,
          exRs1 = 0,
          exRs2 = 0,
          meIr = nop,
          meRe = 0,
          meVal = 0,
          wbIr = nop,
          wbRe = 0,
          pipeCtrl = initCtrl
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

prog2 :: Vec 5 Word
prog2 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- r3 := mem[r0 + 0],
        IType (Load Word Signed) 3 0 0,
        -- r4 := r0 + r3
        RType ADD 4 0 3,
        -- mem[1 + r0] := r4
        SType Word 1 0 4
      ]

prog3 :: Vec 5 Word
prog3 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 3
        IType (Arith ADD) 2 0 3,
        -- r3 := r0 + r2
        RType ADD 3 0 2,
        -- r2 == r3 ? jump pc + 2
        BType EQ 2 2 3,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- mem[1 + r0] := r2
        SType Word 1 0 2
      ]

prog4 :: Vec 2 Word
prog4 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- mem[1 + r0] := r2
        SType Word 1 0 2
      ]

prog5 :: Vec 3 Word
prog5 =
  map encode $
    unsafeFromList
      [ -- r2 := r0 + 5
        IType (Arith ADD) 2 0 5,
        -- mem[0 + r0] := r2
        SType Word 0 0 2,
        -- r3 := mem[r0 + 0]
        IType (Load Word Signed) 3 0 0
        ---- r4 := r3 + r3
        -- RType ADD 4 3 3,
        ---- r5 := r4 - r3
        -- RType SUB 5 4 3,
        ---- r6 := r4 + r5
        -- RType ADD 6 4 5,
        ---- mem[1 + r0] := r6
        -- SType Word 1 0 6,
        ---- r7 := mem[r0 + 1]
        -- IType (Load Word Signed) 7 0 1,
        ---- r8 := r7 + r3
        -- RType ADD 8 7 3,
        ---- mem[2 + r0] := r8
        -- SType Word 2 0 8
      ]

sumTo :: Int -> Vec 7 Word
sumTo n =
  map encode $
    unsafeFromList
      [ -- r1 := r0 + n
        IType (Arith ADD) 1 0 $ fromIntegral n,
        -- r2 := 0 (res = 0)
        IType (Arith ADD) 2 0 0,
        -- r1 == r0 ? jump pc + 5
        BType EQ 4 1 0,
        -- r2 := r2 + r1 (res += n)
        RType ADD 2 2 3,
        -- r1 := r1 - 1 (n -= 1)
        IType (Arith ADD) 1 1 (-1),
        -- jump back to the branch
        JType 0 (-3),
        -- store result in mem[0]
        SType Word 0 0 2
      ]
