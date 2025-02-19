module Leak where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift)
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.Tuple (swap)
import GHC.TypeNats
import Instruction hiding (decode, halt)
import qualified Instruction
import Pipe
import Regfile
import Simulate (readWord)
import Types
import Prelude hiding (Ordering (..), Word, init, not, undefined, (!!), (&&), (||))

-- `fromJust` is safe here because the `fetch` stage unconditionally always reads
-- from memory (and its read may just be superseded by the `memory` stage).
obs :: Output -> Address
obs = memAddress . fromJust . getFirst . outMem

data LeakState = LeakState
  { leakBubble :: Bool,
    leakPc :: Address
  }
  deriving (Eq, Show)

data LeakInst
  = LJ Address
  | LRType RegIdx RegIdx RegIdx
  | LOther
  deriving (Eq, Ord, Show)

data LeakOut = LeakOut
  { leakInst :: LeakInst,
    leakRs1 :: Word,
    leakRs2 :: Word
  }
  deriving (Eq, Show, Ord)

type LeakM = State LeakState

leak :: Input -> LeakM LeakOut
leak (Input mem rs1 rs2) = do
  linstr <-
    case Instruction.decode mem of
      RType op rd r1 r2 ->
        pure $ LRType rd r1 r2
  pure $
    LeakOut
      { leakInst = linstr,
        leakRs1 = rs1,
        leakRs2 = rs2
      }

-- initLeak :: Vec n Word -> LeakState ((GHC.TypeNats.*) n 4)
-- initLeak prog =
--   LeakState
--     { leakRF = initRF,
--       leakRAM = vecWordToByte prog,
--       leakBubble = False,
--       leakPc = 4 * 50
--     }
--
--
-- leak :: (KnownNat n) => Input -> LeakM n LeakInst
-- leak (Input mem rs1 rs2) =
--   case Instruction.decode mem of
--     RType op rd _ _ -> do
--       writeRF rd $ alu op rs1 rs2
--       pure LOther
--     IType iop rd r1 imm -> do
--       let op =
--             case iop of
--               Arith op' -> op'
--               _ -> ADD
--           res <- alu op rs1 $ pure (signExtend imm)
--       case iop of
--         Arith {} -> pure LOther
--         Load size sign -> do
--           let loadExtend = \case
--                 (Byte, Signed) -> signExtend $ slice d7 d0 res
--                 (Byte, Unsigned) -> zeroExtend $ slice d7 d0 res
--                 (Half, Signed) -> signExtend $ slice d15 d0 res
--                 (Half, Unsigned) -> signExtend $ slice d15 d0 res
--                 (Word, _) -> signExtend $ slice d31 d0 res
--               val = loadExtend (size, sign)
--           writeRF rd =<< readRAM (unpack val)
--           pure LOther
--         Jump -> do
--           writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--           pure $ LJ $ bitCoerce res
--         Env {} -> error ""
--     SType size imm r1 r2 -> do
--       addr <- unpack <$> (alu ADD <$> readRF r1 <*> pure (signExtend imm))
--       val <- readRF r2
--       writeRAM size addr val
--       pure LOther
--     BType cmp imm r1 r2 -> do
--       branched <- branch cmp <$> readRF r1 <*> readRF r2
--       if branched
--         then
--           LJ . bitCoerce
--             <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
--         else pure LOther
--     UType Zero rd imm -> do
--       let imm' = imm ++# 0 `shiftL` 12
--       writeRF rd $ imm'
--       pure LOther
--     UType PC rd imm -> do
--       let imm' = imm ++# 0 `shiftL` 12
--       writeRF rd imm'
--       pure LOther
--     JType rd imm -> do
--       writeRF rd =<< (bitCoerce . (+ 4)) <$> gets leakPc
--       LJ . bitCoerce
--         <$> (alu ADD <$> (bitCoerce <$> gets leakPc) <*> pure (signExtend imm))
--
-- leakRun :: (KnownNat n) => LeakState n -> Word -> (LeakState n, LeakInst)
-- leakRun s i = swap $ runState (leak i) s
--
-- leakStep :: (KnownNat n) => (LeakState n, SimState) -> Word -> ((LeakState n, SimState), Address)
-- leakStep (ls, ss) i = ((ls', ss'), pc)
--   where
--     (ls', inst) = leakRun ls i
--     (ss', pc) = simRun ss inst
--
-- simOn :: Int -> Vec n Word -> [Address]
-- simOn n prog = simOn' n initState
--   where
--     simOn' 0 _ = mempty
--     simOn' _ _ = mempty
--     simOn' n s =
--       let (s', pc) = leakStep s
--        in pc : simOn' (n - 1) s'
--     initState = (initLeak prog, initSim)
--
-- type SimM = State SimState
--
-- data SimState = SimState
--   { simPc :: Address,
--     simInstr :: LeakInst
--   }
--   deriving (Show, Eq)
--
-- initSim :: SimState
-- initSim =
--   SimState
--     { simPc = 4 * 50,
--       simInstr = LOther
--     }
--
-- simFetch :: LeakInst -> SimM Address
-- simFetch _ = gets simPc
--
-- simExecute :: SimM ()
-- simExecute = do
--   instr <- gets simInstr
--   case instr of
--     LJ pc' ->
--       modify $ \s -> s {simPc = pc'}
--     LOther ->
--       modify $ \s -> s {simPc = simPc s + 4}
--
-- simTick :: LeakInst -> SimM Address
-- simTick inst = do
--   simExecute
--   simFetch inst
--
-- simRun :: SimState -> LeakInst -> (SimState, Address)
-- simRun s i = swap $ runState (simTick i) s
--
-- readRF :: RegIdx -> LeakM n Word
-- readRF idx = gets $ lookupRF idx . leakRF
--
-- writeRF :: RegIdx -> Word -> LeakM n ()
-- writeRF idx val =
--   modify $ \s -> s {leakRF = modifyRF idx val $ leakRF s}
--
-- -- TODO: Fix the code duplication with Simulator.hs
-- writeRAM :: (KnownNat n) => Size -> Address -> Word -> LeakM n ()
-- writeRAM size addr w =
--   modify $ \s -> s {leakRAM = write size addr w $ leakRAM s}
--   where
--     write size addr w mem =
--       let b0 = slice d7 d0 w
--           b1 = slice d15 d8 w
--           b2 = slice d23 d16 w
--           b3 = slice d31 d24 w
--           writeByte =
--             replace addr b0
--           writeHalf =
--             replace (addr + 1) b1 . writeByte
--           writeWord =
--             replace (addr + 3) b3
--               . replace (addr + 2) b2
--               . writeHalf
--        in case size of
--             Byte -> writeByte mem
--             Half -> writeHalf mem
--             Word -> writeWord mem
--
-- readRAM :: (KnownNat n) => Address -> LeakM n Word
-- readRAM addr = gets $ readWord addr . leakRAM
--   where
--     readWord addr mem =
--       (mem !! (addr + 3))
--         ++# (mem !! (addr + 2))
--         ++# (mem !! (addr + 1))
--         ++# (mem !! addr)
