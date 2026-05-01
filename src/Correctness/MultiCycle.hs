{-# LANGUAGE TypeFamilies #-}
module Correctness.MultiCycle where

import Clash.Prelude hiding (Word, Ordering(..))
import Prelude hiding (Word, (!!), not, (&&), (||))
import Types
import Instruction
import Interp
import Data.Maybe
import Data.Functor.Identity
import Simulate (Machine(..))
import Util (ramRead, ramWrite, regWrite, regRead, initPc)
import Core hiding (State, init)
import Access
import Data.Monoid (First(..))

-- | Phase of the simple non-pipelined core.
-- Each instruction takes multiple cycles since we can only do one memory
-- operation per cycle through outMem, and register reads take one cycle
-- (requested via outRs1/outRs2, returned in next cycle's inputRs1/inputRs2).
data Phase
  = Fetch
    -- ^ Request instruction fetch at current PC.
    -- Used at boot, after stores, and after ecalls — any time the memory
    -- port was unavailable for fetching in the previous cycle.
  | Decode
    -- ^ Instruction arrived in inputMem. Decode it, request register reads.
  | Execute Instruction
    -- ^ Register values arrived. Execute the instruction.
  | LoadWait RegIdx Size Sign
    -- ^ Waiting for load result from memory.
  deriving (Show, Generic, NFDataX, Eq)

-- | Architectural State — minimal non-pipelined core.
-- Register file is stored externally in the MonadMemory (IOMemT).
data AState = AState
  { aPc :: Address
  , aPhase :: Phase
  }
  deriving (Show, Generic, NFDataX, Eq)

instance Machine AState where
  type MachineInput AState = Input Identity
  type MachineOutput AState = Output Identity
  mInitInput = initInput
  mInitState = AState initPc Fetch

  -- | Pure step: given input and state, produce new state and output.
  -- Memory and register file I/O happen in mNext.
  mStep inp s = pure $ case aPhase s of

    -- FETCH: request instruction at PC from memory.
    -- Transition to Decode so next cycle processes the fetched instruction.
    Fetch ->
      ( s { aPhase = Decode }
      , mempty
          { outMem = pure $ MemAccess True (aPc s) Word Nothing
          }
      )

    -- DECODE: instruction arrived in inputMem. Decode it, request registers.
    Decode ->
      let instr = if inputIsInstr inp
                  then decode' (unAccess $ inputMem inp)
                  else nop
      in ( s { aPhase = Execute instr }
         , mempty
             { outRs1 = pure $ fromMaybe 0 $ getRs1 instr
             , outRs2 = pure $ fromMaybe 0 $ getRs2 instr
             }
         )

    -- EXECUTE: register values arrived. Execute the instruction.
    Execute instr ->
      let r1 = inputRs1 inp
          r2 = inputRs2 inp
          pc = aPc s
          res = interp instr r1 r2 pc
          pc' = fromMaybe (pc + 4) (interpAddr res)
      in case instr of

        -- Store: write data to memory. Need a separate Fetch cycle to get
        -- the next instruction since the memory port is used for the store.
        SType size imm _ _ ->
          ( s { aPc = pc', aPhase = Fetch }
          , mempty
              { outMem = pure $ MemAccess False
                  (unpack (unAccess r1 + signExtend imm))
                  size
                  (Just r2)
              }
          )

        -- Load: request memory read, wait for result.
        IType (Load size sign) rd _ imm ->
          ( s { aPhase = LoadWait rd size sign }
          , mempty
              { outMem = pure $ MemAccess False
                  (unpack $ unAccess $ alu True ADD r1 (pure $ signExtend imm))
                  size
                  Nothing
              }
          )

        -- Ecall: signal syscall, fetch next instruction.
        IType (Env Call) _ _ _ ->
          ( s { aPc = pc', aPhase = Fetch }
          , mempty
              { outSyscall = pure True
              }
          )

        -- Ebreak: signal halt.
        IType (Env Break) _ _ _ ->
          ( s { aPhase = Fetch }
          , mempty
              { outHalt = pure True
              }
          )

        -- All other instructions (ALU, branch, jump, upper immediate):
        -- writeback result and fetch next instruction in one cycle.
        _ ->
          let wbVal = case getRd instr of
                Just rd | rd /= 0 -> pure (rd, pure (interpRes res))
                _ -> mempty
          in ( s { aPc = pc', aPhase = Decode }
             , mempty
                 { outMem = pure $ MemAccess True pc' Word Nothing
                 , outRd = wbVal
                 }
             )

    -- LOAD WAIT: memory read result has arrived, writeback and fetch next.
    LoadWait rd size sign ->
      let loadedWord = loadExtend size sign (unAccess $ inputMem inp)
          pc = aPc s
          pc' = pc + 4
      in ( s { aPc = pc', aPhase = Fetch }
         , mempty
             { outRd = if rd /= 0 then pure (rd, pure loadedWord) else mempty
             }
         )

  -- | Handle I/O: process the output's memory and register operations,
  -- construct the next input.
  mNext (Output mem rs1 rs2 rd syscall hlt)
    | getFirst hlt == Just True = pure Nothing
    | otherwise = do
        -- Write register if requested
        case getFirst rd of
            Just (idx, val) | idx /= 0 -> regWrite idx (unAccess val)
            _ -> pure ()

        -- Read registers if requested
        rs1' <- maybe (pure 0) regRead $ getFirst rs1
        rs2' <- maybe (pure 0) regRead $ getFirst rs2

        -- Handle memory access
        (mem_in, mem_instr) <- case getFirst mem of
            Just (MemAccess isInstr addr size mval) -> do
                case mval of
                    Nothing -> do
                        word <- ramRead addr
                        pure (pure word, isInstr)
                    Just val -> do
                        ramWrite addr size (unAccess val)
                        pure (pure 0, isInstr)
            Nothing -> pure (pure 0, False)

        pure $ Just $ Input
            { inputIsInstr = mem_instr
            , inputMem = mem_in
            , inputRs1 = pure rs1'
            , inputRs2 = pure rs2'
            }
