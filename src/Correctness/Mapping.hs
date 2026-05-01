module Correctness.Mapping where

import Core
import Correctness.MultiCycle
import Types
import Instruction
import Access
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))

-- | Map a pipelined core state into a non-empty list of architectural states.
-- Each stage in the pipeline corresponds to an architectural state at some phase.
-- If a mapping cannot be made (e.g. stage is empty or invalid), it is omitted.
-- The list is guaranteed to be non-empty because the Fetch stage is always active.
mapCore :: (Access f) => State f -> NonEmpty AState
mapCore s = AState (stateFePc s) Fetch :| catMaybes [de, ex, me, wb]
  where
    ctrl = stateCtrl s
    
    -- 2. Decode Stage: The instruction requested in the previous cycle has arrived.
    -- We omit this in the first cycle as no instruction has been requested yet.
    de = if ctrlFirstCycle ctrl 
         then Nothing 
         else Just $ AState (stateDePc s) Decode
    
    -- 3. Execute Stage: ALU and branch operations are performed here.
    ex = if stateExInstr s == nop 
         then Nothing 
         else Just $ AState (stateExPc s) (Execute (stateExInstr s))
    
    -- 4. Memory Stage: Memory requests (Loads/Stores) are sent here.
    -- In MultiCycle.hs, these requests are part of the Execute phase.
    me = if stateMemInstr s == nop 
         then Nothing 
         else Just $ AState (stateMemPc s) (Execute (stateMemInstr s))
    
    -- 5. Writeback Stage: Results are written back to the register file.
    -- For Loads, this corresponds to the LoadWait phase where the data is arriving.
    -- For other instructions, they have effectively finished their architectural 
    -- Execute phase, so we don't map them to a distinct phase in MultiCycle.hs.
    wb = case stateWbInstr s of
      Instruction.IType (Load size sign) rd _ _ -> 
        Just $ AState (stateWbPc s) (LoadWait rd size sign)
      _ -> Nothing
