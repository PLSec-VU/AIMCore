{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module STSimSpec (stSimTests) where

import Access (unAccess)
import Clash.Prelude hiding (Word, init, lookup)
import Control.Monad.ST (runST)
import qualified Core
import Elf.ElfLoader (getElfSegments, startAddr, readElf, baseAddr)
import STSimulate
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Types
import RegFile (lookupRF)
import Data.Functor.Identity
import qualified Data.Map.Strict as Map

stSimTests :: TestTree
stSimTests =
  testGroup
    "ST Memory Simulator Tests"
    [ testCase "rv32ui-p-add using STMemory" $ do
        elf <- readElf "test/rv32ui/rv32ui-p-add"
        entry <- startAddr elf
        base <- baseAddr elf
        let segments = getElfSegments elf
        
        let finalState = runST $ do
              stMem <- newSTMemory (fromIntegral entry) (fromIntegral base) segments
              runUntilHalt @Identity stMem
        
        -- Success criteria for rv32ui tests:
        -- gp (x3) should be 1
        -- a0 (x10) should be 0
        let rf = Core.stateRegFile finalState
        let gp = lookupRF 3 rf
        let a0 = lookupRF 10 rf
        
        unAccess gp @?= 1
        unAccess a0 @?= 0
    ]
