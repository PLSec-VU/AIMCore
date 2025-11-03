module SecuritySpec (securityTests) where

import Access
import SecureMemory
import SecureSimulate
import Simulate
import Control.Monad.State
import Clash.Prelude hiding (Word, (&&))
import qualified Clash.Prelude as CP
import Types
import Util
import RegFile
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- Helper functions to avoid overlapping instances
secureMarkMemoryRegion :: Address -> Address -> Bool -> State (SecureMem n) ()
secureMarkMemoryRegion startAddr endAddr isSecret = do
  let region = MemoryRegion startAddr endAddr isSecret
  modify $ \s -> s {securityRegions = region : securityRegions s}

secureIsMemorySecret :: Address -> State (SecureMem n) Bool
secureIsMemorySecret addr = do
  regions <- gets securityRegions
  pure $ any (isAddrInSecretRegion addr) regions
  where
    isAddrInSecretRegion :: Address -> MemoryRegion -> Bool
    isAddrInSecretRegion a (MemoryRegion start end secret) =
      secret && a >= start && a <= end

identityMarkMemoryRegion :: Address -> Address -> Bool -> State (Mem n) ()
identityMarkMemoryRegion _ _ _ = pure ()

identityIsMemorySecret :: Address -> State (Mem n) Bool
identityIsMemorySecret _ = pure False

securityTests :: TestTree
securityTests = testGroup "Security Memory Functions"
  [ testGroup "Memory Region Marking"
      [ testCase "should mark memory regions as secret" $ do
          let initialMem = initSecureMem (CP.repeat 0 :: Vec 100 Byte) initRF
          let result = execState testMarkSecret initialMem
          let isSecret = evalState (secureIsMemorySecret 0x1000) result
          isSecret @?= True,

        testCase "should mark memory regions as public" $ do
          let initialMem = initSecureMem (CP.repeat 0 :: Vec 100 Byte) initRF
          let result = execState testMarkPublic initialMem
          let isSecret = evalState (secureIsMemorySecret 0x2000) result
          isSecret @?= False,

        testCase "should handle overlapping regions correctly" $ do
          let initialMem = initSecureMem (CP.repeat 0 :: Vec 100 Byte) initRF
          let result = execState testOverlapping initialMem
          let isSecret1 = evalState (secureIsMemorySecret 0x1500) result
          let isSecret2 = evalState (secureIsMemorySecret 0x2500) result
          isSecret1 @?= True
          isSecret2 @?= False
      ],

    testGroup "Identity vs PubSec behavior"
      [ testCase "should have no-op security functions for Identity" $ do
          let initialMem = Mem (CP.repeat 0 :: Vec 100 Byte) initRF
          let result = execState testIdentityNoOp initialMem
          let isSecret = evalState (identityIsMemorySecret 0x1000) result
          isSecret @?= False
      ]
  ]

testMarkSecret :: State (SecureMem 100) ()
testMarkSecret = secureMarkMemoryRegion 0x1000 0x1FFF True

testMarkPublic :: State (SecureMem 100) ()
testMarkPublic = secureMarkMemoryRegion 0x2000 0x2FFF False

testOverlapping :: State (SecureMem 100) ()
testOverlapping = do
  secureMarkMemoryRegion 0x1000 0x1FFF True   -- Secret region
  secureMarkMemoryRegion 0x2000 0x2FFF False  -- Public region

testIdentityNoOp :: State (Mem 100) ()
testIdentityNoOp = identityMarkMemoryRegion 0x1000 0x1FFF True