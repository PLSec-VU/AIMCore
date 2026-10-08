{-# LANGUAGE BangPatterns #-}

module CacheSpec (cacheTests) where

import Clash.Prelude hiding (Word)
import Instruction
import Memory.Cache
import Memory.Types (MemBytes, mkProg)
import Proof.Machine
import RegFile (RegFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import Types (Size (Word), Word)
import Prelude hiding (Word, not)

cacheTests :: TestTree
cacheTests =
  testGroup
    "Cache"
    [ directMappedTests,
      timingLeakTests
    ]

-- | 4-line direct-mapped cache for collision testing.
type TestCache = DirectMapped 4

freshCache :: TestCache
freshCache = mkDirectMapped

directMappedTests :: TestTree
directMappedTests =
  testGroup
    "DirectMapped"
    [ testCase "a fresh cache misses everywhere" $
        cacheLookup 0 freshCache @?= (Nothing :: Maybe Word),
      testCase "a lookup right after an insert hits" $
        cacheLookup 0 (cacheInsert 0 0xCAFE freshCache) @?= Just 0xCAFE,
      testCase "a colliding address evicts the resident line" $
        let c = cacheInsert 16 0xBEEF (cacheInsert 0 0xCAFE freshCache)
         in (cacheLookup 0 c, cacheLookup 16 c) @?= (Nothing, Just 0xBEEF),
      testCase "a non-colliding address does not evict" $
        let c = cacheInsert 4 0xBEEF (cacheInsert 0 0xCAFE freshCache)
         in (cacheLookup 0 c, cacheLookup 4 c) @?= (Just 0xCAFE, Just 0xBEEF),
      testCase "invalidating a resident line makes it miss again" $
        let c = cacheInvalidate 0 (cacheInsert 0 0xCAFE freshCache)
         in cacheLookup 0 c @?= (Nothing :: Maybe Word)
    ]

-- | Extra cycles incurred by a cache miss.
missPenalty :: Int
missPenalty = 3

-- | Two loads to the same address (second hits).
progHit :: Vec 50 Word
progHit =
  mkProg $
    IType (Load Word Signed) 1 0 0
      :> IType (Load Word Signed) 2 0 0
      :> Instruction.break
      :> Nil

-- | Two loads to colliding addresses (second misses).
progMiss :: Vec 50 Word
progMiss =
  mkProg $
    IType (Load Word Signed) 1 0 0
      :> IType (Load Word Signed) 2 0 16
      :> Instruction.break
      :> Nil

-- | Run a cached system to completion, returning cycles taken.
cyclesToHalt :: CacheSys RegFile MemBytes TestCache -> Int
cyclesToHalt = go 0
  where
    go !n cs
      | not (running (cacheSysCore cs)) = n
      | otherwise = go (n + 1) (stepCached (fromIntegral missPenalty) cs)

timingLeakTests :: TestTree
timingLeakTests =
  testGroup
    "Timing leak"
    [ testCase "a cache miss costs exactly the configured extra cycles" $ do
        let hitCycles = cyclesToHalt (initCacheSys (initSys progHit) freshCache)
            missCycles = cyclesToHalt (initCacheSys (initSys progMiss) freshCache)
        assertEqual
          "same instructions, only the second load's address differs, but the \
          \attacker-visible cycle count does not match"
          missPenalty
          (missCycles - hitCycles)
    ]
