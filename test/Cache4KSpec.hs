{-# LANGUAGE PackageImports #-}

-- | The 4 KiB cache ("Memory.Cache4K") and its server ("Memory.Cache4KServer")
-- on their own: reads that cross lines, and a few concrete runs. The cache's
-- proof and the core behind the cache are tested in "CacheProofSpec".
module Cache4KSpec
  ( cache4KTests,
    TestMem (..),
    genMem,
    genTag,
    genAddr,
    genWord,
  )
where

import Clash.Prelude hiding (Ordering (..), Word, def, init, lift, log)
import Core.Pipeline (Input (..), MemAccess (..), Request)
import Memory.Cache4K
import Memory.Cache4KServer
import Memory.Ideal (idealServe)
import Memory.Types
import Proof.Memory.Cache4K.Server.Functional.Driver (srvDriver)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
import "aimcore" Types
import Prelude hiding (Ordering (..), Word, init, log, map, not, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

cache4KTests :: TestTree
cache4KTests =
  testGroup
    "4 KiB cache"
    [ testProperty "a read, from the lines of its first and last byte, answers as memory does" $
        withMaxSuccess 2000 $ checkCoverage $
          forAll genMem $ \mem -> forAll genAddr $ \a -> forAll (elements [Byte, Half, Types.Word]) $ \size ->
            let c = cacheFill (lastByteOf a size) mem (cacheFill a mem emptyCache4K)
             in cover 10 (straddles a size) "straddling" $
                  Just (cacheRead a size c) == idealAnswer mem (Just (MemRead a size)),
      runTests
    ]

-- Memory --------------------------------------------------------------------------

-- | A memory for the tests: bytes that are a function of their address and two
-- seeds, and the bytes written since, newest first. Unlike 'MemFn' it has
-- equality, which 'Proof.Memory.Cache4K.Server.Functional.Contract.srvReady' needs, and
-- it shows. Equality compares the representation, so two memories that agree
-- byte for byte may still differ; a server that leaves memory alone leaves it
-- equal.
data TestMem = TestMem (Unsigned 32) (Unsigned 32) [(Address, Byte)]
  deriving (Eq, Show)

instance MemOps TestMem where
  memReadByte a (TestMem k1 k2 ws) = case P.lookup a ws of
    Just b -> b
    Nothing -> slice d7 d0 (pack ((a * (k1 .|. 1) + k2) `xor` (a `shiftR` 9)))
  memReadWord a m =
    memReadByte (a + 3) m ++# memReadByte (a + 2) m ++# memReadByte (a + 1) m ++# memReadByte a m
  memWrite a size w (TestMem k1 k2 ws) = TestMem k1 k2 (bytes P.++ ws)
    where
      bytes = case size of
        Byte -> [(a, b0)]
        Half -> [(a + 1, b1), (a, b0)]
        Types.Word -> [(a + 3, b3), (a + 2, b2), (a + 1, b1), (a, b0)]
      b0 = slice d7 d0 w
      b1 = slice d15 d8 w
      b2 = slice d23 d16 w
      b3 = slice d31 d24 w

-- | A memory different in every test, nothing written yet.
genMem :: Gen TestMem
genMem = (\k1 k2 -> TestMem k1 k2 []) <$> genU32 <*> genU32
  where
    genU32 = fromIntegral <$> choose (0 :: Integer, 0xFFFFFFFF)

-- | Tags of the first three 4 KiB blocks of memory, sometimes others.
genTag :: Gen Tag
genTag = frequency [(10, elements [0, 1, 2]), (1, fromIntegral <$> choose (0 :: Int, 0xFFFFF))]

-- | Addresses in the first three 4 KiB blocks, sometimes anywhere.
genAddr :: Gen Address
genAddr =
  frequency
    [ (10, fromIntegral <$> choose (0 :: Int, 3 * 4096 - 1)),
      (1, fromIntegral <$> choose (0 :: Integer, 0xFFFFFFFF))
    ]

genWord :: Gen Word
genWord = oneof [elements [0, 1, 0xFF, 0x1234, 0x80000000, 0xDEADBEEF], fromIntegral <$> (arbitrary :: Gen Int)]

-- | Ideal memory's answer to a request.
idealAnswer :: (MemOps m) => m -> Request -> Maybe Word
idealAnswer mem req = inputMem (snd (idealServe mem req))

-- | The address of the last byte of an access.
lastByteOf :: Address -> Size -> Address
lastByteOf a size =
  a + case size of
    Byte -> 0
    Half -> 1
    Types.Word -> 3

-- | Does an access cross a word boundary?
straddles :: Address -> Size -> Bool
straddles a size = a `shiftR` 2 /= lastByteOf a size `shiftR` 2

-- Concrete runs -------------------------------------------------------------------

memU :: MemFn
memU = MemFn (\a -> slice d7 d0 (pack (a * 13 + 7)))

rdReq :: Address -> Size -> Request
rdReq a size = Just (MemRead a size)

wrReq :: Address -> Size -> Word -> Request
wrReq a size v = Just (MemWrite a size v)

-- | Serve requests one hop each, from the server at start in front of 'memU':
-- the driver's count of each hop, and the answer that ends it.
hops :: [Request] -> [(Int, Maybe Word)]
hops = go (initCacheSrv memU)
  where
    go _ [] = []
    go srv (req : reqs) =
      let k = srvDriver srv req
          (srv', ans) = serveN k srv req
       in (k, inputMem ans) : go srv' reqs

-- | Serve a request, then @n@ cycles without one.
serveN :: (MemOps m) => Int -> CacheSrv m -> Request -> (CacheSrv m, Input)
serveN n srv req
  | n <= 0 = serve srv req
  | otherwise = serveN (n - 1) (fst (serve srv req)) Nothing

p :: Int
p = fromIntegral penalty

runTests :: TestTree
runTests =
  testGroup
    "concrete runs"
    [ testCase "a read miss stalls, then the rest of its line hits" $
        hops [rdReq 0x100 Types.Word, rdReq 0x10C Types.Word, rdReq 0x106 Half]
          @?= [ (p, idealAnswer memU (rdReq 0x100 Types.Word)),
                (0, idealAnswer memU (rdReq 0x10C Types.Word)),
                (0, idealAnswer memU (rdReq 0x106 Half))
              ],
      testCase "a read that crosses into a missing line stalls for both" $
        P.map fst (hops [rdReq 0x10E Types.Word, rdReq 0x100 Types.Word, rdReq 0x110 Types.Word])
          @?= [2 * p, 0, 0],
      testCase "lines 4 KiB apart evict each other" $
        P.map fst (hops [rdReq 0x100 Types.Word, rdReq 0x1100 Types.Word, rdReq 0x100 Types.Word])
          @?= [p, p, p],
      testCase "a write hit is merged into the cached word" $
        hops [rdReq 0x100 Types.Word, wrReq 0x101 Byte 0xAB, rdReq 0x100 Types.Word]
          @?= [ (p, idealAnswer memU (rdReq 0x100 Types.Word)),
                (0, Just 0),
                (0, idealAnswer (memWrite 0x101 Byte 0xAB memU) (rdReq 0x100 Types.Word))
              ],
      testCase "a write that crosses into the next line is merged into both" $
        hops [rdReq 0x100 Types.Word, rdReq 0x110 Types.Word, wrReq 0x10E Types.Word 0x01020304, rdReq 0x10C Types.Word, rdReq 0x110 Types.Word]
          @?= [ (p, idealAnswer memU (rdReq 0x100 Types.Word)),
                (p, idealAnswer memU (rdReq 0x110 Types.Word)),
                (0, Just 0),
                (0, idealAnswer memW (rdReq 0x10C Types.Word)),
                (0, idealAnswer memW (rdReq 0x110 Types.Word))
              ]
    ]
  where
    memW = memWrite 0x10E Types.Word 0x01020304 memU
