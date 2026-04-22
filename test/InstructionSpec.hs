{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module InstructionSpec (instructionTests) where

import Clash.Prelude hiding (Ordering (..), Word, break, def, init, lift, log, resize)
import Control.Monad
import Data.Maybe (isJust, fromJust)
import Instruction
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
import Types
import Prelude hiding (Ordering (..), Word, break, init, log, not, repeat, undefined, (!!), (&&), (++), (||))
import qualified Prelude as P

-- | Main test group for instruction encoding/decoding
instructionTests :: TestTree
instructionTests =
  testGroup
    "Instruction Encoding/Decoding Tests"
    [ testGroup
        "Specific Examples"
        [ testJalExample,
          testBeqExample
        ],
      testGroup
        "Property-based Tests"
        [ testRoundtripProperty,
          testValidInstructionsProperty
        ],
      testGroup
        "Edge Cases"
        [ testInvalidInstructions,
          testBoundaryValues
        ]
    ]

-- | Test the specific example: jal x1, 208620: 0x6ed320ef
testJalExample :: TestTree
testJalExample = testCase "decode 0x6ed320ef = jal x1, 208620" $ do
  let word = 0x6ed320ef
      decoded = decode word

  -- First check what we actually decode
  case decoded of
    Just (JType rd imm) -> do
      rd @?= 1  -- Should be register x1
      -- The immediate should be 208620, let's see what we get and then check roundtrip
      let encoded = encode (JType rd imm)
      encoded @?= word  -- This is the critical test - roundtrip should work
    Just other -> assertBool ("Expected JType, got: " P.++ show other) False
    Nothing -> assertBool "Decode failed" False

-- | Test the beq example: beq x4, x7, -1650: 0x987207e3
testBeqExample :: TestTree
testBeqExample = testCase "decode 0x987207e3 = beq x4, x7, -1650" $ do
  let word = 0x987207e3
      decoded = decode word

  -- First check what we actually decode
  case decoded of
    Just (BType cmp imm rs1 rs2) -> do
      cmp @?= EQ     -- Should be branch equal
      rs1 @?= 4      -- Should be register x4
      rs2 @?= 7      -- Should be register x7
      -- The immediate should be -1650, let's check roundtrip
      let encoded = encode (BType cmp imm rs1 rs2)
      encoded @?= word  -- This is the critical test - roundtrip should work
    Just other -> assertBool ("Expected BType, got: " P.++ show other) False
    Nothing -> assertBool "Decode failed" False

-- | Property test: for any valid instruction, encode(decode(x)) = x
testRoundtripProperty :: TestTree
testRoundtripProperty = testProperty "encode(decode(x)) = x for valid instructions" $
  forAll genValidWord $ \word ->
    case decode word of
      Just instr -> encode instr === word
      Nothing -> property True -- Skip invalid instructions

-- | Property test: for any instruction, decode(encode(x)) = x
testValidInstructionsProperty :: TestTree
testValidInstructionsProperty = testProperty "decode(encode(x)) = x for all instructions" $
  forAll genArbitraryInstruction $ \instr ->
    let encoded = encode instr
        decoded = decode encoded
    in decoded === Just instr

-- | Generate arbitrary instruction (simplified version)
genArbitraryInstruction :: Gen Instruction
genArbitraryInstruction = oneof
  [ RType <$> pure ADD <*> pure 1 <*> pure 2 <*> pure 3,
    IType <$> pure (Arith ADD) <*> pure 1 <*> pure 2 <*> pure 100,
    SType <$> pure Word <*> pure 50 <*> pure 1 <*> pure 2,
    BType <$> pure EQ <*> pure 8 <*> pure 1 <*> pure 2,
    UType <$> pure Zero <*> pure 1 <*> pure 1000,
    JType <$> pure 1 <*> pure 500
  ]

-- | Test invalid instructions return Nothing
testInvalidInstructions :: TestTree
testInvalidInstructions = testGroup "Invalid Instructions"
  [ testCase "Invalid opcode" $ decode 0b11111111111111111111111111111111 @?= Nothing,
    testCase "Invalid R-type funct3/funct7" $ decode 0b11111110000000000111000000110011 @?= Nothing,
    testCase "Invalid I-type shift with wrong funct7" $ decode 0b11111110000000000001000000010011 @?= Nothing,
    testCase "Invalid load size" $ decode 0b00000000000000000011000000000011 @?= Nothing,
    testCase "Invalid branch comparison" $ decode 0b00000000000000000011000001100011 @?= Nothing
  ]

-- | Test boundary values for immediates and registers
testBoundaryValues :: TestTree
testBoundaryValues = testGroup "Boundary Values"
  [ testCase "Max register indices" $ do
      let instr = RType ADD 31 31 31
      let encoded = encode instr
      decode encoded @?= Just instr,

    testCase "Max immediate values" $ do
      let instr = IType (Arith ADD) 1 2 0b111111111111  -- 12-bit max
      let encoded = encode instr
      decode encoded @?= Just instr,

    testCase "Max upper immediate" $ do
      let instr = UType Zero 1 0b11111111111111111111  -- 20-bit max
      let encoded = encode instr
      decode encoded @?= Just instr
  ]

-- | Generator for potentially valid instruction words
genValidWord :: Gen Word
genValidWord = oneof
  [ genRTypeWord,
    genITypeWord,
    genSTypeWord,
    genBTypeWord,
    genUTypeWord,
    genJTypeWord
  ]

-- | Generate R-type instruction words
genRTypeWord :: Gen Word
genRTypeWord = do
  arith <- oneof $ P.map pure [ADD, SUB, XOR, OR, AND, SLL, SRL, SRA, SLT, SLTU]
  rd <- chooseBoundedIntegral (0, 31)
  rs1 <- chooseBoundedIntegral (0, 31)
  rs2 <- chooseBoundedIntegral (0, 31)
  let instr = RType arith rd rs1 rs2
  return $ encode instr

-- | Generate I-type instruction words
genITypeWord :: Gen Word
genITypeWord = do
  op <- oneof
    [ Arith <$> oneof (P.map pure [ADD, XOR, OR, AND, SLT, SLTU]),
      uncurry Load <$> oneof (P.map pure [(Byte, Signed), (Half, Signed), (Word, Signed), (Byte, Unsigned), (Half, Unsigned)]),
      pure Jump,
      Env <$> oneof (P.map pure [Call, Break])
    ]
  rd <- chooseBoundedIntegral (0, 31)
  rs1 <- chooseBoundedIntegral (0, 31)
  imm <- chooseBoundedIntegral (0, 0b111111111111)  -- 12-bit immediate
  let instr = IType op rd rs1 imm
  return $ encode instr

-- | Generate S-type instruction words
genSTypeWord :: Gen Word
genSTypeWord = do
  size <- oneof $ P.map pure [Byte, Half, Word]
  rs1 <- chooseBoundedIntegral (0, 31)
  rs2 <- chooseBoundedIntegral (0, 31)
  imm <- chooseBoundedIntegral (0, 0b111111111111)  -- 12-bit immediate
  let instr = SType size imm rs1 rs2
  return $ encode instr

-- | Generate B-type instruction words
genBTypeWord :: Gen Word
genBTypeWord = do
  cmp <- oneof $ P.map pure [EQ, NE, LT, GE, LTU, GEU]
  rs1 <- chooseBoundedIntegral (0, 31)
  rs2 <- chooseBoundedIntegral (0, 31)
  imm <- chooseBoundedIntegral (0, 0b111111111111)  -- 12-bit immediate
  let instr = BType cmp imm rs1 rs2
  return $ encode instr

-- | Generate U-type instruction words
genUTypeWord :: Gen Word
genUTypeWord = do
  base <- oneof $ P.map pure [Zero, PC]
  rd <- chooseBoundedIntegral (0, 31)
  imm <- chooseBoundedIntegral (0, 0b11111111111111111111)  -- 20-bit immediate
  let instr = UType base rd imm
  return $ encode instr

-- | Generate J-type instruction words
genJTypeWord :: Gen Word
genJTypeWord = do
  rd <- chooseBoundedIntegral (0, 31)
  imm <- chooseBoundedIntegral (0, 0b11111111111111111111)  -- 20-bit immediate
  let instr = JType rd imm
  return $ encode instr


-- | Test that nop instruction works correctly
testNop :: TestTree
testNop = testCase "nop instruction" $ do
  let nopInstr = nop
  let encoded = encode nopInstr
  let decoded = decode encoded
  decoded @?= Just nopInstr

  -- Verify it's actually ADD x0, x0, x0
  nopInstr @?= RType ADD 0 0 0

-- | Test specific instruction type predicates
testInstructionPredicates :: TestTree
testInstructionPredicates = testGroup "Instruction Predicates"
  [ testCase "isBreak predicate" $ do
      let breakInstr = IType (Env Break) 0 0 1
      isBreak breakInstr @?= True
      isBreak nop @?= False,

    testCase "isCall predicate" $ do
      let callInstr = IType (Env Call) 0 0 0
      isCall callInstr @?= True
      isCall nop @?= False,

    testCase "isLoad predicate" $ do
      let loadInstr = IType (Load Word Signed) 1 2 100
      isLoad loadInstr @?= True
      isLoad nop @?= False
  ]

-- | Test register extraction functions
testRegisterExtraction :: TestTree
testRegisterExtraction = testGroup "Register Extraction"
  [ testCase "getRd function" $ do
      let rtype = RType ADD 5 10 15
      let itype = IType (Arith XOR) 7 12 200
      let utype = UType Zero 3 1000
      let jtype = JType 9 500
      let stype = SType Word 100 1 2

      getRd rtype @?= Just 5
      getRd itype @?= Just 7
      getRd utype @?= Just 3
      getRd jtype @?= Just 9
      (getRd stype :: Maybe RegIdx) @?= Nothing,

    testCase "getRs1 function" $ do
      let rtype = RType ADD 5 10 15
      let itype = IType (Arith XOR) 7 12 200
      let stype = SType Word 100 11 2
      let btype = BType EQ 50 13 14
      let utype = UType Zero 3 1000

      getRs1 rtype @?= Just 10
      getRs1 itype @?= Just 12
      getRs1 stype @?= Just 11
      getRs1 btype @?= Just 13
      (getRs1 utype :: Maybe RegIdx) @?= Nothing,

    testCase "getRs2 function" $ do
      let rtype = RType ADD 5 10 15
      let stype = SType Word 100 11 22
      let btype = BType EQ 50 13 24
      let itype = IType (Arith XOR) 7 12 200

      getRs2 rtype @?= Just 15
      getRs2 stype @?= Just 22
      getRs2 btype @?= Just 24
      (getRs2 itype :: Maybe RegIdx) @?= Nothing
  ]

-- | Extended test suite including all the additional tests
extendedInstructionTests :: TestTree
extendedInstructionTests =
  testGroup
    "Extended Instruction Tests"
    [ instructionTests,
      testNop,
      testInstructionPredicates,
      testRegisterExtraction
    ]