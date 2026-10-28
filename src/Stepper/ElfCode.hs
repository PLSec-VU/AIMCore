{-# LANGUAGE GADTs #-}

module Stepper.ElfCode
  ( elfCode
  , DecodedTable
  , expectDecodeAgreement
  , expectDecodeDisagreement
  , withWrongFirstInstruction
  ) where

import Control.Monad.Catch (throwM)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Elf
import Data.Elf.Headers
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Elf.ElfLoader (getElfSegments, readElf)
import Instruction (Instruction (..), Reason4Stall (..), decode', nop)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import Numeric (showHex)
import qualified Stepper.RelState as RelState
import qualified SymFC.BuiltIn as SymFC
import Types (Address, Word)
import Prelude hiding (Word)

data SymbolRange = SymbolRange
  { rangeStart :: Integer
  , rangeSize :: Integer
  }

data Image = Image
  { imageSymbols :: Map String [SymbolRange]
  , imageChunks :: [(Integer, BS.ByteString)]
  }

type FetchTable = [(Integer, Integer)]

type DecodedTable = [(Word, Instruction)]

decodeAgrees :: DecodedTable -> Bool
decodeAgrees [] = True
decodeAgrees ((word, decoded) : rest) = decode' word == decoded && decodeAgrees rest

everyTableAgrees :: [DecodedTable] -> Bool
everyTableAgrees [] = True
everyTableAgrees (table : tables) = decodeAgrees table && everyTableAgrees tables

expectDecodeAgreement :: [DecodedTable] -> SymFC.Sym ()
expectDecodeAgreement tables = if everyTableAgrees tables then pure () else SymFC.halt

expectDecodeDisagreement :: [DecodedTable] -> SymFC.Sym ()
expectDecodeDisagreement tables = if everyTableAgrees tables then SymFC.halt else pure ()

differentInstruction :: Instruction -> Instruction
differentInstruction decoded = if decoded == Nop DecodeFail then nop else Nop DecodeFail

withWrongFirstInstruction :: DecodedTable -> DecodedTable
withWrongFirstInstruction [] = []
withWrongFirstInstruction ((word, decoded) : rest) = (word, differentInstruction decoded) : rest

instructionBytes :: Integer
instructionBytes = 4

loadImage :: FilePath -> IO Image
loadImage path = do
  elf@(Elf classS elfs) <- readElf path
  case classS of
    SELFCLASS64 -> throwM (userError "64-bit ELF not supported")
    SELFCLASS32 -> do
      header <- elfFindHeader elfs
      symtab <- elfFindSectionByName elfs ".symtab"
      symbols <- parseSymbolTable (ehData header) symtab elfs
      pure
        Image
          { imageSymbols =
              Map.fromListWith (++)
                [ (steName symbol, [SymbolRange (fromIntegral (steValue symbol)) (fromIntegral (steSize symbol))])
                | symbol <- symbols
                ]
          , imageChunks = [(fromIntegral addr, bytes) | (addr, bytes) <- getElfSegments elf]
          }

littleEndian :: [Integer] -> Integer
littleEndian = foldr (\byte rest -> (rest `shiftL` 8) .|. byte) 0

fetchInstruction :: Image -> Integer -> Either String Integer
fetchInstruction image addr =
  case [(base, bytes) | (base, bytes) <- imageChunks image, base <= addr, addr + instructionBytes <= base + toInteger (BS.length bytes)] of
    [] -> Left ("no ELF data at address " ++ show addr)
    (base, bytes) : _ ->
      let offset = fromIntegral (addr - base)
          instructionBytesOf = BS.take (fromIntegral instructionBytes) (BS.drop offset bytes)
       in Right (littleEndian (map fromIntegral (BS.unpack instructionBytesOf)))

lookupSymbol :: Image -> String -> Either String [SymbolRange]
lookupSymbol image name = maybe (Left ("symbol not found: " ++ name)) Right (Map.lookup name (imageSymbols image))

lowestStart :: [SymbolRange] -> Integer
lowestStart = minimum . map rangeStart

rangeFetchTable :: Image -> SymbolRange -> Either String FetchTable
rangeFetchTable image SymbolRange {..} =
  mapM (\addr -> (,) addr <$> fetchInstruction image addr) addresses
  where
    addresses = [rangeStart, rangeStart + instructionBytes .. rangeStart + rangeSize - instructionBytes]

symbolFetchTable :: Image -> String -> Either String FetchTable
symbolFetchTable image name = do
  ranges <- lookupSymbol image name
  concat <$> mapM (rangeFetchTable image) ranges

functionFetchTable :: Image -> [String] -> Either String FetchTable
functionFetchTable image symbols = do
  tables <- mapM (symbolFetchTable image) symbols
  pure (Map.toAscList (Map.fromList (concat tables)))

entryAddress :: Image -> [String] -> Either String Integer
entryAddress image symbols = case symbols of
  [] -> Left "function lists no symbols"
  entry : _ -> lowestStart <$> lookupSymbol image entry

type FetchedTable = [(Integer, Name)]

searchTree :: Name -> FetchedTable -> Q Exp
searchTree _ [] = [|Nothing|]
searchTree pc [(addr, here)] =
  [|if $(varE pc) == $(literal addr) then Just $(varE here) else Nothing|]
searchTree pc table =
  case splitAt (length table `div` 2) table of
    (lower, upper@((pivot, _) : _)) ->
      [|if $(varE pc) < $(literal pivot) then $(searchTree pc lower) else $(searchTree pc upper)|]
    (lower, []) -> searchTree pc lower

literal :: Integer -> Q Exp
literal = litE . integerL

fetchedName :: String -> Integer -> Name
fetchedName name addr = mkName (name ++ "At" ++ showHex addr "")

decodedTableName :: String -> Name
decodedTableName name = mkName (name ++ "Decoded")

instructionName :: String -> Integer -> Name
instructionName name word = mkName (name ++ "Instruction" ++ showHex word "")

distinctWords :: FetchTable -> [Integer]
distinctWords table = Set.toAscList (Set.fromList (map snd table))

instructionDeclarations :: String -> FetchTable -> Q [Dec]
instructionDeclarations name table = concat <$> mapM declare (distinctWords table)
  where
    declare word = do
      body <- lift (decode' (fromInteger word))
      pure
        [ SigD (instructionName name word) (ConT ''Instruction)
        , ValD (VarP (instructionName name word)) (NormalB body) []
        ]

decodedTableDeclarations :: String -> FetchTable -> Q [Dec]
decodedTableDeclarations name table = do
  body <- listE [[|($(literal word), $(varE (instructionName name word)))|] | word <- distinctWords table]
  pure
    [ SigD (decodedTableName name) (ConT ''DecodedTable)
    , ValD (VarP (decodedTableName name)) (NormalB body) []
    ]

fetchedDeclarations :: String -> FetchTable -> Q [Dec]
fetchedDeclarations name table = concat <$> mapM declare table
  where
    addresses = Map.fromList table
    following addr
      | Map.member (addr + instructionBytes) addresses = [|Just $(varE (fetchedName name (addr + instructionBytes)))|]
      | otherwise = [|Nothing|]
    declare (addr, word) = do
      body <- [|RelState.fetchedDecoded $(literal addr) $(literal word) $(varE (instructionName name word)) $(following addr)|]
      pure
        [ SigD (fetchedName name addr) (ConT ''RelState.Fetched)
        , ValD (VarP (fetchedName name addr)) (NormalB body) []
        ]

codeExpression :: String -> FetchTable -> Q Exp
codeExpression name table = do
  pc <- newName "pc"
  let sortedTable = sortOn fst table
      fetchedTable = [(addr, fetchedName name addr) | (addr, _) <- sortedTable]
      fetch = lamE [varP pc] (searchTree pc fetchedTable)
      addresses = listE (map (literal . fst) sortedTable)
  [|RelState.Code $fetch (RelState.KnownAddresses $addresses)|]

orFail :: Either String a -> Q a
orFail = either fail pure

functionDeclarations :: Image -> (String, [String]) -> Q [Dec]
functionDeclarations image (name, symbols) = do
  entry <- orFail (entryAddress image symbols)
  table <- orFail (functionFetchTable image symbols)
  let entryName = mkName (name ++ "Entry")
      codeName = mkName (name ++ "Code")
  code <- codeExpression name table
  instructions <- instructionDeclarations name table
  fetchedTable <- fetchedDeclarations name table
  decodedTable <- decodedTableDeclarations name table
  pure $
    instructions
      ++ fetchedTable
      ++ decodedTable
      ++ [ SigD entryName (ConT ''Address)
         , ValD (VarP entryName) (NormalB (LitE (IntegerL entry))) []
         , SigD codeName (ConT ''RelState.Code)
         , ValD (VarP codeName) (NormalB code) []
         ]

elfCode :: FilePath -> [(String, [String])] -> Q [Dec]
elfCode path functions = do
  addDependentFile path
  image <- runIO (loadImage path)
  concat <$> mapM (functionDeclarations image) functions
