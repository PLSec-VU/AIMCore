{-# LANGUAGE GADTs #-}

module Stepper.ElfCode
  ( elfCode
  ) where

import Control.Monad.Catch (throwM)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Elf
import Data.Elf.Headers
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Elf.ElfLoader (getElfSegments, readElf)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import qualified Stepper.RelState as RelState
import Types (Address)

data SymbolRange = SymbolRange
  { rangeStart :: Integer
  , rangeSize :: Integer
  }

data Image = Image
  { imageSymbols :: Map String SymbolRange
  , imageChunks :: [(Integer, BS.ByteString)]
  }

type FetchTable = [(Integer, Integer)]

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
              Map.fromList
                [ (steName symbol, SymbolRange (fromIntegral (steValue symbol)) (fromIntegral (steSize symbol)))
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

lookupSymbol :: Image -> String -> Either String SymbolRange
lookupSymbol image name = maybe (Left ("symbol not found: " ++ name)) Right (Map.lookup name (imageSymbols image))

symbolFetchTable :: Image -> String -> Either String FetchTable
symbolFetchTable image name = do
  SymbolRange {..} <- lookupSymbol image name
  let addresses = [rangeStart, rangeStart + instructionBytes .. rangeStart + rangeSize - instructionBytes]
  mapM (\addr -> (,) addr <$> fetchInstruction image addr) addresses

functionFetchTable :: Image -> [String] -> Either String FetchTable
functionFetchTable image symbols = do
  tables <- mapM (symbolFetchTable image) symbols
  pure (Map.toAscList (Map.fromList (concat tables)))

entryAddress :: Image -> [String] -> Either String Integer
entryAddress image symbols = case symbols of
  [] -> Left "function lists no symbols"
  entry : _ -> rangeStart <$> lookupSymbol image entry

searchTree :: Name -> FetchTable -> Q Exp
searchTree _ [] = [|Nothing|]
searchTree pc [(addr, word)] =
  [|if $(varE pc) == $(literal addr) then Just $(literal word) else Nothing|]
searchTree pc table =
  case splitAt (length table `div` 2) table of
    (lower, upper@((pivot, _) : _)) ->
      [|if $(varE pc) < $(literal pivot) then $(searchTree pc lower) else $(searchTree pc upper)|]
    (lower, []) -> searchTree pc lower

literal :: Integer -> Q Exp
literal = litE . integerL

codeExpression :: FetchTable -> Q Exp
codeExpression table = do
  pc <- newName "pc"
  let sortedTable = sortOn fst table
      fetch = lamE [varP pc] (searchTree pc sortedTable)
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
  code <- codeExpression table
  pure
    [ SigD entryName (ConT ''Address)
    , ValD (VarP entryName) (NormalB (LitE (IntegerL entry))) []
    , SigD codeName (ConT ''RelState.Code)
    , ValD (VarP codeName) (NormalB code) []
    ]

elfCode :: FilePath -> [(String, [String])] -> Q [Dec]
elfCode path functions = do
  addDependentFile path
  image <- runIO (loadImage path)
  concat <$> mapM (functionDeclarations image) functions
