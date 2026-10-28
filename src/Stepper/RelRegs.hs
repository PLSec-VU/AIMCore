{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.RelRegs
  ( RelRegs
  , regsFrom
  , readReg
  , writeReg
  , RegSlot (..)
  , slotFor
  , knownSlot
  , sourceSlot
  , readSlot
  , writeSlot
  ) where

import Stepper.Concrete (concreteRegIdx)
import Stepper.Rel (Rel (..))
import SymFC.BuiltIn qualified as SymFC
import Types (RegIdx, Word)
import Prelude hiding (Word)

data RegTree
  = RegLeaf (Rel Word)
  | RegSplit RegIdx RegTree RegTree

data RelRegs = RelRegs
  { knownIndexRegs :: RegTree
  , anyIndexRegs :: RegIdx -> Rel Word
  }

data Turn = Below | Above

data RegSlot
  = NoRegister
  | ZeroRegister
  | KnownRegister RegIdx [Turn]
  | AnyRegister RegIdx

halvingWidths :: [RegIdx]
halvingWidths = [16, 8, 4, 2, 1]

regsFrom :: (RegIdx -> Rel Word) -> RelRegs
regsFrom initial = RelRegs (spanning 0 halvingWidths) initial
  where
    spanning lowest [] = RegLeaf (initial lowest)
    spanning lowest (half : halves) =
      RegSplit (lowest + half) (spanning lowest halves) (spanning (lowest + half) halves)

data Pivots
  = NoPivot
  | Pivot RegIdx Pivots Pivots

pivots :: Pivots
pivots = spanning 0 halvingWidths
  where
    spanning _ [] = NoPivot
    spanning lowest (half : halves) =
      Pivot (lowest + half) (spanning lowest halves) (spanning (lowest + half) halves)

turnsTo :: RegIdx -> [Turn]
turnsTo idx = turnsWithin pivots
  where
    turnsWithin NoPivot = []
    turnsWithin (Pivot pivot below above)
      | idx < pivot = Below : turnsWithin below
      | otherwise = Above : turnsWithin above

zeroTurns :: [Turn]
zeroTurns = turnsTo 0

knownSlot :: RegIdx -> RegSlot
knownSlot 0 = ZeroRegister
knownSlot known = KnownRegister known (turnsTo known)

slotFor :: RegIdx -> SymFC.Sym RegSlot
slotFor idx = maybe (AnyRegister idx) knownSlot <$> concreteRegIdx idx

sourceSlot :: Maybe RegIdx -> SymFC.Sym RegSlot
sourceSlot = maybe (pure NoRegister) slotFor

lookupAlong :: [Turn] -> RegTree -> Rel Word
lookupAlong _ (RegLeaf value) = value
lookupAlong (Above : turns) (RegSplit _ _ above) = lookupAlong turns above
lookupAlong (Below : turns) (RegSplit _ below _) = lookupAlong turns below
lookupAlong [] (RegSplit _ below _) = lookupAlong [] below

readSlot :: RelRegs -> RegSlot -> Rel Word
readSlot _ NoRegister = Same 0
readSlot regs ZeroRegister = lookupAlong zeroTurns (knownIndexRegs regs)
readSlot regs (KnownRegister _ turns) = lookupAlong turns (knownIndexRegs regs)
readSlot regs (AnyRegister idx) = anyIndexRegs regs idx

readReg :: RelRegs -> Maybe RegIdx -> SymFC.Sym (Rel Word)
readReg regs idx = readSlot regs <$> sourceSlot idx

replaceAlong :: [Turn] -> Rel Word -> RegTree -> RegTree
replaceAlong _ value (RegLeaf _) = RegLeaf value
replaceAlong (Above : turns) value (RegSplit pivot below above) = RegSplit pivot below (replaceAlong turns value above)
replaceAlong (Below : turns) value (RegSplit pivot below above) = RegSplit pivot (replaceAlong turns value below) above
replaceAlong [] value (RegSplit pivot below above) = RegSplit pivot (replaceAlong [] value below) above

replaceMatchingLeaf :: RegIdx -> Rel Word -> RegIdx -> RegTree -> RegTree
replaceMatchingLeaf rd value lowest (RegLeaf old) = RegLeaf (if lowest == rd then value else old)
replaceMatchingLeaf rd value lowest (RegSplit pivot below above) =
  RegSplit pivot (replaceMatchingLeaf rd value lowest below) (replaceMatchingLeaf rd value pivot above)

withChained :: RegIdx -> Rel Word -> (RegIdx -> Rel Word) -> RegIdx -> Rel Word
withChained rd value chain idx = if idx == rd then value else chain idx

writeUnknownIndex :: RegIdx -> Rel Word -> RelRegs -> RelRegs
writeUnknownIndex 0 _ regs = regs
writeUnknownIndex rd value (RelRegs tree chain) =
  RelRegs (replaceMatchingLeaf rd value 0 tree) (withChained rd value chain)

writeSlot :: RegSlot -> Rel Word -> RelRegs -> RelRegs
writeSlot NoRegister _ regs = regs
writeSlot ZeroRegister _ regs = regs
writeSlot (KnownRegister rd turns) value (RelRegs tree chain) =
  RelRegs (replaceAlong turns value tree) (withChained rd value chain)
writeSlot (AnyRegister rd) value regs = writeUnknownIndex rd value regs

writeReg :: RegIdx -> Rel Word -> RelRegs -> SymFC.Sym RelRegs
writeReg rd value regs = do
  slot <- slotFor rd
  pure (writeSlot slot value regs)
