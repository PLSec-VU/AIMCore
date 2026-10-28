module Stepper.RelRegs
  ( RelRegs
  , regsFrom
  , readReg
  , writeReg
  ) where

import Stepper.Concrete (concreteRegIdx)
import Stepper.Rel (Rel (..))
import Types (RegIdx, Word)
import Prelude hiding (Word)

data RegTree
  = RegLeaf (Rel Word)
  | RegSplit RegIdx RegTree RegTree

data RelRegs = RelRegs
  { knownIndexRegs :: RegTree
  , anyIndexRegs :: RegIdx -> Rel Word
  }

halvingWidths :: [RegIdx]
halvingWidths = [16, 8, 4, 2, 1]

regsFrom :: (RegIdx -> Rel Word) -> RelRegs
regsFrom initial = RelRegs (spanning 0 halvingWidths) initial
  where
    spanning lowest [] = RegLeaf (initial lowest)
    spanning lowest (half : halves) =
      RegSplit (lowest + half) (spanning lowest halves) (spanning (lowest + half) halves)

lookupTree :: RegIdx -> RegTree -> Rel Word
lookupTree _ (RegLeaf value) = value
lookupTree idx (RegSplit pivot below above)
  | idx < pivot = lookupTree idx below
  | otherwise = lookupTree idx above

readAt :: RelRegs -> RegIdx -> Rel Word
readAt regs idx = case concreteRegIdx idx of
  Just known -> lookupTree known (knownIndexRegs regs)
  Nothing -> anyIndexRegs regs idx

readReg :: RelRegs -> Maybe RegIdx -> Rel Word
readReg regs = maybe (Same 0) (readAt regs)

replacePath :: RegIdx -> Rel Word -> RegTree -> RegTree
replacePath _ value (RegLeaf _) = RegLeaf value
replacePath rd value (RegSplit pivot below above)
  | rd < pivot = RegSplit pivot (replacePath rd value below) above
  | otherwise = RegSplit pivot below (replacePath rd value above)

replaceMatchingLeaf :: RegIdx -> Rel Word -> RegIdx -> RegTree -> RegTree
replaceMatchingLeaf rd value lowest (RegLeaf old) = RegLeaf (if lowest == rd then value else old)
replaceMatchingLeaf rd value lowest (RegSplit pivot below above) =
  RegSplit pivot (replaceMatchingLeaf rd value lowest below) (replaceMatchingLeaf rd value pivot above)

writeTree :: RegIdx -> Rel Word -> RegTree -> RegTree
writeTree rd value tree = case concreteRegIdx rd of
  Just known -> replacePath known value tree
  Nothing -> replaceMatchingLeaf rd value 0 tree

writeReg :: RegIdx -> Rel Word -> RelRegs -> RelRegs
writeReg 0 _ regs = regs
writeReg rd value (RelRegs tree chain) =
  RelRegs (writeTree rd value tree) (\idx -> if idx == rd then value else chain idx)
