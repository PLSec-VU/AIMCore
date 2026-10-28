{-# LANGUAGE ImportQualifiedPost #-}

module Stepper.AddrMap
  ( AddrMap
  , empty
  , insert
  , insertWith
  , lookup
  , foldrWithKey
  , partitionByRange
  ) where

import SymFC.BuiltIn qualified as SymFC
import Types (Address)
import Prelude hiding (lookup)

data Color = Red | Black

data AddrMap v
  = Tip
  | Bin Color Address v (AddrMap v) (AddrMap v)

empty :: AddrMap v
empty = Tip

balance :: Color -> Address -> v -> AddrMap v -> AddrMap v -> AddrMap v
balance Black z zv (Bin Red y yv (Bin Red x xv a b) c) d = Bin Red y yv (Bin Black x xv a b) (Bin Black z zv c d)
balance Black z zv (Bin Red x xv a (Bin Red y yv b c)) d = Bin Red y yv (Bin Black x xv a b) (Bin Black z zv c d)
balance Black x xv a (Bin Red z zv (Bin Red y yv b c) d) = Bin Red y yv (Bin Black x xv a b) (Bin Black z zv c d)
balance Black x xv a (Bin Red y yv b (Bin Red z zv c d)) = Bin Red y yv (Bin Black x xv a b) (Bin Black z zv c d)
balance color k v l r = Bin color k v l r

insertRed :: (Maybe v -> v) -> Address -> AddrMap v -> AddrMap v
insertRed combine k Tip = Bin Red k (combine Nothing) Tip Tip
insertRed combine k (Bin color nk nv l r)
  | k < nk = balance color nk nv (insertRed combine k l) r
  | k > nk = balance color nk nv l (insertRed combine k r)
  | otherwise = Bin color k (combine (Just nv)) l r

blackRoot :: AddrMap v -> AddrMap v
blackRoot (Bin _ k v l r) = Bin Black k v l r
blackRoot Tip = Tip

insertWith :: (Maybe v -> v) -> Address -> AddrMap v -> AddrMap v
insertWith combine k tree = blackRoot (insertRed combine k tree)

insert :: Address -> v -> AddrMap v -> AddrMap v
insert k v = insertWith (const v) k

lookup :: Address -> AddrMap v -> Maybe v
lookup _ Tip = Nothing
lookup k (Bin _ nk nv l r)
  | k < nk = lookup k l
  | k > nk = lookup k r
  | otherwise = Just nv

foldrWithKey :: (Address -> v -> b -> b) -> b -> AddrMap v -> b
foldrWithKey _ z Tip = z
foldrWithKey f z (Bin _ k v l r) = foldrWithKey f (f k v (foldrWithKey f z r)) l

singleton :: Address -> v -> AddrMap v
singleton k v = Bin Black k v Tip Tip

lowestKey :: Address -> AddrMap v -> Address
lowestKey k Tip = k
lowestKey _ (Bin _ k _ l _) = lowestKey k l

highestKey :: Address -> AddrMap v -> Address
highestKey k Tip = k
highestKey _ (Bin _ k _ _ r) = highestKey k r

unionDisjoint :: AddrMap v -> AddrMap v -> AddrMap v
unionDisjoint a b = foldrWithKey insert b a

partitionByRange
  :: (Address -> Address -> SymFC.Sym Bool)
  -> AddrMap v
  -> SymFC.Sym (AddrMap v, AddrMap v)
partitionByRange _ Tip = pure (Tip, Tip)
partitionByRange rangeIsClear tree@(Bin _ k v l r) = do
  clear <- rangeIsClear (lowestKey k l) (highestKey k r)
  if clear then pure (Tip, tree) else partitionParts rangeIsClear k v l r

partitionParts
  :: (Address -> Address -> SymFC.Sym Bool)
  -> Address
  -> v
  -> AddrMap v
  -> AddrMap v
  -> SymFC.Sym (AddrMap v, AddrMap v)
partitionParts _ k v Tip Tip = pure (singleton k v, Tip)
partitionParts rangeIsClear k v l r = do
  (hitLeft, clearLeft) <- partitionByRange rangeIsClear l
  (hitKey, clearKey) <- partitionByRange rangeIsClear (singleton k v)
  (hitRight, clearRight) <- partitionByRange rangeIsClear r
  pure
    ( unionDisjoint hitLeft (unionDisjoint hitKey hitRight)
    , unionDisjoint clearLeft (unionDisjoint clearKey clearRight)
    )
