module Stepper.AddrMap
  ( AddrMap
  , empty
  , insert
  , lookup
  , foldrWithKey
  ) where

import Types (Address)
import Prelude hiding (lookup)

data AddrMap v
  = Tip
  | Bin Int Address v (AddrMap v) (AddrMap v)

empty :: AddrMap v
empty = Tip

height :: AddrMap v -> Int
height Tip = 0
height (Bin h _ _ _ _) = h

node :: Address -> v -> AddrMap v -> AddrMap v -> AddrMap v
node k v l r = Bin (1 + max (height l) (height r)) k v l r

rotateRight :: AddrMap v -> AddrMap v
rotateRight (Bin _ k v (Bin _ lk lv ll lr) r) = node lk lv ll (node k v lr r)
rotateRight t = t

rotateLeft :: AddrMap v -> AddrMap v
rotateLeft (Bin _ k v l (Bin _ rk rv rl rr)) = node rk rv (node k v l rl) rr
rotateLeft t = t

balanceFactor :: AddrMap v -> Int
balanceFactor Tip = 0
balanceFactor (Bin _ _ _ l r) = height l - height r

leansLeft :: AddrMap v -> Bool
leansLeft t = balanceFactor t > 0

leansRight :: AddrMap v -> Bool
leansRight t = balanceFactor t < 0

balance :: Address -> v -> AddrMap v -> AddrMap v -> AddrMap v
balance k v l r
  | height l > height r + 1 =
      rotateRight (node k v (if leansRight l then rotateLeft l else l) r)
  | height r > height l + 1 =
      rotateLeft (node k v l (if leansLeft r then rotateRight r else r))
  | otherwise = node k v l r

insert :: Address -> v -> AddrMap v -> AddrMap v
insert k v Tip = Bin 1 k v Tip Tip
insert k v (Bin h nk nv l r)
  | k < nk = balance nk nv (insert k v l) r
  | k > nk = balance nk nv l (insert k v r)
  | otherwise = Bin h k v l r

lookup :: Address -> AddrMap v -> Maybe v
lookup _ Tip = Nothing
lookup k (Bin _ nk nv l r)
  | k < nk = lookup k l
  | k > nk = lookup k r
  | otherwise = Just nv

foldrWithKey :: (Address -> v -> b -> b) -> b -> AddrMap v -> b
foldrWithKey _ z Tip = z
foldrWithKey f z (Bin _ k v l r) = foldrWithKey f (f k v (foldrWithKey f z r)) l
