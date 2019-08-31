module Utility where

import qualified Data.Array.IArray as Arr
import qualified Data.Array.MArray.Safe as Arr

-- | 允许二分搜索创建区间中点
class (Arr.Ix a, Enum a) => LinearIndex a where
    median :: a -> a -> a

-- | 二分搜索
binarySearch :: (Ord a, Arr.IArray v a, LinearIndex i) => v i a -> a -> i
binarySearch v x = go lo (succ hi)
    where (lo, hi) = Arr.bounds v
          go l r | succ l == r = l
                 | c == x      = m
                 | c < x       = go m r
                 | otherwise   = go l m
                 where m = median l r
                       c = v Arr.! m

-- | 就地修改数组元素
modifyArray :: (Arr.MArray a e m, Arr.Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr idx f = Arr.readArray arr idx >>= Arr.writeArray arr idx . f
