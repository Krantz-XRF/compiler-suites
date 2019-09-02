{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Lexer.Common
    ( FsmInput(.., Epsilon)
    , FsmState(..)
    , AppendList(.., Nil, Append)
    , runAppendList
    , unsnoc
    ) where

import qualified Data.Array.IArray as Arr
import Utility (LinearIndex(..))

-- | 状态机的状态 FsmState
newtype FsmState = FsmState { unwrapFsmState :: Int }
    deriving stock (Show)
    deriving newtype (Eq, Ord, Enum, Arr.Ix)

-- | 状态机的输入 FsmInput
newtype FsmInput = FsmInput { unwrapFsmInput :: Int }
    deriving stock (Show)
    deriving newtype (Eq, Ord, Enum, Arr.Ix)
instance LinearIndex FsmInput where
    median (FsmInput x) (FsmInput y) = let m = x + (y - x) `quot` 2 in FsmInput m

-- | 状态机的空输入 Epsilon
pattern Epsilon :: FsmInput
pattern Epsilon = FsmInput 0

-- | 用于把元素附加到末尾而不是开头的列表
newtype AppendList a = AppendList { unwrapAppendList :: [a] } deriving stock (Show)

-- | 把 AppendList 转换为普通列表
runAppendList :: AppendList a -> [a]
runAppendList = reverse . unwrapAppendList

-- | 空的 AppendList
pattern Nil :: AppendList a
pattern Nil = AppendList []

-- | 向列表尾部附加一个元素
pattern Append :: AppendList a -> a -> AppendList a
pattern Append xs x <- (unsnoc -> Just (xs, x))
    where Append (AppendList xs) x = AppendList (x:xs)

-- | 尝试将列表分解为头部和最后一个元素
unsnoc :: AppendList a -> Maybe (AppendList a, a)
unsnoc (AppendList []) = Nothing
unsnoc (AppendList (x:xs)) = Just (AppendList xs, x)
