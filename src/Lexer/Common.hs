{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Lexer.Common
    ( FsmInput(.., Epsilon)
    , FsmState(..)
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
