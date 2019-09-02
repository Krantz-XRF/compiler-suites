{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
module Lexer.Dfa where

import qualified Data.Array.IArray as Arr

import Lexer.Common

-- | DFA 各个状态的信息
data StateInfo a = AcceptState a | NormalState deriving stock (Show)

instance Semigroup a => Semigroup (StateInfo a) where
    NormalState <> x = x
    x <> NormalState = x
    AcceptState x <> AcceptState y = AcceptState (x <> y)

instance Semigroup a => Monoid (StateInfo a) where
    mempty = NormalState

-- | 确定的有穷自动机：
-- * 状态用整数 FsmState (~ Int) 表示
-- * 约定开始状态总是 0
-- * 状态集合，标记所有结束状态的序号
-- * 输入必须要预先编码成一个 FsmInput (~ Int)
-- * 注：输入的 0 在 NFA 中已经预留给了 Epsilon，这里的输入从 1 开始
data Dfa c a = Dfa
    { dfaTransition :: Arr.Array (FsmState, FsmInput) FsmState
    , dfaStates :: Arr.Array FsmState (StateInfo a)
    , dfaInputs :: Arr.Array FsmInput c
    } deriving stock (Show)

-- | DFA 的无效状态
pattern InvalidState :: FsmState
pattern InvalidState = FsmState (-1)
