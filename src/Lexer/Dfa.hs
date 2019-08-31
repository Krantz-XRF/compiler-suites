{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
module Lexer.Dfa where

import qualified Data.Array.IArray as Arr

import Lexer.Common

-- | DFA 各个状态的信息
type StateInfo = Maybe

-- | DFA 的一个接受状态，接受时产生 a 类型的值
pattern AcceptState :: a -> StateInfo a
pattern AcceptState x = Just x

-- | DFA 的非接受状态
pattern NormalState :: StateInfo a
pattern NormalState = Nothing

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
