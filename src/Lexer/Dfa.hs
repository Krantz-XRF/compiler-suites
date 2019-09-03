{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
module Lexer.Dfa where

import qualified Data.Array.IArray as Arr

import Control.Monad

import Lexer.Common
import Printer

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

-- | 将 DFA 以 GraphViz DOT 格式输出为一个字符串
dfaToDot :: (Enum c, Bounded c, Show c, Show a) => Dfa c a -> String
dfaToDot m = runPrinter $ do
    plain "digraph DFA {";
    indent 2 $ do
        plain "node[shape=point,color=white,fontcolor=white];"
        plain "start;"
        plain "rankdir=LR;"
        plain "overlap=false;"
        plain "node[color=black,fontcolor=black];"
        plain "node[shape=doublecircle];"
        forM_ [(s, x) | (FsmState s, AcceptState x) <- Arr.assocs (dfaStates m)] $ \(s, x) ->
            joint $ do pShow s; plain "[xlabel=\""; pShow x; plain "\"];"
        plain "node[shape=circle];"
        plain "start->0;"
        let inputs = dfaInputs m
        let maxInput = snd (Arr.bounds inputs)
        forM_ (Arr.assocs $ dfaTransition m) $ \((FsmState s, a), t@(FsmState tVal)) ->
            when (t /= InvalidState) $ joint $ do
                pShow s; plain "->"; pShow tVal
                plain "[label=\"["
                pShow $ inputs Arr.! a
                plain ".."
                pShow $ if a == maxInput
                    then maxBound
                    else pred $ inputs Arr.! succ a
                plain "]\"];"
    plain "}"
