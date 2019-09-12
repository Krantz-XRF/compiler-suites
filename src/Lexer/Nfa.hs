{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lexer.Nfa where

import qualified Data.Map.Strict as Map
import qualified Data.Array.Unboxed as Arr
import qualified Data.Set as Set

import Control.Monad (forM_)

import Lexer.Common
import Printer

-- | 非确定的有穷自动机：
-- * 状态用整数 FsmState (= Int) 表示
-- * 输入必须要预先编码成一个 FsmInput (~ Int)
-- * 输入 (0 :: FsmInput) 用于表示 Epsilon
-- * 开始状态
-- * 接受状态集合
data Nfa c a = Nfa
    { nfaTransition :: Arr.Array FsmState (Map.Map FsmInput (Set.Set FsmState))
    , nfaStart :: FsmState
    , nfaFinal :: Map.Map FsmState a
    , nfaInputs :: Arr.UArray FsmInput c
    } deriving stock (Functor)
deriving stock instance (Show a, Show c, Arr.IArray Arr.UArray c) => Show (Nfa c a)

-- | 用于 NFA 输出使用的占位类型
data Empty = Empty

instance Show Empty where show _ = ""

-- | 将 NFA 以 GraphViz DOT 格式输出为一个字符串
nfaToDot :: (Enum c, Bounded c, Show c, Show a, Arr.IArray Arr.UArray c)
         => Nfa c a -> String
nfaToDot m = runStringPrinter $ do
    plain "digraph NFA {";
    indent 2 $ do
        plain "node[shape=point,color=white,fontcolor=white];"
        plain "start;"
        plain "rankdir=LR;"
        plain "overlap=false;"
        plain "node[color=black,fontcolor=black];"
        plain "node[shape=doublecircle];"
        forM_ (Map.toAscList $ nfaFinal m) $ \(s, x) ->
            joint $ do pShow s; plain "[xlabel=\""; pShow x; plain "\"];"
        plain "node[shape=circle];"
        plain "start->0;"
        let trans = nfaTransition m
        let inputs = nfaInputs m
        let maxInput = snd (Arr.bounds inputs)
        let (s0, sN) = Arr.bounds trans
        forM_ [s0 .. sN] $ \s ->
            let arcs = trans Arr.! s
            in forM_ (Map.toAscList arcs) $ \(a, ts) ->
                forM_ (Set.toAscList ts) $ \t -> joint $ do
                    pShow s; plain "->"; pShow t
                    plain "[label=\""
                    if a == Epsilon then plain "ε" else do
                        plain "["
                        pShow $ inputs Arr.! a
                        plain ".."
                        pShow $ if a == maxInput
                            then maxBound
                            else pred $ inputs Arr.! succ a
                        plain "]"
                    plain "\"];"
    plain "}"

-- | 去除 NFA 所有结束状态的标签
-- * 如果标签数据很大，可以防止输出为 DOT 时过大
-- * 如果标签数据未实现 Show 类型类，可以去除标签输出
withoutLabels :: Nfa c a -> Nfa c Empty
withoutLabels = fmap (const Empty)
