{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lexer.Dfa where

import qualified Data.Array.Unboxed as Arr

import Control.Monad
import Data.Bifunctor
import Data.String

import Lexer.Common
import Printer
import Utility

import Lexer.Dfa.Input

import Language.Haskell.TH.Syntax

-- | DFA 各个状态的信息
data StateInfo a = AcceptState a | NormalState deriving stock (Show, Lift)

instance Semigroup a => Semigroup (StateInfo a) where
    NormalState <> x = x
    x <> NormalState = x
    AcceptState x <> AcceptState y = AcceptState (x <> y)

instance Semigroup a => Monoid (StateInfo a) where
    mempty = NormalState

-- | 确定的有穷自动机：
-- * 状态用整数 FsmState (= Int) 表示
-- * 约定开始状态总是 0
-- * 状态集合，标记所有结束状态的序号
-- * 输入必须要预先编码成一个 FsmInput (~ Int)
-- * 注：输入的 0 在 NFA 中已经预留给了 Epsilon，这里的输入从 1 开始
data Dfa c a = Dfa
    { dfaTransition :: Arr.UArray (FsmState, FsmInput) FsmState
    , dfaStates :: Arr.Array FsmState (StateInfo a)
    , dfaInputs :: Arr.UArray FsmInput c
    }
deriving stock instance (Show a, Show c, Arr.IArray Arr.UArray c) => Show (Dfa c a)

instance (Lift c, Lift a, Arr.IArray Arr.UArray c) => Lift (Dfa c a) where
    lift (Dfa trans states inputs) = do
        trans' <- liftArray trans
        states' <- liftArray states
        inputs' <- liftArray inputs
        return (ConE 'Dfa `AppE` trans' `AppE` states' `AppE` inputs')

-- | 用于提升数组：提升方法是转换成 @listArray@
liftArray :: (Lift i, Lift e, Arr.Ix i, Arr.IArray a e) => a i e -> Q Exp
liftArray arr =
    let bounds = Arr.bounds arr
        elems = Arr.elems arr
    in [| Arr.listArray bounds elems |]

-- | DFA 的无效状态
pattern InvalidState :: FsmState
pattern InvalidState = -1

-- | 将 DFA 以 GraphViz DOT 格式输出为一个字符串
dfaToDot :: (Enum c, Bounded c, Show c, Arr.IArray Arr.UArray c,
             EfficientConcat s, Show a, Monoid s, IsString s)
         => Dfa c a -> s
dfaToDot m = runPrinter $ do
    "digraph DFA {"
    indent 2 $ do
        "node[shape=point,color=white,fontcolor=white];"
        "start;"
        "rankdir=LR;"
        "overlap=false;"
        "node[color=black,fontcolor=black];"
        "node[shape=doublecircle];"
        forM_ [(s, x) | (s, AcceptState x) <- Arr.assocs (dfaStates m)] $ \(s, x) ->
            joint $ do pShow s; "[xlabel=\""; pShow x; "\"];"
        "node[shape=circle];"
        "start->0;"
        let inputs = dfaInputs m
        let maxInput = snd (Arr.bounds inputs)
        forM_ (Arr.assocs $ dfaTransition m) $ \((s, a), t) ->
            when (t /= InvalidState) $ joint $ do
                pShow s; "->"; pShow t
                "[label=\"["
                pShow $ inputs Arr.! a
                ".."
                pShow $ if a == maxInput
                    then maxBound
                    else pred $ inputs Arr.! succ a
                "]\"];"
    "}"

-- | 在指定字符串上运行 DFA 获得结果
runDfa :: forall s c a . (DfaInput s c, Ord c, Arr.IArray Arr.UArray c)
       => Dfa c a -> s -> Either [FsmInput] (a, s, s)
runDfa m str = bimap collectInput handleSuccess $ go (Left startState) startState 0 str where
    handleSuccess (a, n) = let (l, r) = splitTokenRest n str in (a, l, r)
    inputs = dfaInputs m
    states = dfaStates m
    startState = 0
    transition = dfaTransition m
    collectInput :: FsmState -> [FsmInput]
    collectInput s = filter (\i -> transition Arr.! (s, i) /= InvalidState)
                   $ uncurry enumFromTo $ Arr.bounds inputs
    trans :: FsmState -> c -> FsmState
    trans s c = transition Arr.! (s, binarySearch inputs c)
    go :: Either FsmState (a, Int) -> FsmState -> Int -> s -> Either FsmState (a, Int)
    go res s n (uncons -> Just (x, xs)) = case trans s x of
        InvalidState -> res
        t -> let n' = succ n in case states Arr.! t of
            NormalState -> go res t n' xs
            AcceptState a -> go (Right (a, n')) t n' xs
    go res _ _ _ = res
