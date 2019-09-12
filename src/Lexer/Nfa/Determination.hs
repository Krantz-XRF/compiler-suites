{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lexer.Nfa.Determination where

import qualified Data.Map.Strict as Map
import qualified Data.Array.Unboxed as Arr
import qualified Data.Set as Set

import Control.Monad.State

import Data.Maybe
import Data.List (foldl1')
import Data.Foldable

import Lexer.Nfa
import Lexer.Dfa
import Lexer.Common

-- | 构建 DFA 的中间状态
-- 我们会按产生的先后顺序生成所有状态的 GO 函数，所以会存储为一个倒序的列表
data DfaBuilder a = DfaBuilder
    { dfaBuilderTransition :: AppendList [FsmState]
    , dfaBuilderStates :: Map.Map (Set.Set FsmState) (FsmState, StateInfo a)
    } deriving stock (Show)

-- | 使用给定的状态合并方法将 NFA 确定化为 DFA
determineWith :: forall c a . Arr.IArray Arr.UArray c => (a -> a -> a) -> Nfa c a -> Dfa c a
determineWith select m =
    let -- NFA 的起始状态的 Epsilon-闭包成为 DFA 的起始状态
        start = epsilonClosure m (nfaStart m)
        -- DFA 的状态信息
        stateCount = Map.size (dfaBuilderStates res)
        startState = 0
        maxState = stateCount - 1
        -- 状态信息的初始值：仅含有 NFA 起始状态的 Epsilon-闭包
        initStates = Map.singleton start (startState, collectInfo start)
        -- 从初始状态出发构建 DFA 的转换表
        res = execState (processStates [start]) (DfaBuilder Nil initStates)
        -- 构造 DFA 最终的转换表形式
        transition = Arr.listArray ((startState, inputL), (maxState, inputR))
                   $ concat $ runAppendList (dfaBuilderTransition res)
        -- 构造 DFA 最终的状态信息形式
        statesInfo = Arr.array (startState, maxState)
                   $ map snd $ Map.toAscList $ dfaBuilderStates res
    in Dfa { dfaTransition = transition
           , dfaStates = statesInfo
           , dfaInputs = nfaInputs m
           } where
    -- 输入字符的范围信息
    (inputL, inputR) = Arr.bounds (nfaInputs m)
    -- 用于构建 DFA，将一个状态列表中的所有状态依次构造转换边，并添加到 DFA 中
    processStates :: [Set.Set FsmState] -> State (DfaBuilder a) ()
    processStates [] = return ()
    processStates xs = do
        -- 先计算状态集合 x 的转换边，并记录产生的所有新状态集合
        -- transList :: [(FsmInput, FsmState)] 转换边的集合
        -- newStates :: [Maybe (Set.Set FsmState)] 可能转换到的新状态集合
        (transList, newStates) <- fmap (unzip . concat) $
            forM xs $ \x -> forM [inputL .. inputR] $ \i -> do
                states <- gets dfaBuilderStates
                -- 先计算目标状态的 Epsilon-闭包
                let tgt = epsilonClosureSet m (convertSet m x i)
                if Set.null tgt
                -- 目标集合是空集，即转换到非法状态（InvalidState），未产生新状态
                then return (InvalidState, Nothing)
                else case Map.lookup tgt states of
                    -- 目标集合已经记录过，转换到该状态，未产生新状态
                    Just (tgtState, _) -> return (tgtState, Nothing)
                    -- 目标集合未记录……
                    Nothing -> do
                        -- 转换到该状态
                        let n = Map.size states
                        let info = collectInfo tgt
                        -- 添加该集合
                        modifyStates (Map.insert tgt (n, info))
                        -- 报告为新状态
                        return (n, Just tgt)
        -- 将生成的转换边附加到末尾
        modifyTransition (`Append` transList)
        processStates (catMaybes newStates)

    modifyStates f = modify $ \(DfaBuilder t s) -> DfaBuilder t (f s)
    modifyTransition f = modify $ \(DfaBuilder t s) -> DfaBuilder (f t) s

    collectInfo :: Set.Set FsmState -> StateInfo a
    collectInfo s =
        let outputs = catMaybes $ flip Map.lookup (nfaFinal m) <$> Set.toAscList s
        in case outputs of
            [] -> NormalState
            lst -> AcceptState (foldl1' select lst)

-- | 将状态视为半群来合并将 NFA 确定化为 DFA
determine :: (Semigroup a, Arr.IArray Arr.UArray c) => Nfa c a -> Dfa c a
determine = determineWith (<>)

-- | a-弧转换：经过一个 a-弧能够到达的状态
convert :: Nfa c a -> FsmState -> FsmInput -> Set.Set FsmState
convert m s a = fromMaybe Set.empty $ nfaTransition m Arr.! s Map.!? a

-- | 集合的 a-弧转换
convertSet :: Nfa c a -> Set.Set FsmState -> FsmInput -> Set.Set FsmState
convertSet m s a = foldl' Set.union Set.empty $ map (\x -> convert m x a) $ Set.toAscList s

-- | Epsilon-弧转换：经过一个 Epsilon-弧能够到达的状态
epsilonConvert :: Nfa c a -> FsmState -> Set.Set FsmState
epsilonConvert m s = convert m s Epsilon

-- | 状态的 Epsilon 闭包
epsilonClosure :: Nfa c a -> FsmState -> Set.Set FsmState
epsilonClosure m s = addToClosure m [s] Set.empty

-- | 状态集合的 Epsilon 闭包
epsilonClosureSet :: Nfa c a -> Set.Set FsmState -> Set.Set FsmState
epsilonClosureSet m s = addToClosure m (Set.toAscList s) Set.empty

-- | 将一系列状态及其 Epsilon 闭包添加到集合中
addToClosure :: Nfa c a -> [FsmState] -> Set.Set FsmState -> Set.Set FsmState
addToClosure _ [] res = res
addToClosure m (x:xs) res
    | x `Set.member` res = addToClosure m xs res
    | otherwise = let es = epsilonConvert m x
        in addToClosure m (Set.toAscList es) $ addToClosure m xs $ x `Set.insert` res
