module Lexer.Dfa.Minimize where

import Data.Array.IArray as Arr
import Data.Map.Strict as Map
import Data.Set as Set

import Data.Maybe

import Lexer.Common
import Lexer.Dfa

-- | 最小化 DFA，使用给定的函数判断接受状态是否可以合并
-- * 如果 merge 在两个附加值上计算出 Nothing，则认为不可以合并
-- * 如果 merge 给出 Just x，将有可能把他们合并为 x
minimizeWith :: (a -> a -> Maybe a) -> Dfa c a -> Dfa c a
minimizeWith merge m = m
    { dfaTransition = newTrans
    , dfaStates = newStates
    } where
    -- 计算出的分组信息
    parent = applyUntilEqual splitGroup initialGroup
    -- 由分组确定的每一组的代表元素
    newStateSet = Set.fromList (Arr.elems parent)
    newStateList = Set.toAscList newStateSet
    newStateCount = Set.size newStateSet
    reps = Map.fromDistinctAscList $ zip newStateList [FsmState 0 ..]
    -- 新状态
    (minState, maxState) = (FsmState 0, FsmState (newStateCount - 1))
    -- 从原始状态到新状态的映射
    replacement = fmap (reps Map.!) parent
    -- 新 DFA 的状态转换
    newTrans = Arr.listArray ((minState, minInput), (maxState, maxInput))
             [ let t = trans (original Arr.! s) i
               in if t == InvalidState
               then InvalidState
               else replacement Arr.! t
             | s <- [minState .. maxState]
             , i <- [minInput .. maxInput]
             ]
    newStates = Arr.listArray (minState, maxState)
              [ dfaStates m Arr.! (original Arr.! s)
              | s <- [minState .. maxState]
              ]
    -- 新状态向着它表示的原始状态分组的代表元素的映射
    original :: Arr.Array FsmState FsmState
    original = Arr.listArray (FsmState 0, FsmState (newStateCount - 1)) newStateList
    -- 反复迭代直到相等
    applyUntilEqual :: Eq a => (a -> a) -> a -> a
    applyUntilEqual f x = let y = f x in if y == x then x else applyUntilEqual f y
    -- 初始分组：所有非接受状态为一组，附加值可合并的接受状态为一组
    initialGroup = splitOnce (const False) equiv where
        equiv x y = equivInfo (statesInfo Arr.! x) (statesInfo Arr.! y)
        equivInfo NormalState NormalState = True
        equivInfo (AcceptState x) (AcceptState y) = isJust (merge x y)
        equivInfo _ _ = False
    -- 迭代步骤：使用上一次分组的信息进一步划分组合
    splitGroup :: Arr.Array FsmState FsmState -> Arr.Array FsmState FsmState
    splitGroup groups = splitOnce lastUniq newEquiv where
        lastEquiv InvalidState InvalidState = True
        lastEquiv InvalidState _ = False
        lastEquiv _ InvalidState = False
        lastEquiv x y = groups Arr.! x == groups Arr.! y
        newEquiv x y = all (\i -> lastEquiv (trans x i) (trans y i)) inputs
        lastUniq x = groups Arr.! x == x
    -- 最一般的分组函数：uniq 指定必然会新开一组的元素，equiv 判断两个元素是否为一组
    splitOnce :: (FsmState -> Bool)
              -> (FsmState -> FsmState -> Bool)
              -> Arr.Array FsmState FsmState
    splitOnce uniq equiv = Arr.listArray stateBounds (go [] allStates) where
        go :: [FsmState] -> [FsmState] -> [FsmState]
        go _  []     = []
        go rp (x:xs)
            | uniq x = x : go (x:rp) xs
            | otherwise = case dropWhile (not . equiv x) rp of
                [] -> x : go (x:rp) xs
                (r:_) -> r : go rp xs
    -- DFA 状态信息
    statesInfo = dfaStates m
    stateBounds = Arr.bounds statesInfo
    allStates = uncurry enumFromTo stateBounds
    -- DFA 输入相关
    inputs = enumFromTo minInput maxInput
    (minInput, maxInput) = Arr.bounds (dfaInputs m)
    trans :: FsmState -> FsmInput -> FsmState
    trans x i = dfaTransition m Arr.! (x, i)

-- | 最小化 DFA，仅合并完全相同的接受状态
minimize :: Eq a => Dfa c a -> Dfa c a
minimize = minimizeWith $ \x y -> if x == y then Just x else Nothing
