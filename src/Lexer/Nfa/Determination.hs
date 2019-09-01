module Lexer.Nfa.Determination where

import qualified Data.Map.Strict as Map
import qualified Data.Array.IArray as Arr
import qualified Data.Set as Set

import Data.Maybe

import Lexer.Nfa
import Lexer.Dfa
import Lexer.Common

determine :: Nfa c a -> Dfa c a
determine = undefined

-- | a-弧转换：经过一个 a-弧能够到达的状态
convert :: Nfa c a -> FsmState -> FsmInput -> Set.Set FsmState
convert m s a = fromMaybe Set.empty $ nfaTransition m Arr.! s Map.!? a

-- | 集合的 a-弧转换
convertSet :: Nfa c a -> Set.Set FsmState -> FsmInput -> Set.Set FsmState
convertSet m s a = foldl Set.union Set.empty $ map (\x -> convert m x a) $ Set.toAscList s

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
        in addToClosure m (Set.toAscList es) $ addToClosure m xs $ res `Set.union` es
