module Lexer.Nfa.Determination where

import qualified Data.Map.Strict as Map
import qualified Data.Array.IArray as Arr
import qualified Data.Set as Set

import Lexer.Nfa
import Lexer.Dfa
import Lexer.Common

determine :: Nfa c a -> Dfa c a
determine = undefined

-- | 仅仅经过一个 Epsilon 弧能够到达的状态
epsilonConvert :: Nfa c a -> FsmState -> Set.Set FsmState
epsilonConvert m s = nfaTransition m Arr.! s Map.! Epsilon

-- | 状态的 Epsilon 闭包
epsilonClosure :: Nfa c a -> FsmState -> Set.Set FsmState
epsilonClosure m s = addToClosure [s] Set.empty
    where addToClosure :: [FsmState] -> Set.Set FsmState -> Set.Set FsmState
          addToClosure [] res = res
          addToClosure (x:xs) res
              | x `Set.member` res = addToClosure xs res
              | otherwise = let es = epsilonConvert m x
                  in addToClosure (Set.toAscList es) $ addToClosure xs $ res `Set.union` es
