{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
module Lexer.Nfa.Builder where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.ST

import qualified Data.Array.ST.Safe as Arr
import qualified Data.Array.Unboxed as Arr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Data.Array.Unsafe as Unsafe (unsafeFreeze)

import Lexer.Common
import Lexer.Nfa
import Utility

-- | 非确定有穷自动机构建过程中的中间状态
-- * 当前的状态总数
-- * 当前的状态转移的描述
data NfaBuilderState c s = NfaBuilderState
    { transitionArcs :: Arr.STArray s FsmState (Map.Map FsmInput (Set.Set FsmState))
    , inputSplitPoints :: Arr.UArray FsmInput c
    }

-- | 允许在 NFA Builder 中执行 ST 操作
liftST :: ST s a -> NfaBuilder c s a
liftST = NfaBuilder . lift . lift

-- | 构建 NFA 的通用接口
class (Ord c, Monad m) => MonadNfaBuilder c m | m -> c where
    -- | 创建新状态
    newState :: m FsmState
    -- | 状态转移
    transition :: c -> c -> FsmState -> FsmState -> m ()
    -- | 通过 Epsilon 弧转移状态
    epsilonTrans :: FsmState -> FsmState -> m ()

-- | NFA Builder
newtype NfaBuilder c s a = NfaBuilder
    { unwrapNfaBuilder :: ReaderT (NfaBuilderState c s) (StateT Int (ST s)) a
    } deriving newtype (Functor, Applicative, Monad)
deriving newtype instance MonadReader (NfaBuilderState c s) (NfaBuilder c s)
deriving newtype instance MonadState Int (NfaBuilder c s)

-- | 依赖于 Builder 的临时 NFA
data TempNfa c = TempNfa
    { tempInitialState :: FsmState
    , tempAcceptedState :: FsmState
    } deriving stock (Show)
type role TempNfa phantom

-- | 快速创建临时 NFA
newNfa :: MonadNfaBuilder c m => (FsmState -> FsmState -> m ()) -> m (TempNfa c)
newNfa proc = do
    s <- newState
    t <- newState
    proc s t
    return (TempNfa s t)

instance (Ord c, Enum c, Arr.IArray Arr.UArray c) => MonadNfaBuilder c (NfaBuilder c s) where
    newState = do
        s <- get
        put (s + 1)
        return s
    transition lo hi from to = do
        ps <- asks inputSplitPoints
        let loIdx = binarySearch ps lo
        let hiIdx = binarySearch ps (succ hi)
        tArcs <- asks transitionArcs
        liftST $ forM_ [loIdx .. pred hiIdx] $ \i ->
            modifyArray tArcs from (Map.alter (\case
                Nothing -> Just (Set.singleton to)
                Just oldTos -> Just (Set.insert to oldTos)) i)
    epsilonTrans from to = do
        tArcs <- asks transitionArcs
        liftST $ modifyArray tArcs from (Map.alter (\case
            Nothing -> Just (Set.singleton to)
            Just oldTos -> Just (Set.insert to oldTos)) Epsilon)

-- | 收集的 NFA 输入信息
data NfaInfo c = NfaInfo
    { nfaInputSplitPoints :: Set.Set c
    , nfaTotalStateCount :: Int
    } deriving stock (Show)

emptyNfaInfo :: Bounded c => NfaInfo c
emptyNfaInfo = NfaInfo (Set.singleton minBound) 0

updateSplitPoints :: (Set.Set c -> Set.Set c) -> (NfaInfo c -> NfaInfo c)
updateSplitPoints f info = info{ nfaInputSplitPoints = f (nfaInputSplitPoints info) }

-- | NFA 的输入收集器
newtype NfaInputCollector c a = NfaInputCollector
    { unwrapNfaInputCollector :: State (NfaInfo c) a
    } deriving newtype (Functor, Applicative, Monad)
deriving newtype instance MonadState (NfaInfo c) (NfaInputCollector c)

-- | NFA 的输入
-- * Bounded 提供的 minBound 作为输入集合最左的分点，保证二分查找总能成功
-- * Enum 提供的 succ 用于将闭区间 [l, r] 转化为开区间 [l, succ r)
-- * Ord 提供的比较函数用于最终的二分查找算法
type InputType c = (Bounded c, Enum c, Ord c)

instance (Enum c, Ord c) => MonadNfaBuilder c (NfaInputCollector c) where
    newState = do
        s <- gets nfaTotalStateCount
        modify $ \info -> info{ nfaTotalStateCount = s + 1 }
        return s
    transition lo hi _ _ = modify $ updateSplitPoints $ Set.insert lo . Set.insert (succ hi)
    epsilonTrans _ _ = return ()

-- | 收集 NFA Builder 中的所有输入数据分点，并初始化状态转移的存储空间
collectInput :: (Bounded c, Arr.IArray Arr.UArray c)
             => NfaInputCollector c a -> ST s (NfaBuilderState c s)
collectInput (NfaInputCollector f) = do
    let NfaInfo ps cnt = execState f emptyNfaInfo
    -- 生成的数组中，下标是从 1 开始的，因为 0 预留为 Epsilon
    let psArr = Arr.listArray (FsmInput 1, FsmInput $ Set.size ps) (Set.toAscList ps)
    arcs <- Arr.newArray (0, cnt - 1) Map.empty
    return NfaBuilderState
        { transitionArcs = arcs
        , inputSplitPoints = psArr
        }

-- | 从 NFA Builder 构建 NFA
buildNfa :: forall c f a . (InputType c, Foldable f, Arr.IArray Arr.UArray c)
         => (forall m . MonadNfaBuilder c m => m (f (a, TempNfa c))) -> Nfa c a
buildNfa builders = runST $ do
    let builderMono :: forall m . MonadNfaBuilder c m => m (FsmState, Map.Map FsmState a)
        builderMono = do
            s0 <- newState
            nfas <- builders
            let addNfa ts (x, TempNfa s t) = do
                    epsilonTrans s0 s
                    return (Map.insert t x ts)
            ts <- foldM addNfa Map.empty nfas
            return (s0, ts)
    st <- collectInput builderMono
    (s, ts) <- (unwrapNfaBuilder builderMono `runReaderT` st) `evalStateT` 0
    trans <- Unsafe.unsafeFreeze (transitionArcs st)
    return Nfa
        { nfaTransition = trans
        , nfaStart = s
        , nfaFinal = ts
        , nfaInputs = inputSplitPoints st
        }
