{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Monad.Writer.Custom
    ( module Control.Monad.Writer.Class
    , WriterT(.., WriterT)
    , runWriterT
    , execWriterT
    , Writer
    , runWriter
    , execWriter
    ) where

import Control.Monad.State
import Control.Monad.Writer.Class
import Control.Monad.Identity

-- | Writer Monad，用 State Monad 实现，避免内存泄漏
newtype WriterT w m a = WriterT' { runWriterT' :: StateT w m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- | 匹配成通常熟悉的 WriterT 的模式
-- 即 @newtype WriterT w m a = WriterT { runWriterT :: m (w, a) }@
pattern WriterT :: (Monoid w, Monad m) => m (a, w) -> WriterT w m a
pattern WriterT m <- (runWriterT -> m)
    where WriterT m = lift m >>= writer

-- | 运行 WriterT Monad 转换器，得到返回值和写出的信息
runWriterT :: (Monad m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT m = runStateT (runWriterT' m) mempty

-- | 运行 WriterT Monad 转换器，舍弃返回值
execWriterT :: (Monad m, Monoid w) => WriterT w m a -> m w
execWriterT m = snd <$> runWriterT m

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    tell w = WriterT' $ modify' (<> w)
    listen m = (,) <$> m <*> WriterT' get
    pass m = do
        (x, f) <- m
        WriterT' (modify' f)
        return x

-- | 普通的 Writer Monad
type Writer w = WriterT w Identity

-- | 运行 Writer Monad，得到返回值和写出的信息
runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

-- | 运行 Writer Monad，舍弃返回值
execWriter :: Monoid w => Writer w a -> w
execWriter = runIdentity . execWriterT
