{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Monad.Writer.Custom
    ( module Control.Monad.Writer.Class
    , WriterT(WriterT)
    , runWriterT
    , execWriterT
    , execWriter
    ) where

import Control.Monad.State
import Control.Monad.Writer.Class
import Control.Monad.Identity

-- | Writer Monad，用 State Monad 实现，避免内存泄漏
newtype WriterT w m a = WriterT' { runWriterT' :: StateT w m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

runWriterT :: (Monad m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT m = runStateT (runWriterT' m) mempty

pattern WriterT :: (Monoid w, Monad m) => m (a, w) -> WriterT w m a
pattern WriterT m <- (runWriterT -> m)
    where WriterT m = lift m >>= writer

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    tell w = WriterT' $ modify' (<> w)
    listen m = (,) <$> m <*> WriterT' get
    pass m = do
        (x, f) <- m
        WriterT' (modify' f)
        return x

type Writer w = WriterT w Identity

execWriterT :: (Monad m, Monoid w) => WriterT w m a -> m w
execWriterT m = snd <$> runWriterT m

execWriter :: Monoid w => Writer w a -> w
execWriter = runIdentity . execWriterT
