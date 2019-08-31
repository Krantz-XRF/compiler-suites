module Printer where

import Control.Monad.Identity

newtype PrinterT m a = PrinterT { runPrinterT :: Int -> m (a, String -> String) }

instance Monad m => Functor (PrinterT m) where
    fmap f a = PrinterT $ \n -> do
        (x, p) <- runPrinterT a n
        return (f x, p)

instance Monad m => Applicative (PrinterT m) where
    pure x = PrinterT $ \_ -> return (x, id)
    mf <*> mx = PrinterT $ \n -> do
        (f, p1) <- runPrinterT mf n
        (x, p2) <- runPrinterT mx n
        return (f x, p1 . p2)

instance Monad m => Monad (PrinterT m) where
    return = pure
    m >>= f = PrinterT $ \n -> do
        (x, p1) <- runPrinterT m n
        (res, p2) <- runPrinterT (f x) n
        return (res, p1 . p2)

plain :: Monad m => String -> PrinterT m ()
plain s = PrinterT $ \_ -> return ((), showString s)

line :: Monad m => PrinterT m ()
line = PrinterT $ \n -> return ((), showChar '\n' . showString (replicate n ' '))

indent :: Monad m => Int -> PrinterT m a -> PrinterT m a
indent n p = PrinterT $ \_ -> runPrinterT p n

pShow :: (Monad m, Show a) => a -> PrinterT m ()
pShow = plain . show

type Printer = PrinterT Identity

runPrinter :: Printer a -> String
runPrinter = ($ "") . snd . runIdentity . ($ 0) . runPrinterT
