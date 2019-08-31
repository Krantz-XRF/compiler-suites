{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Printer where

import Control.Monad.Reader
import Control.Monad.Writer.Custom
import Control.Monad.Identity

newtype PString = PString { unwrapPString :: String -> String }
instance Semigroup PString where PString p <> PString q = PString (p . q)
instance Monoid PString where mempty = PString id
instance Show PString where show p = unwrapPString p ""

data PrinterStatus = PrinterStatus
    { printerIndent :: Int
    , printerSeparators :: [PString]
    }

-- | Printer Monad 转换器
newtype PrinterT m a = PrinterT
    { runPrinterT :: ReaderT PrinterStatus (WriterT PString m) a }
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader PrinterStatus, MonadWriter PString)

plain :: Monad m => String -> PrinterT m ()
plain = tell . PString . showString

line :: Monad m => PrinterT m ()
line = do
    n <- asks printerIndent
    tell $ PString $ showChar '\n' . showString (replicate n ' ')

indent :: Monad m => Int -> PrinterT m a -> PrinterT m a
indent n = local (\ps -> ps{ printerIndent = printerIndent ps + n })

pShow :: (Monad m, Show a) => a -> PrinterT m ()
pShow = plain . show

type Printer = PrinterT Identity

runPrinter :: Printer a -> String
runPrinter = show . execWriter . (`runReaderT` PrinterStatus 0 []) . runPrinterT
