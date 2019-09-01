{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Printer where

import Control.Monad.Reader
import Control.Monad.Writer.Custom
import Control.Monad.Identity

-- | 能在常数时间连接的字符串
newtype PString = PString { unwrapPString :: String -> String }
instance Semigroup PString where PString p <> PString q = PString (p . q)
instance Monoid PString where mempty = PString id
instance Show PString where show p = unwrapPString p ""

-- | Printer Monad 状态：缩进数和是否多行模式
data PrinterStatus = PrinterStatus
    { printerIndent :: Int
    , printerJoint :: Bool
    } deriving stock (Show, Eq)

-- | 默认的 Printer 状态
newPrinterStatus :: PrinterStatus
newPrinterStatus = PrinterStatus
    { printerIndent = 0
    , printerJoint = False
    }

-- | 增加缩进空格数目
increaseIndent :: Int -> PrinterStatus -> PrinterStatus
increaseIndent n s = s{ printerIndent = n + printerIndent s }

-- | 设置为单行模式
setJoint :: PrinterStatus -> PrinterStatus
setJoint s = s{ printerJoint = True }

-- | Printer Monad 转换器
newtype PrinterT m a = PrinterT
    { unwrapPrinterT :: ReaderT PrinterStatus (WriterT PString m) a }
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader PrinterStatus, MonadWriter PString)

-- | 一般的 MonadPrinter 类型类
class Monad m => MonadPrinter m where
    -- | 将字符串输出到 Printer
    plain :: String -> m ()
    -- | 给指定的 Printer 缩进增加指定的数目
    indent :: Int -> m a -> m a
    -- | 获取当前的缩进状态
    currentIndent :: m Int
    -- | 把指定的 Printer 中所有 plain 动作都放在同一行
    joint :: m a -> m a

instance Monad m => MonadPrinter (PrinterT m) where
    plain = makeLine . tell . PString . showString
    indent = local . increaseIndent
    currentIndent = asks printerIndent
    joint = makeLine . local setJoint

-- | 根据需要确定是否输出行首缩进空格和行尾换行符
makeLine :: Monad m => PrinterT m a -> PrinterT m a
makeLine proc = do
    indentLevel <- asks printerIndent
    isJointMode <- asks printerJoint
    unless isJointMode $ tell $ PString $ showString $ replicate indentLevel ' '
    res <- proc
    unless isJointMode $ tell $ PString $ showChar '\n'
    return res

-- | 将属于 Show 类型类的对象输出到 Printer
pShow :: (Monad m, Show a) => a -> PrinterT m ()
pShow = plain . show

-- | Printer Monad
type Printer = PrinterT Identity

-- | 运行 Printer Monad 转换器
runPrinterT :: Monad m => PrinterT m a -> m String
runPrinterT = fmap show . execWriterT . flip runReaderT newPrinterStatus . unwrapPrinterT

-- | 运行 Printer Monad
runPrinter :: Printer a -> String
runPrinter = runIdentity . runPrinterT
