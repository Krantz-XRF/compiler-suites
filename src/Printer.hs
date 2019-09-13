{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Printer where

import Control.Monad.Reader
import Control.Monad.Writer.Custom
import Control.Monad.Identity

import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText

-- | 能在常数时间连接的字符串
newtype PString = PString { unwrapPString :: String -> String }
instance Semigroup PString where PString p <> PString q = PString (p . q)
instance Monoid PString where mempty = PString id
instance Show PString where show p = unwrapPString p ""
instance IsString PString where fromString = PString . showString

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
newtype PrinterT s m a = PrinterT
    { unwrapPrinterT :: ReaderT PrinterStatus (WriterT s m) a }
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader PrinterStatus, MonadWriter s)

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

instance (Monoid s, IsString s, Monad m) => MonadPrinter (PrinterT s m) where
    plain = makeLine . tell . fromString
    indent = local . increaseIndent
    currentIndent = asks printerIndent
    joint = makeLine . local setJoint

instance (Monoid s, IsString s, Monad m, a ~ ()) => IsString (PrinterT s m a) where
    fromString = plain

-- | 实现字符串高效连接
class (Monoid (ConcatHelper s), IsString (ConcatHelper s)) => EfficientConcat s where
    type ConcatHelper s :: *
    runConcat :: ConcatHelper s -> s
    {-# MINIMAL #-}
    type ConcatHelper s = s
    default runConcat :: ConcatHelper s ~ s => ConcatHelper s -> s
    runConcat = id

instance EfficientConcat String where
    type ConcatHelper String = PString
    runConcat s = unwrapPString s ""

instance EfficientConcat Text.Text where
    type ConcatHelper Text.Text = LText.Builder
    runConcat = LText.toStrict . LText.toLazyText

instance EfficientConcat LText.Text where
    type ConcatHelper LText.Text = LText.Builder
    runConcat = LText.toLazyText

-- | 根据需要确定是否输出行首缩进空格和行尾换行符
makeLine :: (Monoid s, IsString s, Monad m) => PrinterT s m a -> PrinterT s m a
makeLine proc = do
    indentLevel <- asks printerIndent
    isJointMode <- asks printerJoint
    unless isJointMode $ tell $ fromString $ replicate indentLevel ' '
    res <- proc
    unless isJointMode $ tell $ fromString "\n"
    return res

-- | 将属于 Show 类型类的对象输出到 Printer
pShow :: (Monad m, Monoid s, IsString s, Show a) => a -> PrinterT s m ()
pShow = plain . show

-- | Printer Monad
type Printer s = PrinterT s Identity

-- | 运行 Printer Monad 转换器
runPrinterT :: (Monoid s, Monad m, EfficientConcat s) => PrinterT (ConcatHelper s) m a -> m s
runPrinterT = fmap runConcat . execWriterT . flip runReaderT newPrinterStatus . unwrapPrinterT

-- | 运行 Printer Monad
runPrinter :: (Monoid s, EfficientConcat s) => Printer (ConcatHelper s) a -> s
runPrinter = runIdentity . runPrinterT
