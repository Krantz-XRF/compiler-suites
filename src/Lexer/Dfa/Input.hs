{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lexer.Dfa.Input where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

-- | 串 s 能够作为 Dfa c a 的输入
class DfaInput s c | s -> c where
    uncons :: s -> Maybe (c, s)
    splitTokenRest :: Int -> s -> (s, s)

instance DfaInput [c] c where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
    splitTokenRest = splitAt

instance DfaInput Text.Text Char where
    uncons = Text.uncons
    splitTokenRest = Text.splitAt

instance DfaInput LText.Text Char where
    uncons = LText.uncons
    splitTokenRest = LText.splitAt . fromIntegral
