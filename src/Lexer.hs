{-# LANGUAGE FlexibleContexts #-}
module Lexer
    ( module Lexer.Regex
    , module Lexer.Dfa
    , mergeEq
    , mergeSemigroup
    , buildDfaWith
    ) where

import Lexer.Regex
import Lexer.Nfa.Builder
import Lexer.Nfa.Determination
import Lexer.Dfa
import Lexer.Dfa.Minimize

import Control.Monad (mapM)

import Data.Array.Unboxed as Arr

-- | 从正则表达式构建构建一个 Dfa
buildDfaWith :: (Arr.IArray Arr.UArray c, Ord c, Enum c, Bounded c)
             => (a -> a -> Maybe a) -> (a -> a -> a) -> [(a, Regex c)] -> Dfa c a
buildDfaWith merge select lst
    = minimizeWith merge
    $ determineWith select
    $ buildNfa
    $ mapM (mapM buildRegex) lst
