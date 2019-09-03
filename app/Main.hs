{-# LANGUAGE DerivingStrategies #-}
module Main where

import Lexer.Regex
import Lexer.Nfa.Builder
import Lexer.Nfa.Determination
import Lexer.Dfa
import Lexer.Dfa.Minimize

import Control.Monad (mapM)

data Token
    = Letters
    | Numbers
    deriving stock (Show, Eq, Ord)

instance Semigroup Token where (<>) = min

main :: IO ()
main = putStr $ dfaToDot $ minimize $ determine $ buildNfa $ mapM (mapM buildRegex)
    [ (Letters, Range 'A' 'Z' `Or` Range 'a' 'z')
    , (Numbers, Some $ Range '0' '9')
    ]
