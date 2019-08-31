{-# LANGUAGE DerivingStrategies #-}
module Main where

import Lexer.Nfa
import Lexer.Nfa.Builder
import Lexer.Regex

import Control.Monad (mapM)

data Token
    = Letters
    | Numbers
    deriving stock (Show)

main :: IO ()
main = putStr $ nfaToDot $ buildNfa $ mapM (mapM buildRegex)
    [ (Letters, Range 'A' 'Z' `Or` Range 'a' 'z')
    , (Numbers, Some $ Range '0' '9')
    ]
