{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Text.IO as Text

import Lexer
import Utils

import Language.Haskell.TH.Syntax

main :: IO ()
main = do
    Text.putStrLn (dfaToDot dfa)
    str <- Text.getLine
    print (runDfa dfa str)

dfa :: Dfa Char Token
dfa = $(lift $ buildDfaWith mergeEq (<>)
    [ (Type,        Range 'A' 'Z' `Concat` Many (Range 'A' 'Z' `Or` Range 'a' 'z'))
    , (Variable,    (Range 'a' 'z' `Or` sng '_') `Concat` Some (Range 'A' 'Z' `Or` Range 'a' 'z'))
    , (Decimal,     Range '1' '9' `Concat` Some (Range '0' '9'))
    , (Octal,       sng '0' `Concat` Many (Range '0' '7'))
    , (Hexadecimal, sng '0' `Concat` (sng 'x' `Or` sng 'X') `Concat`
                    Some (Range '0' '9' `Or` Range 'A' 'F' `Or` Range 'a' 'z'))
    ])
