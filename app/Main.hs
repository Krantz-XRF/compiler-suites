{-# LANGUAGE DerivingStrategies #-}
module Main where

import Lexer.Regex
import Lexer.Nfa.Builder
import Lexer.Nfa.Determination
import Lexer.Dfa
import Lexer.Dfa.Minimize

import Control.Monad (mapM)

data Token
    = Type
    | Variable
    | Decimal
    | Octal
    | Hexadecimal
    deriving stock (Show, Eq, Ord)

instance Semigroup Token where (<>) = min

sng :: c -> Regex c
sng c = Range c c

main :: IO ()
main = do
    putStr (dfaToDot dfa)
    str <- getLine
    print (runDfa dfa str)

dfa :: Dfa Char Token
dfa = minimize $ determine $ buildNfa $ mapM (mapM buildRegex)
    [ (Type,        Range 'A' 'Z' `Concat` Many (Range 'A' 'Z' `Or` Range 'a' 'z'))
    , (Variable,    (Range 'a' 'z' `Or` sng '_') `Concat` Some (Range 'A' 'Z' `Or` Range 'a' 'z'))
    , (Decimal,     Range '1' '9' `Concat` Some (Range '0' '9'))
    , (Octal,       sng '0' `Concat` Many (Range '0' '7'))
    , (Hexadecimal, sng '0' `Concat` (sng 'x' `Or` sng 'X') `Concat`
                    Some (Range '0' '9' `Or` Range 'A' 'F' `Or` Range 'a' 'z'))
    ]
