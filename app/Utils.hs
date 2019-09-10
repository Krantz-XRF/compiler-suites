{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveLift #-}
module Utils where

import Language.Haskell.TH.Syntax
import Lexer.Regex

data Token
    = Type
    | Variable
    | Decimal
    | Octal
    | Hexadecimal
    deriving stock (Show, Eq, Ord, Lift)

instance Semigroup Token where (<>) = min

sng :: c -> Regex c
sng c = Range c c
