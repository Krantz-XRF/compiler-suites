{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
module Lexer.Regex
    ( Regex(.., Many)
    , buildRegex
    ) where

import Lexer.Nfa.Builder

-- | 正则表达式
data Regex c
    = Range c c
    | Or (Regex c) (Regex c)
    | Concat (Regex c) (Regex c)
    | Some (Regex c)
    | Optional (Regex c)
    deriving stock (Show)

pattern Many :: Regex c -> Regex c
pattern Many r = Optional (Some r)

-- | 从正则表达式构建 NFA
buildRegex :: MonadNfaBuilder c m => Regex c -> m (TempNfa c)
buildRegex (Range l r) = newNfa $ \s t -> transition l r s t
buildRegex (Or r1 r2) = newNfa $ \s t -> do
    TempNfa s1 t1 <- buildRegex r1
    TempNfa s2 t2 <- buildRegex r2
    epsilonTrans s s1
    epsilonTrans s s2
    epsilonTrans t1 t
    epsilonTrans t2 t
buildRegex (Concat r1 r2) = do
    TempNfa s1 t1 <- buildRegex r1
    TempNfa s2 t2 <- buildRegex r2
    epsilonTrans t1 s2
    return (TempNfa s1 t2)
buildRegex (Some r) = do
    res@(TempNfa s1 t1) <- buildRegex r
    epsilonTrans t1 s1
    return res
buildRegex (Optional r) = newNfa $ \s t -> do
    TempNfa s1 t1 <- buildRegex r
    epsilonTrans s t
    epsilonTrans s s1
    epsilonTrans t1 t
