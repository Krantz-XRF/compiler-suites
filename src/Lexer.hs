{-# LANGUAGE FlexibleContexts #-}
module Lexer
    ( module Lexer.Regex
    , module Lexer.Dfa
    , mergeEq
    , mergeSemigroup
    , buildDfaWith
    , buildLexerFromDfa
    , buildLexerWith
    ) where

import Lexer.Regex
import Lexer.Nfa.Builder
import Lexer.Nfa.Determination
import Lexer.Dfa
import Lexer.Dfa.Input
import Lexer.Dfa.Minimize

import Control.Monad (mapM)
import Control.Monad.Except
import Control.Monad.State

import Data.Array.Unboxed as Arr
import Data.Map.Strict as Map

-- | 从正则表达式构建构建一个 Dfa
buildDfaWith :: (Arr.IArray Arr.UArray c, Ord c, Enum c, Bounded c)
             => (a -> a -> Maybe a) -> (a -> a -> a) -> [(a, Regex c)] -> Dfa c a
buildDfaWith merge select lst
    = minimizeWith merge
    $ determineWith select
    $ buildNfa
    $ mapM (mapM buildRegex) lst

newtype Lexer c s m a = Lexer { unwrapLexer :: ExceptT [(c, c)] (State s m) a }
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadError [(c, c)], MonadState s)

instance MonadTrans (Lexer c s) where
    lift = lift . lift

buildLexerFromDfa :: (Ord a, DfaInput s c, Monad m) => Dfa c a -> (a -> s -> m b) -> Lexer c s m b
buildLexerFromDfa m handler = do
    let resultHandlerMap = Map.fromList handlers
    input <- get
    case runDfa m input of
        Left err -> throwError err
        Right (t, tok, rest) -> put rest
    lift (handler t tok)

buildLexerWith :: ( Arr.IArray Arr.UArray c
                  , Ord c, Enum c, Bounded c
                  , DfaInput s c, Ord a, Monad m)
               => (a -> a -> Maybe a)
               -> (a -> a -> a)
               -> [(a, Regex c)]
               -> (a -> s -> m b)
               -> Lexer c s m b
buildLexerWith merge select = buildLexerFromDfa . buildDfaWith merge select
