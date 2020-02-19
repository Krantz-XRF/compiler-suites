{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Lexer.Regex.Applicative
import StructuralEq

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "structuralEq" $
    it "structuralEq x x" $ property $ \re ->
      structuralEq @Char @Int re re
  describe "Functor (RE s)" $ do
    it "fmap id = id" $ property $ \re ->
      structuralEq @Char @Int (fmap id re) re
    it "fmap f . fmap g = fmap (f . g)" $
      property $ \(re :: RE Char Int) -> do
        f <- arbitrary @(Int -> Int)
        g <- arbitrary @(Int -> Int)
        return $ structuralEq
          (fmap f . fmap g $ re)
          (fmap (f . g) re)
  describe "Applicative (RE s)" $ do
    it "pure id <*> v = v" $ property $ \re ->
      structuralEq @Char @Int (pure id <*> re) re
    it "pure f <*> pure x = pure (f x)" $
      property $ \(x :: Int) -> do
        f <- arbitrary @(Int -> Int)
        return $ structuralEq @Char
          (pure f <*> pure x)
          (pure (f x))
    it "u <*> pure y = pure ($ y) <*> u" $
      property $ \(u :: RE Char (Int -> Int)) y ->
                   structuralEq
                   (u <*> pure y)
                   (pure ($ y) <*> u)
    it "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $
      property $ \(u :: RE Char (Int -> Int))
                  (v :: RE Char (Int -> Int))
                  (w :: RE Char Int) ->
                   structuralEq
                   (pure (.) <*> u <*> v <*> w)
                   (u <*> (v <*> w))

instance (CoArbitrary s, Arbitrary s, Arbitrary a) =>
         Arbitrary (RE s a) where
  arbitrary = sized arbN where
    conds n =
      let arb :: Arbitrary b => Gen b
          arb = resize (n-1) arbitrary
      in [ (1, pure Fail)
         , (1, Eps <$> arb)
         , (9, Ran <$> arb <*> arb <*> arb)
         , (2, Box <$> arb <*> arb @(RE s s))
         , (2, Many <$> arb <*> pure []
               <*> pure (flip (:))
               <*> arb @(RE s s))
         , (4, Alt <$> arb <*> arb)
         , (7, App <$> arb <*> arb @(RE s s))
         ]
    arbN 0 = frequency $ take 2 $ conds 0
    arbN n = frequency $ conds n
