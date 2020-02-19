{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Lexer.Regex.Applicative
  ( RE(.., Sng)
  , manyFromTo
  , manyFrom
  ) where

import Control.Applicative

-- |Regular expressions.
-- After matching the text @[s]@, produces some result @a@.
data RE s a where
    Eps :: a -> RE s a
    Box :: b -> RE s a -> RE s b
    Ran :: (s -> a) -> s -> s -> RE s a
    Many :: (b -> c) -> b -> (b -> a -> b) -> RE s a -> RE s c
    Alt :: RE s a -> RE s a -> RE s a
    App :: RE s (a -> b) -> RE s a -> RE s b
    Fail :: RE s a

-- |Regular expression to match a single character.
pattern Sng :: (Enum s, Eq s) => (s -> a) -> s -> RE s a
pattern Sng f c <- Ran f c ((succ c ==) -> True)
  where Sng f c = Ran f c (succ c)

parens :: ShowS -> ShowS
parens s = showChar '(' . s . showChar ')'

-- |Show (RE s a) as regular expression form. Precedence:
-- * Fail, Eps, Ran > Many > App (concat) > Alt
-- * Box: transparent
instance Show s => Show (RE s a) where
    showsPrec 1 (Eps _) = id
    showsPrec _ (Eps _) = showString "<EPSILON>"
    showsPrec _ (Ran _ l r) = showChar '[' . shows l . showChar '-' . shows r . showChar ']'
    showsPrec 0 (Alt x y) = shows x . showChar '|' . shows y
    showsPrec n (App x y) | n <= 1 = showsPrec 1 x . showsPrec 1 y
    showsPrec _ (Many _ _ _ x) = showsPrec 2 x . showChar '*'
    showsPrec n (Box _ x) = showsPrec n x
    showsPrec _ Fail = showString "<FAIL>"
    showsPrec _ r = parens (shows r)

{-| Functor Laws:
(1) Identity
    > fmap id = id
(2) Composition
    > fmap f . fmap g = fmap (f . g)

* Trivial for 'Eps', 'Box', 'Ran', 'Many' and 'Fail'
* Recursive for 'Alt' and 'App'
-}
instance Functor (RE s) where
    fmap f (Eps x) = Eps (f x)
    fmap f (Box x r) = Box (f x) r
    fmap f (Ran g l r) = Ran (f . g) l r
    fmap f (Many g z0 k r) = Many (f . g) z0 k r
    fmap f (Alt r1 r2) = Alt (fmap f r1) (fmap f r2)
    fmap f (App rf fa) = App (fmap (f .) rf) fa
    fmap _ Fail = Fail

    x <$ (Eps _) = Eps x
    x <$ (Box _ r) = Box x r
    _ <$ Fail = Fail
    x <$ r = Box x r

{-| Applicative Laws:
(1) Identity
    > pure id <*> v = v
(2) Homomorphism
    > pure f <*> pure x = pure (f x)
(3) Interchange
    > u <*> pure y = pure ($ y) <*> u
(4) Composition
    > pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

* (1-3) is trivial
* For (4):
    * Suppose @u@, @v@, @w@ has no 'App'.
        @
        LHS = pure (.) <*> u <*> v <*> w
            = (.) <$> u <*> v <*> w
            = App (App ((.) <$> u) v) w
        RHS = u <*> (v <*> w)
            = u <*> App v w
            = App (App ((.) <$> u) v) w
        @
    * Or else recursive
-}
instance Applicative (RE s) where
    pure = Eps
    Eps f <*> rx = fmap f rx
    ru <*> Eps y = fmap ($ y) ru
    rf <*> App rg rx = App ((.) <$> rf <*> rg) rx
    rf <*> rx = App rf rx

instance Alternative (RE s) where
    empty = Fail

    Fail <|> x = x
    x <|> Fail = x
    x <|> y = Alt x y

    many (Eps _) = Eps []
    many r = Many reverse [] (flip (:)) r
    some x = liftA2 (:) x (many x)

-- |Repeat a regular expression.
-- * If upper bound @Just u@, repeat [l, u) times.
-- * If upper bound @Nothing@, repeat [l, +inf) times.
manyFromTo :: Int -> Maybe Int -> RE s a -> RE s [a]
manyFromTo l mu re
  = foldr (<*>) (moreThan d)
  $ replicate l (fmap (:) re)
  where d = fmap (subtract l) mu
        moreThan Nothing = many re
        moreThan (Just n)
          = foldl (<|>) Fail
          $ take n
          $ iterate (fmap (:) re <*>) (Eps [])

-- |Repeat a regular expression.
-- Equivalant to 'manyFrom' with upper bound 'Nothing'.
manyFrom :: Int -> RE s a -> RE s [a]
manyFrom l = manyFromTo l Nothing
