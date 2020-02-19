{-# LANGUAGE GADTs #-}
module StructuralEq where

import Lexer.Regex.Applicative

structuralEq :: Eq s => RE s a -> RE s b -> Bool
structuralEq Fail Fail = True
structuralEq (Eps _) (Eps _) = True
structuralEq (Box _ r) (Box _ r') = structuralEq r r'
structuralEq (Ran _ l r) (Ran _ l' r') = l == l' && r == r'
structuralEq (Many _ _ _ r) (Many _ _ _ r') = structuralEq r r'
structuralEq (Alt r1 r2) (Alt r1' r2') = structuralEq r1 r1' && structuralEq r2 r2'
structuralEq (App rf rx) (App rf' rx') = structuralEq rf rf' && structuralEq rx rx'
structuralEq _ _ = False
