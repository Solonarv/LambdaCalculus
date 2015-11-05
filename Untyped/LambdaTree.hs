module LambdaTree where

-- An untyped lambda expression AST
data LambdaTree a = a :\. LambdaTree a | LambdaTree a :\$ LambdaTree a | Var a

instance Eq a => UniRepr (LambdaTree a) where
    lift = liftTree

liftTree :: Eq a => LambdaTree a -> U
liftTree (v :\. t) = liftTree $ \x -> liftTree (subst v x t)
liftTree (x :\$ y) = liftTree x $$ liftTree y
liftTree (Var _) = undefined

subst :: Eq a => a -> LambdaTree a -> LambdaTree a -> LambdaTree a
subst var val (x :\. t) | var == x = x :\. t
                       | not (var `elem` freevars t)  = x :\. subst var val t
                       -- If neither of those are true, var must be alpha-renamed to something else
                       -- in t. This can't be accomplished in general, because there's no typeclass that can
                       -- always provide an element not in a given list. Thus, this case is INTENTIONALLY not handled.
subst var val (x :\$ y) = subst var val x :\$ subst var val y
subst var val (Var v)  | var == v = val
                       | var /= v = (Var v)

freevars :: Eq a => LambdaTree a -> [a]
freevars (v :\. t) = freevars t \\ [v]
freevars (x :\$ y) = freevars x `union` freevars y
freevars (Var v) = [v]