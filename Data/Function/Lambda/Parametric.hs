{-# LANGUAGE
    DeriveFunctor
    #-}

module Data.Function.Lambda.Parametric where

newtype Lam f g = Lam { runLam :: f (Lam f g) -> g (Lam f g) }

newtype K a (b :: k) = K { unK :: a } deriving (Functor)

idL :: Lam f f
idL = Lam id