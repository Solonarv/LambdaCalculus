-- How close can we get to literal untyped lambda calculus?

{-# LANGUAGE
    ExistentialQuantification,
    RankNTypes,
    UndecidableInstances,
    OverlappingInstances #-}

module Untyped where

import Data.List

-- The one type in ULC, implemented in a somewhat hacky way
-- because infinite types are tricky
newtype U = U { u :: U -> U }

class Uni a where
    lift :: a -> U
    unlift :: U -> a

instance {-# OVERLAPPABLE #-} Uni U where
    lift = id
    unlift = id

instance (Uni a, Uni b) => Uni (a -> b) where
    unlift x = unlift . u x . lift
    lift f = U (lift . f . unlift)


infixl 0 $$!, $$
($$!) :: U -> U -> U
($$!) = u

($$) :: (Uni a, Uni b) => (forall c. Uni c => (a -> b -> c))
x $$ y = let x' = lift x
             y' = lift y
         in unlift $ (x' $$! y')