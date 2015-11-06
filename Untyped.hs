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

($$) :: Uni c => (forall a b. (Uni a, Uni b) => (a -> b -> c))
x $$ y = let x' = lift x
             y' = lift y
         in unlift $ (x' $$! y')

liftL1 :: (U -> U) -> U
liftL1 = U

liftL2 :: (U -> U -> U) -> U
liftL2 f = U $ liftL1 . f

liftL3 :: (U -> U -> U -> U) -> U
liftL3 f = U $ liftL2 . f

liftL4 :: (U -> U -> U -> U -> U) -> U
liftL4 f = U $ liftL3 . f