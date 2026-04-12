{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Data.Functor.Identity

class Applicative m => KleisliMonad m where
  {-# MINIMAL (>=>) #-}
  infixr 1 >=>
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

infixl 1 >>=
(>>=) :: KleisliMonad m => m a -> (a -> m b) -> m b
(>>=) = error "TODO: define (>>=) in Task2"

join :: KleisliMonad m => m (m a) -> m a
join = error "TODO: define join in Task2"

instance KleisliMonad Identity where
  (>=>) = error "TODO: define (>=>) (KleisliMonad Identity)"

instance KleisliMonad Maybe where
  (>=>) = error "TODO: define (>=>) (KleisliMonad Maybe)"

instance KleisliMonad [] where
  (>=>) = error "TODO: define (>=>) (KleisliMonad [])"

instance (Monoid e) => KleisliMonad ((,) e) where
  (>=>) = error "TODO: define (>=>) (KleisliMonad ((,) e))"

instance KleisliMonad ((->) e) where
  (>=>) = error "TODO: define (>=>) (KleisliMonad ((->) e))"
