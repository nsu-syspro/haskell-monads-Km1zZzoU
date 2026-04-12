{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

import Data.Functor.Identity

class Applicative m => JoinMonad m where
  {-# MINIMAL join #-}
  join :: m (m a) -> m a

infixl 1 >>=
(>>=) :: JoinMonad m => m a -> (a -> m b) -> m b
(>>=) = error "TODO: define (>>=) in Task1"

infixr 1 >=>
(>=>) :: JoinMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = error "TODO: define (>=>) in Task1"

instance JoinMonad Identity where
  join = error "TODO: define join (JoinMonad Identity)"

instance JoinMonad Maybe where
  join = error "TODO: define join (JoinMonad Maybe)"

instance JoinMonad [] where
  join = error "TODO: define join (JoinMonad [])"

instance (Monoid e) => JoinMonad ((,) e) where
  join = error "TODO: define join (JoinMonad ((,) e))"

instance JoinMonad ((->) e) where
  join = error "TODO: define join (JoinMonad ((->) e))"
