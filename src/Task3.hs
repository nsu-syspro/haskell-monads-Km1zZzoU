{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Data.Functor.Identity

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap = error "TODO: define fmap (Functor (Compose f g))"

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = error "TODO: define pure (Applicative (Compose f g))"

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) = error "TODO: define (<*>) (Applicative (Compose f g))"

instance (Monad m, Monad n, Distrib n m) => Monad (Compose m n) where
  (>>=) :: forall a b. Compose m n a -> (a -> Compose m n b) -> Compose m n b
  (>>=) = error "TODO: define (>>=) (Monad (Compose m n))"

-- For composition of two monads m and n we need them to be distributable using:
--   distrib :: m (n a) -> n (m a)

-- IO (Maybe Int) -- is IO computation that might fail
-- Maybe (IO Int) -- is potentially IO computation

class (Monad m, Monad n) => Distrib m n where
  distrib :: m (n a) -> n (m a)

instance Monad n => Distrib Identity n where
  distrib = error "TODO: define distrib (Distrib Identity n)"

instance Monad n => Distrib Maybe n where
  distrib :: Maybe (n a) -> n (Maybe a)
  distrib = error "TODO: define distrib (Distrib Maybe n)"

instance Monad n => Distrib [] n where
  distrib :: [] (n a) -> n ([] a)
  distrib = error "TODO: define distrib (Distrib [] n)"

instance (Monad n, Monoid e) => Distrib ((,) e) n where
  distrib :: (e, n a) -> n (e, a)
  distrib = error "TODO: define distrib (Distrib ((,) e) n)"

instance Monad n => Distrib n ((->) e) where
  distrib :: n (e -> a) -> (e -> n a)
  distrib = error "TODO: define distrib (Distrib n ((->) e))"
