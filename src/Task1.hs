{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Hide built-in bind definition
import Prelude hiding ((>>=))

import Data.Functor.Identity

-- * Join monad

-- | Monad based on 'join' operation
-- instead of usual bind operator '(>>=)'.
class Applicative m => JoinMonad m where
  join :: m (m a) -> m a

-- * Equivalent views

infixl 1 >>=
(>>=) :: JoinMonad m => m a -> (a -> m b) -> m b
(>>=) m f = join (fmap f m)

infixr 1 >=>
(>=>) :: JoinMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g = \a -> join (fmap g (f a))

-- * Instances

instance JoinMonad Identity where
  join :: Identity (Identity a) -> Identity a
  join (Identity (Identity x)) = Identity x

instance JoinMonad Maybe where
  join :: Maybe (Maybe a) -> Maybe a
  join (Just (Just x)) = Just x
  join (Just Nothing)  = Nothing
  join Nothing         = Nothing

instance JoinMonad [] where
  join :: [[a]] -> [a]
  join = concat

instance (Monoid e) => JoinMonad ((,) e) where
  join :: Monoid e => (e, (e, a)) -> (e, a)
  join (e1, (e2, a)) = (e1 <> e2, a)

instance JoinMonad ((->) e) where
  join :: (e -> (e -> a)) -> (e -> a)
  join f e = f e e
