{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Data.Functor.Identity
import Control.Monad (join)

-- * Functor composition

-- | Represents composition of two functors.
newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose (fmap (fmap f) fga)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure (pure a))

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose fgab) (Compose fga) = Compose (liftA2 (<*>) fgab fga)

-- * Monad composition

instance (Monad m, Monad n, Distrib n m) => Monad (Compose m n) where
  (>>=) :: forall a b. Compose m n a -> (a -> Compose m n b) -> Compose m n b
  (>>=) (Compose mna) k = Compose $
    mna >>= \na ->
      let nmnb :: n (m (n b))
          nmnb = fmap (getCompose . k) na
          mnnb :: m (n (n b))
          mnnb = distrib nmnb
      in fmap join mnnb

-- * Distributive property

-- | Describes distributive property of two monads.
class (Monad m, Monad n) => Distrib m n where
  distrib :: m (n a) -> n (m a)

-- * Distributive instances

instance Monad n => Distrib Identity n where
  distrib :: Monad n => Identity (n a) -> n (Identity a)
  distrib (Identity na) = Identity <$> na

instance Monad n => Distrib Maybe n where
  distrib :: Maybe (n a) -> n (Maybe a)
  distrib Nothing  = pure Nothing
  distrib (Just na) = Just <$> na

instance Monad n => Distrib [] n where
  distrib :: [] (n a) -> n ([] a)
  distrib = foldr (liftA2 (:)) (pure [])

instance (Monad n, Monoid e) => Distrib ((,) e) n where
  distrib :: (e, n a) -> n (e, a)
  distrib (e, na) = (e,) <$> na

instance Monad n => Distrib n ((->) e) where
  distrib :: n (e -> a) -> (e -> n a)
  distrib nea e = (\f -> f e) <$> nea
