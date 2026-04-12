{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Task3Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Functor.Identity

import Task3
import Data.Coerce (coerce)

task3Tests :: TestTree
task3Tests = testGroup "Task3"
  [ testGroup "Functor"
      [ functorLaws @(Compose Identity Maybe)        "Compose Identity Maybe"     1000 (milli 100)
      , functorLaws @(Compose Maybe Identity)        "Compose Maybe Identity"     1000 (milli 100)
      , functorLaws @(Compose Maybe [])              "Compose Maybe []"           1000 (milli 100)
      , functorLaws @(Compose [] Identity)           "Compose [] Identity"        1000 (milli 100)
      , functorLaws @(Compose [] Maybe)              "Compose [] Maybe"           1000 (milli 100)
      , functorLaws @(Compose ((,) String) Identity) "Compose (String,) Identity" 1000 (milli 100)
      , functorLaws @(Compose ((,) String) Maybe)    "Compose (String,) Maybe"    1000 (milli 100)
      ]
  , testGroup "Applicative"
      [ applicativeLaws @(Compose Identity Maybe)        "Compose Identity Maybe"     1000 (milli 100)
      , applicativeLaws @(Compose Maybe Identity)        "Compose Maybe Identity"     1000 (milli 100)
      , applicativeLaws @(Compose Maybe [])              "Compose Maybe []"           1000 (milli 100)
      , applicativeLaws @(Compose [] Identity)           "Compose [] Identity"        1000 (milli 100)
      , applicativeLaws @(Compose [] Maybe)              "Compose [] Maybe"           1000 (milli 100)
      , applicativeLaws @(Compose ((,) String) Identity) "Compose (String,) Identity" 1000 (milli 100)
      , applicativeLaws @(Compose ((,) String) Maybe)    "Compose (String,) Maybe"    1000 (milli 100)
      ]
  , testGroup "Monad"
      [ monadLaws @(Compose Identity Maybe)        "Compose Identity Maybe"     1000 (milli 100)
      , monadLaws @(Compose Maybe Identity)        "Compose Maybe Identity"     1000 (milli 100)
      , monadLaws @(Compose Maybe [])              "Compose Maybe []"           100  (milli 1000)
      , monadLaws @(Compose [] Identity)           "Compose [] Identity"        100  (milli 1000)
      , monadLaws @(Compose [] Maybe)              "Compose [] Maybe"           100  (milli 1000)
      , monadLaws @(Compose ((,) String) Identity) "Compose (String,) Identity" 1000 (milli 100)
      , monadLaws @(Compose ((,) String) Maybe)    "Compose (String,) Maybe"    1000 (milli 100)
      ]
  , testGroup "Reader ((->) Int)"
      [ distribReader 1000 (milli 100)
      ]
  ]

functorLaws :: forall m.
            ( Functor m, Arbitrary (m Int), Eq (m Int), Show (m Int))
           => String -> Int -> Int -> TestTree
functorLaws name n limit = testGroup ("functor laws for " <> name)
  [ testProperty "identity: fmap id f = id f" $
      withMaxSuccess n $ counterexample "(f)" $ within limit $
        \(f :: m Int) ->
          (fmap id f) === id f
  ]

applicativeLaws :: forall m.
            ( Applicative m, Arbitrary (m Int), Eq (m Int), Show (m Int)
            , Show (m String), Eq (m String))
           => String -> Int -> Int -> TestTree
applicativeLaws name n limit = testGroup ("applicative laws for " <> name)
  [ testProperty "identity: pure id <*> v = v" $
      withMaxSuccess n $ counterexample "(v)" $ within limit $
        \(v :: m Int) ->
          (pure id <*> v) === v
  , testProperty "homomorphism: pure f <*> pure x = pure (f x)" $
      withMaxSuccess n $ counterexample "(f, x)" $ within limit $
        \(Fun _ (f :: Int -> String), x) ->
          (pure f <*> pure x) === pure @m (f x)
  , testProperty "functor: fmap f x = pure f <*> x" $
      withMaxSuccess n $ counterexample "(f, x)" $ within limit $
        \(Fun _ (f :: Int -> String), x :: m Int) ->
          (fmap f x) === (pure f <*> x)

  -- TODO: interchange, composition
  ]

monadLaws :: forall m.
            ( Monad m, Arbitrary (m Int), Arbitrary (m Char)
            , Eq (m Int), Eq (m Char), Show (m Char), Show (m Int))
           => String -> Int -> Int -> TestTree
monadLaws name n limit = testGroup ("monad laws for " <> name)
  [ testProperty "left identity: pure a >>= k = k a" $
      withMaxSuccess n $ counterexample "(a, k)" $ within limit $
        \(a, Fun _ (k :: Int -> m Char)) ->
          (pure a >>= k) === k a
  , testProperty "right identity: m >>= pure = m" $
      withMaxSuccess n $ counterexample "(m)" $ within limit $
        \(m :: m Int) ->
          (m >>= pure) === m
  , testProperty "associativity: m >>= (\\x -> k x >>= h) = (m >>= k) >>= h" $
      withMaxSuccess n $ counterexample "(m, k, h)" $ within limit $
        \(m :: m Int, Fun _ (k :: Int -> m Char), Fun _ (h :: Char -> m Int)) ->
          (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)
  ]

distribReader :: Int -> Int -> TestTree
distribReader n limit = testGroup "distrib check for (Int ->)"
  [ testProperty "runIdentityWrapper (distrib (IdentityWrapper f) i) = f i" $
      withMaxSuccess n $ counterexample "(f, i)" $ within limit $
        \(Fun _ (f :: Int -> String), i) ->
          runIdentityWrapper (distrib (coerce f) i) === Identity (f i)
  ]


milli :: Int -> Int
milli n = n * 10 ^ (3 :: Int)

instance (Arbitrary (m (n a))) => Arbitrary (Compose m n a) where
  arbitrary = Compose <$> arbitrary
  shrink = shrinkMap Compose getCompose

newtype IdentityWrapper a = IdentityWrapper { runIdentityWrapper :: Identity a }
  deriving (Show, Eq, Functor, Applicative, Monad, Arbitrary)
