{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Task1Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Prelude hiding ((>>=))
import Data.Functor.Identity

import Task1

task1Tests :: TestTree
task1Tests = testGroup "Task1"
  [ testGroup "Bind (>>=)"
      [ bindLaws @Identity     "Identity"  1000 (milli 100)
      , bindLaws @Maybe        "Maybe"     1000 (milli 100)
      , bindLaws @[]           "List"      100  (milli 1000)
      , bindLaws @((,) String) "(String,)" 1000 (milli 100)
      , bindLawsReader                     1000 (milli 100)
      ]

  , testGroup "Kleisli composition (>=>)"
      [ compositionLaws @Identity     "Identity"  1000 (milli 100)
      , compositionLaws @Maybe        "Maybe"     1000 (milli 100)
      , compositionLaws @[]           "List"      100  (milli 1000)
      , compositionLaws @((,) String) "(String,)" 1000 (milli 100)
      ]
  ]

bindLaws :: forall m.
            ( JoinMonad m, Arbitrary (m Int), Arbitrary (m Char)
            , Eq (m Int), Eq (m Char), Show (m Char), Show (m Int))
           => String -> Int -> Int -> TestTree
bindLaws name n limit = testGroup ("bind laws for " <> name)
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

bindLawsReader :: Int -> Int -> TestTree
bindLawsReader n limit = testGroup "bind laws for (Int ->)"
  [ testProperty "left identity: (pure a >>= k) i = (k a) i" $
      withMaxSuccess n $ counterexample "(a, k, i)" $ within limit $
        \(a, Fn2 (k :: Int -> (Int -> Char)), i) ->
          (pure a >>= k) i === (k a) i
  , testProperty "right identity: (m >>= pure) i = m i" $
      withMaxSuccess n $ counterexample "(m, i)" $ within limit $
        \(Fun _ (m :: Int -> Int), i) ->
          (m >>= pure) i === m i
  , testProperty "associativity: (m >>= (\\x -> k x >>= h)) i = ((m >>= k) >>= h) i" $
      withMaxSuccess n $ counterexample "(m, k, h, i)" $ within limit $
        \(Fun _ (m :: Int -> Int), Fn2 (k :: Int -> (Int -> Char)), Fn2 (h :: Char -> (Int -> Int)), i) ->
          (m >>= (\x -> k x >>= h)) i === ((m >>= k) >>= h) i
  ]

compositionLaws :: forall m.
            ( JoinMonad m, Arbitrary (m Int), Arbitrary (m Char)
            , Eq (m Int), Eq (m Char), Show (m Char), Show (m Int))
           => String -> Int -> Int -> TestTree
compositionLaws name n limit = testGroup ("composition laws for " <> name)
  [ testProperty "left identity: (pure >=> g) i = g i" $
      withMaxSuccess n $ counterexample "(g, i)" $ within limit $
        \(Fun _ (g :: Int -> m Char), i) ->
          (pure >=> g) i === g i
  , testProperty "right identity: (g >=> pure) i = g i" $
      withMaxSuccess n $ counterexample "(g, i)" $ within limit $
        \(Fun _ (g :: Int -> m Char), i) ->
          (g >=> pure) i === g i
  , testProperty "associativity: ((g >=> h) >=> k) i = (g >=> (h >=> k)) i" $
      withMaxSuccess n $ counterexample "(g, h, k, i)" $ within limit $
        \(Fun _ (g :: Char -> m Int), Fun _ (h :: Int -> m Char), Fun _ (k :: Char -> m Int), i) ->
          ((g >=> h) >=> k) i === (g >=> (h >=> k)) i
  ]

milli :: Int -> Int
milli n = n * 10 ^ (3 :: Int)
