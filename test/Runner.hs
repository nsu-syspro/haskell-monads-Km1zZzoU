import Test.Tasty

import Task1Suite
import Task2Suite
import Task3Suite

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ task1Tests
  , task2Tests
  , task3Tests
  ]
