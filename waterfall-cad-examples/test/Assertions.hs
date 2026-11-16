module Assertions 
( assertApproxEqual
) where
import GHC.Stack (HasCallStack)
import Control.Monad (unless)
import Test.Tasty.HUnit (Assertion, assertFailure)

--assertApproxEqual :: HasCallStack => String -> Double -> Double -> Double -> Assertion
assertApproxEqual :: (Ord a, Num a, Show a) => String -> a -> a -> a -> Assertion
assertApproxEqual preface tolerance expected actual =
      unless (abs (expected - actual) <= tolerance) (assertFailure msg)
    where
      msg = (if null preface then "" else preface <> "\n")
          <> "expected: " <> show expected
          <> "\n but got: " <> show actual
          <> "\n (difference " <> show (abs (expected - actual))
          <> ", tolerance " <> show tolerance <> ")"