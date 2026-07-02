{-# LANGUAGE LambdaCase #-}

import Control.Monad (void, (<=<))
import Data.Either (isRight)
import Paths_waterfall_cad_svg (getDataFileName)
import Test.Tasty
import Test.Tasty.HUnit
import Waterfall.SVG

readTestFile :: FilePath -> IO (Either SVGError ())
readTestFile = fmap void . readSVG <=< getDataFileName

shouldSatisfy :: Show a => a -> (a -> Bool) -> Assertion
shouldSatisfy x p = assertBool ("predicate failed for " <> show x) $ p x

main :: IO ()
main = defaultMain $
  testGroup "readSVG" $
    [ testCase "reads a good file successfully" $ do
        result <- readTestFile "test-data/fixture1.svg"
        result `shouldSatisfy` isRight
    , testCase "returns SVGTreeError from a bad file" $ do
        result <- readTestFile "test-data/fixture2.svg"
        result `shouldSatisfy` \case
          Left (SVGError SVGTreeError _) -> True
          _ -> False
    , testCase "returns SVGIOError from a non-existent file" $ do
        result <- readTestFile "does-not-exist.svg"
        result `shouldSatisfy` \case
          Left (SVGError SVGIOError _) -> True
          _ -> False
    ]
