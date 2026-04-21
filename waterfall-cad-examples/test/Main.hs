module Main
( main
) where

import DiagramGoldenTests (diagramGoldenTests)
import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "tests"
    [ diagramGoldenTests
    ]