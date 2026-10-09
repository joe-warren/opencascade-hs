module Main
( main
) where

import DiagramGoldenTests (diagramGoldenTests)
import ExceptionTests (exceptionTests)
import PaintTests (paintTests)
import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "tests"
    [ diagramGoldenTests
    , exceptionTests
    , paintTests
    ]