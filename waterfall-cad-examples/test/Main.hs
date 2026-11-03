module Main
( main
) where

import DiagramGoldenTests (diagramGoldenTests)
import ExceptionTests (exceptionTests)
import PaintTests (paintTests)
import LifetimeTests (lifetimeTests)
import SolidTests (solidTests)
import ShapeTests (shapeTests)
import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.Runners (NumThreads (..))


main :: IO ()
main = defaultMain . localOption (NumThreads 1) $ testGroup "tests"
    [ diagramGoldenTests
    , exceptionTests
    , paintTests
    , lifetimeTests
    , solidTests
    , shapeTests
    ]