{-# LANGUAGE ScopedTypeVariables #-}
module PathTests
( pathTests
) where

import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import qualified Waterfall.Path as Path
import qualified Waterfall.TwoD.Path2D as TwoD.Path2D
import qualified Waterfall.Solids as Solids
import Waterfall.Internal.NearZero (nearZero)
import Linear (V3 (..), V2 (..), distance, unit, zero, _x, _z, (^*))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Assertions


pathTests :: TestTree
pathTests = testGroup "Path" 
    [ testGroup "append" 
        [ testCase "append lines" $ 
            let path =  (Path.line zero (V3 1 0 0) <> Path.line (V3 1 0 0) (V3 2 0 0)) :: Path.Path
            in assertApproxEqual "should have the correct Length" 1e-6 2 (Path.pathLength path)
        , testCase "append lines small gap" $ 
            let path gap =  (Path.line zero (V3 1 0 0) <> Path.line (V3 (1 + gap) 0 0) (V3 2 0 0)) :: Path.Path
                test gap = assertApproxEqual ("should have the correct length (" <> show gap <> ")") 1e-5 2 (Path.pathLength . path $ gap)
            in foldMap test [5e-8, 5e-7, 2e-6]
        ]
    ]