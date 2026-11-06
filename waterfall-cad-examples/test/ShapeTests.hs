module ShapeTests
( shapeTests
) where

import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import qualified Waterfall.Solids as Solids
import Linear (V2 (..), unit, _x, _z, (^*))
import Data.Function ((&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

shapeTests :: TestTree
shapeTests = testGroup "Shape"
    [ testGroup "looseShapes"
        [ testCase "mempty has no parts" $
            assertEqual "" 0 (length (Shape.looseShapes mempty))

        , testCase "emptyShape has no parts" $
            assertEqual "" 0 (length (Shape.looseShapes Shape.emptyShape))

        , testCase "a difference that removes everything has no parts" $
            assertEqual "" 0
                (length (Shape.looseShapes
                    (Shape.centeredSquare `Booleans.difference` TwoD.Transforms.scale2D (V2 3 3) Shape.centeredSquare)))

        , testCase "a single shape is one part" $
            assertEqual "" 1 (length (Shape.looseShapes Shape.unitCircle))

        , testCase "overlapping shapes fuse into one part" $
            assertEqual "" 1
                (length (Shape.looseShapes
                    (Shape.centeredSquare <> TwoD.Transforms.translate2D (unit _x ^* 0.5) Shape.centeredSquare)))

        , testCase "separate shapes are separate parts" $
            assertEqual "" 2
                (length (Shape.looseShapes
                    (Shape.centeredSquare <> TwoD.Transforms.translate2D (unit _x ^* 3) Shape.centeredSquare)))

        , testCase "parts preserve area" $
            let parts = Shape.looseShapes
                    (Shape.centeredSquare <> TwoD.Transforms.translate2D (unit _x ^* 3) Shape.centeredSquare)
                area = Solids.volume . Solids.prism 1
            in assertBool "each part has unit area" (all ((< 1e-6) . abs . subtract 1 . area) parts)
        ]
    , testGroup "section"
        [ testCase "cross section of sphere on plane has one part" $
            let parts = Shape.looseShapes (Shape.section Solids.unitSphere)
            in assertEqual "" 1 (length parts) 
            
        , testCase "cross section of sphere off plane has no parts" $
            let parts =  Solids.unitSphere
                    & Transforms.translate (unit _z ^* 2)
                    & Shape.section
                    & Shape.looseShapes
            in assertEqual "" 0 (length parts) 
        ]
    ]
