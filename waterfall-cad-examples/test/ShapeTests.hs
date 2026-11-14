{-# LANGUAGE ScopedTypeVariables #-}
module ShapeTests
( shapeTests
) where

import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import qualified Waterfall.Path.Common as Path
import qualified Waterfall.TwoD.Path2D as TwoD.Path2D
import qualified Waterfall.Solids as Solids
import Waterfall.Internal.NearZero (nearZero)
import Linear (V3 (..), V2 (..), distance, unit, _x, _z, (^*))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
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
            in assertBool "each part has unit area" (all ((< 1e-6) . abs . subtract 1 . Shape.area) parts)
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
    , testGroup "silhouette" 
        [ testCase "emptySolid has no parts" $
            let parts = Shape.looseShapes (Shape.silhouette Solids.emptySolid)
            in assertEqual "" 0 (length parts) 

        , testCase "silhouette of sphere" $
            let s = Shape.silhouette Solids.unitSphere
            in assertEqual "sphere has one part" 1 (length . Shape.looseShapes $ s)
                <> assertBool "silhouette area is pi" ((< 1e-6) . abs . subtract pi . Shape.area $ s)

        , testCase "silhouette of cube" $
            let s = Shape.silhouette Solids.centeredCube 
            in assertEqual "cube has one part" 1 (length . Shape.looseShapes $ s)
                <> assertBool "silhouette area is 1" ((< 1e-6) . abs . subtract 1 . Shape.area $ s)

        , testCase "silhouette of cube (rotated)" $
            let s = Solids.centeredCube 
                    & Transforms.rotate (unit _x) (pi/4)
                    & Shape.silhouette
            in assertEqual "cube has one part" 1 (length . Shape.looseShapes $ s)
                <> assertBool "silhouette area is sqrt(2)" ((< 1e-6) . abs . subtract (sqrt 2) . Shape.area $ s)

        , testCase "silhouette of cylinder" $
            let s = Solids.centeredCylinder 
                    & Shape.silhouette
            in assertEqual "cylinder has one part" 1 (length . Shape.looseShapes $ s)
                <> assertBool "silhouette area is pi" ((< 1e-6) . abs . subtract pi . Shape.area $ s)

        , testCase "silhouette of cylinder (rotated)" $
            let s = Solids.centeredCylinder 
                    & Transforms.rotate (unit _x) (pi/2)
                    & Shape.silhouette
            in assertEqual "cube has one part" 1 (length . Shape.looseShapes $ s)
                <> assertBool "silhouette area is 2" ((< 1e-6) . abs . subtract 2 . Shape.area $ s)

        , testCase "silhouette of torus" $
            let s = Solids.torus 2 1
                    & Shape.silhouette
            in assertEqual "torus has one part" 1 (length . Shape.looseShapes $ s)
                <> assertBool "silhouette area is 8*pi" ((< 1e-4) . abs . subtract (8*pi) . Shape.area $ s)

        , testCase "silhouette of cube with hole" $
            let s = Solids.centeredCube
                    & (`Booleans.difference` (Solids.centeredCylinder & Transforms.scale (V3 0.1 0.1 2)))
                    & Shape.silhouette
            in assertEqual "cube has one part" 1 (length . Shape.looseShapes $ s)
                <> assertBool "silhouette area is 1-pi/100" ((< 1e-5) . abs . subtract (1-pi/100) . Shape.area $ s)
        ]
    , testGroup "shapePaths" 
        [ testCase "mirrored square" $ 
            let shape = Shape.unitSquare
                    & TwoD.Transforms.mirror2D (unit _x)
                paths = Shape.shapePaths shape
                rebuilt = Shape.makeShape <$> paths
                reversePathIsConsistent (p :: TwoD.Path2D.Path2D) = 
                    let getEndpointsOrDie = fromMaybe (error "failed to get path endpoints") . Path.pathEndpoints 
                        (s, e) = getEndpointsOrDie p
                        (e', s') = getEndpointsOrDie . Path.reversePath $ p
                    in all nearZero (s' - s) && all nearZero (e' - e) 
                    
            in assertBool "rebuilt shape has area 1" ((< 1e-5) . abs . subtract (1) . sum . fmap Shape.area $ rebuilt)
                <> assertBool "reversePath is consistent" (all reversePathIsConsistent (paths <> (Path.splitPath =<< paths)))
        , testCase "scaled silhouette" $
              let silhouette = Solids.unitSphere
                      & (`Booleans.difference` (Solids.unitSphere & Transforms.uScale 0.4))
                      & (`Booleans.difference` (Solids.centeredCube & Transforms.translate (unit _x)))
                      & Booleans.union (Solids.centeredCube
                          & Transforms.uScale 0.8
                          & Transforms.translate (unit _x)
                          )
                      & Shape.silhouette
                  scaled = TwoD.Transforms.uScale2D 50 silhouette
                  totalLength = sum . fmap Path.pathLength . Shape.shapePaths
                  isClosed (p :: TwoD.Path2D.Path2D) =
                      maybe False (\(s, e) -> distance s e < 1e-4) (Path.pathEndpoints p)
                  rebuiltArea = sum . fmap (Shape.area . Shape.makeShape) . Shape.shapePaths $ scaled
              in assertBool "paths are closed" (all isClosed (Shape.shapePaths scaled))
                  <> assertBool "length scales with the shape"
                      ((< 1e-3) . abs $ totalLength scaled - 50 * totalLength silhouette)
                  <> assertBool "rebuilt shape keeps its area"
                      ((< 1e-2) . abs $ rebuiltArea - Shape.area scaled)   
        ]
    ]
