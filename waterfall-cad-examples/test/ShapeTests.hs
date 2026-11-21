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
import Linear (V3 (..), V2 (..), distance, unit, zero, normalize, identity, outer, (!-!), _x, _z, (^*))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Assertions

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
    , testGroup "transforms"
        [ testCase "scale2D by zero" (
            let s = TwoD.Transforms.scale2D zero Shape.unitSquare
            in (assertEqual "area is zero" 0.0 . Shape.area $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "scale2D by vector with zero element" (
            let s = TwoD.Transforms.scale2D (V2 1 0) Shape.unitSquare
            in (assertEqual "area is zero" 0.0 . Shape.area $ s)
                <> (assertApproxEqual "area with union is correct" 1e-6 1.0 . Shape.area . (<> Shape.centeredSquare) $ s)
                <> (assertEqual "union has one part" 1 . length . Shape.looseShapes . (<> Shape.centeredSquare) $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "uScale2D by zero" (
            let s = TwoD.Transforms.uScale2D 0 Shape.unitSquare
            in (assertEqual "area is zero" 0.0 . Shape.area $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "matTransform2D by zero matrix" (
            let s = TwoD.Transforms.matTransform2D zero Shape.unitSquare
            in (assertEqual "area is zero" 0.0 . Shape.area $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "matTransform2D by translation with zero linear part" (
            let s = TwoD.Transforms.matTransform2D (V2 (V3 0 0 1) (V3 0 0 2)) Shape.unitSquare
            in (assertEqual "area is zero" 0.0 . Shape.area $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "matTransform2D by singular diagonal matrix" (
            let s = TwoD.Transforms.matTransform2D (V2 (V3 1 0 0) (V3 0 0 0)) Shape.unitSquare
            in (assertEqual "area is zero" 0.0 . Shape.area $ s)
                <> (assertApproxEqual "area with union is correct" 1e-6 1.0 . Shape.area . (<> Shape.centeredSquare) $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "matTransform2D by singular matrix that is not axis aligned" (
            let s = TwoD.Transforms.matTransform2D (V2 (V3 1 1 0) (V3 1 1 0)) Shape.unitSquare
            in (assertEqual "area is zero" 0.0 . Shape.area $ s)
                <> (assertApproxEqual "area with union is correct" 1e-6 1.0 . Shape.area . (<> Shape.centeredSquare) $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "matTransform2D by projection onto a line" (
            let n = normalize (V2 1 2)
                projection = identity !-! outer n n
                s = TwoD.Transforms.matTransform2D (fmap (\(V2 a b) -> V3 a b 0) projection) Shape.unitSquare
            in (assertApproxEqual "area with union is correct" 1e-6 1.0 . Shape.area . (<> Shape.centeredSquare) $ s)
                <> (assertEqual "has no parts" 0 . length . Shape.looseShapes $ s)
        )
        , testCase "matTransform2D by shear preserves area" .
            assertApproxEqual "area is correct" 1e-6 1.0 . Shape.area
                . TwoD.Transforms.matTransform2D (V2 (V3 1 1 0) (V3 0 1 0)) $ Shape.unitSquare
        , testCase "matTransform2D by small invertible matrix is not empty" (
            let s = TwoD.Transforms.matTransform2D (V2 (V3 0.001 0 0) (V3 0 0.002 0)) Shape.unitSquare
            in (assertApproxEqual "area is correct" 1e-10 2e-6 . Shape.area $ s)
                <> (assertEqual "has one part" 1 . length . Shape.looseShapes $ s)
        )
        ]
    ]
