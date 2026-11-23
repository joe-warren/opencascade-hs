module SolidTests
( solidTests
) where

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Revolution as Revolution
import qualified Waterfall.TwoD.Shape as TwoD.Shape
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import qualified Waterfall.Offset as Offset
import Linear (V3 (..), V4 (..), unit, zero, _x, _z, (^*), normalize, identity, outer, (!-!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import qualified Waterfall.Fillet as Fillet
import Assertions

solidTests :: TestTree
solidTests = testGroup "Solid"
    [ testGroup "looseParts"
        [ testCase "mempty has no parts" $
            assertEqual "" 0 (length (Solids.looseParts mempty))

        , testCase "emptySolid has no parts" $
            assertEqual "" 0 (length (Solids.looseParts Solids.emptySolid))

        , testCase "a difference that removes everything has no parts" $
            assertEqual "" 0
                (length (Solids.looseParts
                    (Solids.centeredCube `Booleans.difference` Transforms.scale (V3 3 3 3) Solids.centeredCube)))

        , testCase "a single solid is one part" $
            assertEqual "" 1 (length (Solids.looseParts Solids.unitSphere))

        , testCase "overlapping solids fuse into one part" $
            assertEqual "" 1
                (length (Solids.looseParts
                    (Solids.centeredCube <> Transforms.translate (unit _x ^* 0.5) Solids.centeredCube)))

        , testCase "separate solids are separate parts" $
            assertEqual "" 2
                (length (Solids.looseParts
                    (Solids.centeredCube <> Transforms.translate (unit _x ^* 3) Solids.centeredCube)))

        , testCase "parts preserve volume" $
            let parts = Solids.looseParts
                    (Solids.centeredCube <> Transforms.translate (unit _x ^* 3) Solids.centeredCube)
            in assertBool "each part has unit volume" (all ((< 1e-6) . abs . subtract 1 . Solids.volume) parts)
        ]
    , testGroup "revolution"
        [ testCase "revolution of emptyShape has zero volume" $
            let part = Revolution.revolution mempty 
            in assertBool "part has zero volume" (abs (Solids.volume part) < 1e-6)
        ]
    , testGroup "fillet"
        [ testCase "fillet of sphere is noop" $
            let part = Fillet.roundConditionalFillet (Fillet.whenEndpointsNearlyEqual _z 0.1) Solids.unitSphere
            in assertBool "part has correct volume" (abs (Solids.volume part - 4*pi/3) < 1e-4)
        , testCase "different fillet of sphere is noop" $
            let part = Fillet.roundFillet 0.1 Solids.unitSphere
            in assertBool "part has correct volume" (abs (Solids.volume part - 4*pi/3) < 1e-4)
        , testCase "fillet of capsule is noop" $
            let capsule = Solids.unitCylinder <> Solids.unitSphere <> Transforms.translate (unit _z) Solids.unitSphere
                part = Fillet.roundFillet 0.1 capsule
            in assertBool "part has correct volume" (abs (Solids.volume part - 7*pi/3) < 1e-4)
        ]
    , testGroup "volume"
        [ testCase "volume of sphere" $
            assertBool "sphere has correct volume" (abs (Solids.volume Solids.unitSphere - 4*pi/3) < 1e-4)
        , testCase "volume of prism of empty shape" $ 
            assertEqual "volume is zero" 0.0 (Solids.volume . Solids.prism 1.0 . TwoD.Shape.makeShape $ mempty)
        ]
    , testGroup "transforms" 
        [ testCase "scale by zero" .
            assertEqual "volume is zero" 0.0 . Solids.volume . Transforms.scale zero $ Solids.unitCube
        , testCase "scale by vector with zero element" (
            let s = Transforms.scale (V3 1 0 0.5) $ Solids.unitCube
            in (assertEqual "volume is zero" 0.0 . Solids.volume $ s)            
                <> (assertApproxEqual "volume with union is correct" 1e-6 1.0 . Solids.volume . (<> Solids.centeredCube) $ s)
                <> (assertEqual "union has one part" 1 . length . Solids.looseParts . (<> Solids.centeredCube) $ s)
                <> (assertEqual "has no parts" 0 . length . Solids.looseParts $ s)
        )
        , testCase "uScale by zero" .
            assertEqual "volume is zero" 0.0 . Solids.volume . Transforms.uScale 0 $ Solids.unitCube
        , testCase "matTransform by zero matrix" (
            let s = Transforms.matTransform zero Solids.unitCube
            in (assertEqual "volume is zero" 0.0 . Solids.volume $ s)
                <> (assertEqual "has no parts" 0 . length . Solids.looseParts $ s)
        )
        , testCase "matTransform by translation with zero linear part" (
            let s = Transforms.matTransform (V3 (V4 0 0 0 1) (V4 0 0 0 2) (V4 0 0 0 3)) Solids.unitCube
            in (assertEqual "volume is zero" 0.0 . Solids.volume $ s)
                <> (assertEqual "has no parts" 0 . length . Solids.looseParts $ s)
        )
        , testCase "matTransform by singular diagonal matrix" (
            let s = Transforms.matTransform (V3 (V4 1 0 0 0) (V4 0 0 0 0) (V4 0 0 0.5 0)) Solids.unitCube
            in (assertEqual "volume is zero" 0.0 . Solids.volume $ s)
                <> (assertApproxEqual "volume with union is correct" 1e-6 1.0 . Solids.volume . (<> Solids.centeredCube) $ s)
                <> (assertEqual "has no parts" 0 . length . Solids.looseParts $ s)
        )
        , testCase "matTransform by singular matrix that is not axis aligned" (
            let s = Transforms.matTransform (V3 (V4 1 0 0 0) (V4 0 1 0 0) (V4 1 1 0 0)) Solids.unitCube
            in (assertEqual "volume is zero" 0.0 . Solids.volume $ s)
                <> (assertApproxEqual "volume with union is correct" 1e-6 1.0 . Solids.volume . (<> Solids.centeredCube) $ s)
                <> (assertEqual "has no parts" 0 . length . Solids.looseParts $ s)
        )
        , testCase "matTransform by rank one matrix" (
            let s = Transforms.matTransform (V3 (V4 1 1 1 0) (V4 1 1 1 0) (V4 1 1 1 0)) Solids.unitCube
            in (assertEqual "volume is zero" 0.0 . Solids.volume $ s)
                <> (assertApproxEqual "volume with union is correct" 1e-6 1.0 . Solids.volume . (<> Solids.centeredCube) $ s)
                <> (assertEqual "has no parts" 0 . length . Solids.looseParts $ s)
        )
        , testCase "matTransform by projection onto a plane" (
            let n = normalize (V3 1 2 3)
                projection = identity !-! outer n n
                s = Transforms.matTransform (fmap (\(V3 a b c) -> V4 a b c 0) projection) Solids.unitCube
            in (assertApproxEqual "volume with union is correct" 1e-6 1.0 . Solids.volume . (<> Solids.centeredCube) $ s)
                <> (assertEqual "has no parts" 0 . length . Solids.looseParts $ s)
        )
        , testCase "matTransform by shear preserves volume" .
            assertApproxEqual "volume is correct" 1e-6 1.0 . Solids.volume 
                . Transforms.matTransform (V3 (V4 1 1 0 0) (V4 0 1 0 0) (V4 0 0 1 0)) $ Solids.unitCube
        , testCase "matTransform by small invertible matrix is not empty" (
            let s = Transforms.matTransform (V3 (V4 0.001 0 0 0) (V4 0 0.002 0 0) (V4 0 0 0.003 0)) Solids.unitCube
            in (assertApproxEqual "volume is correct" 1e-12 6e-9 . Solids.volume $ s)
                <> (assertEqual "has one part" 1 . length . Solids.looseParts $ s)
        )
        ]
    , testGroup "offset" $ 
        let steinerVolume volume area meanCurvature d = 
                volume + area * d + meanCurvature * d * d + 4 * pi * d * d * d / 3
            stretchedCylinder = Transforms.scale (V3 1 1 3) Solids.unitCylinder
            stretchedHole = Transforms.scale (V3 0.5 0.5 5) Solids.centeredCylinder
            prismHole = Transforms.translate (V3 0 0 (-2.5)) . Solids.prism 5 . TwoD.Transforms.uScale2D 0.5 $ TwoD.Shape.unitCircle
            cubeWithHole = Booleans.difference (Transforms.uScale 3 Solids.centeredCube)
        in  [ testCase "outward offset of a non-uniformly scaled cylinder" $
                let d = 0.5
                    expected = steinerVolume (3 * pi) (8 * pi) (3 * pi + pi * pi) d
                in assertApproxEqual "volume is correct" 1e-3 expected . Solids.volume 
                    . Offset.offset d $ stretchedCylinder
            , testCase "inward offset of a non-uniformly scaled cylinder" .
                assertApproxEqual "volume is correct" 1e-4 (pi * 0.75 * 0.75 * 2.5) . Solids.volume 
                    . Offset.offset (-0.25) $ stretchedCylinder
            , testCase "outward offset of a non-uniformly scaled sphere" $
                let d = 0.1
                    focalDistance = sqrt 3
                    eccentricity = focalDistance / 2
                    volume = 8 * pi / 3
                    area = 2 * pi * (1 + 2 * asin eccentricity / eccentricity)
                    meanCurvature = 2 * pi * (2 + log ((2 + focalDistance) / (2 - focalDistance)) / (2 * focalDistance))
                in assertApproxEqual "volume is correct" 1e-3 (steinerVolume volume area meanCurvature d) . Solids.volume 
                    . Offset.offset d . Transforms.scale (V3 1 1 2) $ Solids.unitSphere
            , testCase "inward offset of a cube with a non-uniformly scaled through hole" .
                assertApproxEqual "volume is correct" 1e-4 (2.8 * 2.8 * 2.8 - pi * 0.6 * 0.6 * 2.8) . Solids.volume 
                    . Offset.offset (-0.1) $ cubeWithHole stretchedHole
            , testCase "outward offset of a cube with a non-uniformly scaled through hole" $
                let offsetVolume = Solids.volume . Offset.offset 0.1 . cubeWithHole
                in (assertApproxEqual "volume matches the same hole made without scaling" 1e-4 (offsetVolume prismHole) . offsetVolume $ stretchedHole)
                    <> (assertEqual "has one part" 1 . length . Solids.looseParts . Offset.offset 0.1 . cubeWithHole $ stretchedHole)
            ]
    ]