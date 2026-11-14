module SolidTests
( solidTests
) where

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Revolution as Revolution
import Linear (V3 (..), unit, _x, _z, (^*))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import qualified Waterfall.Fillet as Fillet

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
        ]
    ]
