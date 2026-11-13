module SolidTests
( solidTests
) where

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Revolution as Revolution
import Linear (V3 (..), unit, _x, (^*))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

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
    ]
