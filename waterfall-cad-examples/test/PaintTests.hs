module PaintTests
( paintTests
) where

import Waterfall.Paint
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

paintTests :: TestTree
paintTests = testGroup "Paint Show instance"
    [ testCase "mempty" $
        assertEqual "" "mempty" (show (mempty :: Paint))

    , testCase "single property" $
        assertEqual ""
            "paintWithColour (Colour 1.0 0.0 0.0)"
            (show (paintWithColour (Colour 1 0 0)))

    , testCase "combined properties" $
        assertEqual ""
            "paintWithColour (Colour 0.0 0.0 1.0) <> paintWithOpacity 0.5 <> paintWithName \"blue\""
            (show 
                (paintWithName "blue" 
                <> paintWithColour (Colour 1 0 0) 
                <> paintWithOpacity 0.5 
                <> paintWithColour (Colour 0 0 1)
            ))

    , testCase "all properties" $
        assertEqual ""
            "paintWithColour (Colour 1.0 0.0 0.0) <> paintWithOpacity 0.5 <> paintWithMetallic 0.2 <> paintWithRoughness 0.1 <> paintWithEmissiveColour (Colour 0.0 0.0 1.0) <> paintWithName \"red\" <> paintWithDoubleSidedness DoubleSided"
            (show 
                (paintWithName "red" 
                <> paintWithColour (Colour 1 0 0) 
                <> paintWithOpacity 0.5 
                <> paintWithMetallic 0.2
                <> paintWithRoughness 0.1
                <> paintWithEmissiveColour (Colour 0 0 1)
                <> paintWithDoubleSidedness DoubleSided
                ))

    , testCase "in brackets when used as an argument" $
        assertEqual ""
            "Just (paintWithOpacity (-0.5) <> paintWithDoubleSidedness SingleSided)"
            (show (Just (paintWithOpacity (-0.5) <> paintWithDoubleSidedness SingleSided)))
    ]
