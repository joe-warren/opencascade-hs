module PaintTests
( paintTests
) where

import Waterfall.Paint
import Waterfall.Colour (rgb24, rgb)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

paintTests :: TestTree
paintTests = testGroup "Paint Show instance"
    [ testCase "mempty" $
        assertEqual "" "mempty" (show (mempty :: Paint))

    , testCase "single property" $
        assertEqual ""
            "paintWithColour (rgb24 0xff0000)"
            (show (paintWithColour (rgb24 0xFF0000)))

    , testCase "colour falls back to double when unrepresentable" $
        assertEqual ""
            "paintWithColour (rgb 1.0 3.8910505836575876e-3 0.0)"
            (show (paintWithColour (rgb 1 (1 / 257) 0)))

    , testCase "combined properties" $
        assertEqual ""
            "paintWithColour (rgb24 0x0000ff) <> paintWithOpacity 0.5 <> paintWithName \"blue\""
            (show 
                (paintWithName "blue" 
                <> paintWithColour (rgb24 0xFF0000) 
                <> paintWithOpacity 0.5 
                <> paintWithColour (rgb24 0x0000FF)
            ))

    , testCase "all properties" $
        assertEqual ""
            "paintWithColour (rgb24 0xff0000) <> paintWithOpacity 0.5 <> paintWithMetallic 0.2 <> paintWithRoughness 0.1 <> paintWithEmissiveColour (rgb24 0x0000ff) <> paintWithName \"red\" <> paintWithDoubleSidedness DoubleSided"
            (show 
                (paintWithName "red" 
                <> paintWithColour (rgb24 0xFF0000) 
                <> paintWithOpacity 0.5 
                <> paintWithMetallic 0.2
                <> paintWithRoughness 0.1
                <> paintWithEmissiveColour (rgb24 0x0000FF)
                <> paintWithDoubleSidedness DoubleSided
                ))

    , testCase "in brackets when used as an argument" $
        assertEqual ""
            "Just (paintWithOpacity (-0.5) <> paintWithDoubleSidedness SingleSided)"
            (show (Just (paintWithOpacity (-0.5) <> paintWithDoubleSidedness SingleSided)))
    ]
