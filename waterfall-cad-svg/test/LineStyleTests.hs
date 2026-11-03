module LineStyleTests
( lineStyleTests
) where

import Waterfall.SVG.ToSVG.LineStyle
import qualified Waterfall.Colour as Colour
import Control.Lens ((^.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

lineStyleTests :: TestTree
lineStyleTests = testGroup "LineStyle"
    [ lineStyleShowTests
    , diagramStyleShowTests
    , lensTests
    ]

lineStyleShowTests :: TestTree
lineStyleShowTests = testGroup "LineStyle Show instance"
    [ testCase "mempty" $
        assertEqual "" "mempty" (show (mempty :: LineStyle))

    , testCase "single property" $
        assertEqual ""
            "lineWithColour (rgb24 0xff0000)"
            (show (lineWithColour (Colour.rgb24 0xFF0000)))

    , testCase "combined properties" $
        assertEqual ""
            "lineWithColour (rgb24 0x0000ff) <> lineWithOpacity 0.5"
            (show
                (lineWithOpacity 0.5
                <> lineWithColour (Colour.rgb24 0xFF0000)
                <> lineWithColour (Colour.rgb24 0x0000FF)
                ))

    , testCase "all properties" $
        assertEqual ""
            "lineWithColour (rgb24 0xff0000) <> lineWithOpacity 0.5 <> lineWithWidth 2.0"
            (show
                (lineWithWidth 2
                <> lineWithOpacity 0.5
                <> lineWithColour (Colour.rgb24 0xFF0000)
                ))

    , testCase "negative argument in brackets" $
        assertEqual ""
            "lineWithWidth (-1.0)"
            (show (lineWithWidth (-1)))

    , testCase "single property in brackets when used as an argument" $
        assertEqual ""
            "Just (lineWithWidth 2.0)"
            (show (Just (lineWithWidth 2)))

    , testCase "combined properties in brackets when used as an argument" $
        assertEqual ""
            "Just (lineWithOpacity (-0.5) <> lineWithWidth 2.0)"
            (show (Just (lineWithOpacity (-0.5) <> lineWithWidth 2)))
    ]

diagramStyleShowTests :: TestTree
diagramStyleShowTests = testGroup "DiagramStyle Show instance"
    [ testCase "mempty" $
        assertEqual "" "mempty" (show (mempty :: DiagramStyle))

    , testCase "visible line only" $
        assertEqual ""
            "diagramWithVisibleLine (lineWithWidth 2.0)"
            (show (diagramWithVisibleLine (lineWithWidth 2)))

    , testCase "hidden line only, with combined properties" $
        assertEqual ""
            "diagramWithHiddenLine (lineWithColour (rgb24 0x000000) <> lineWithOpacity 0.4)"
            (show (diagramWithHiddenLine (lineWithColour (Colour.rgb24 0x000000) <> lineWithOpacity 0.4)))

    , testCase "both lines" $
        assertEqual ""
            "diagramWithVisibleLine (lineWithWidth 2.0) <> diagramWithHiddenLine (lineWithOpacity 0.4)"
            (show
                (diagramWithHiddenLine (lineWithOpacity 0.4)
                <> diagramWithVisibleLine (lineWithWidth 2)
                ))

    , testCase "repeated lines are merged" $
        assertEqual ""
            "diagramWithVisibleLine (lineWithColour (rgb24 0xff0000) <> lineWithWidth 2.0) <> diagramWithHiddenLine (lineWithOpacity 0.4)"
            (show
                (diagramWithVisibleLine (lineWithWidth 2)
                <> diagramWithHiddenLine (lineWithOpacity 0.4)
                <> diagramWithVisibleLine (lineWithColour (Colour.rgb24 0xFF0000)
                )))

    , testCase "visible line only in brackets when used as an argument" $
        assertEqual ""
            "Just (diagramWithVisibleLine (lineWithWidth 2.0))"
            (show (Just (diagramWithVisibleLine (lineWithWidth 2))))

    , testCase "both lines in brackets when used as an argument" $
        assertEqual ""
            "Just (diagramWithVisibleLine (lineWithWidth 2.0) <> diagramWithHiddenLine (lineWithOpacity 0.4))"
            (show
                (Just
                    (diagramWithVisibleLine (lineWithWidth 2)
                    <> diagramWithHiddenLine (lineWithOpacity 0.4)
                    )))

    , testCase "defaultStyle" $
        assertEqual ""
            "diagramWithVisibleLine (lineWithColour (rgb24 0x000000) <> lineWithOpacity 1.0 <> lineWithWidth 1.0) <> diagramWithHiddenLine (lineWithColour (rgb24 0x000000) <> lineWithOpacity 0.4 <> lineWithWidth 1.0)"
            (show defaultDiagramStyle)
    ]

lensTests :: TestTree
lensTests = testGroup "lenses"
    [ testCase "lineColour" $
        assertEqual "" (Just Colour.red) (lineWithColour Colour.red ^. lineColour)

    , testCase "lineOpacity" $
        assertEqual "" (Just 0.5) (lineWithOpacity 0.5 ^. lineOpacity)

    , testCase "lineWidth" $
        assertEqual "" (Just 2) (lineWithWidth 2 ^. lineWidth)

    , testCase "diagramVisibleLine" $
        assertEqual ""
            (lineWithWidth 2)
            (diagramWithVisibleLine (lineWithWidth 2) ^. diagramVisibleLine)

    , testCase "diagramHiddenLine" $
        assertEqual ""
            (lineWithWidth 2)
            (diagramWithHiddenLine (lineWithWidth 2) ^. diagramHiddenLine)
    ]
