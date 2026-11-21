{-# LANGUAGE ScopedTypeVariables #-}
module PathTests
( pathTests
) where

import qualified Waterfall.Path as Path
import qualified Waterfall.TwoD.Path2D as TwoD.Path2D
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import Linear (V3 (..), V4 (..), V2 (..), zero, angle, unit, norm, _x, _y, _z, (^*))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Waterfall.SVG.ToSVG (path2DToPathCommands)
import Control.Exception (evaluate)
import Control.Monad (void)
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
    , testGroup "arc" 
        [ testCase "exact semicircles" $ mconcat 
            [ let r = 0.1 in 
                assertApproxEqual "has correct length (at origin)" 1e-6  (r * pi) . Path.pathLength 
                    $ TwoD.Path2D.arc TwoD.Path2D.Clockwise r (V2 0 0) (V2 0.2 0)
            , let r = 0.1 in 
                assertApproxEqual "has correct length (not at origin)" 1e-6  (r * pi) . Path.pathLength 
                    $ TwoD.Path2D.arc TwoD.Path2D.Clockwise r (V2 0.7 0) (V2 0.9 0)
            ]
        , testCase "positive and negative radii" $ mconcat
            [let r = 1 in 
                assertApproxEqual "has correct length (chord length 1)" 1e-6  (pi / 3) . Path.pathLength 
                    $ TwoD.Path2D.arc TwoD.Path2D.Clockwise r (V2 0 0) (V2 1 0)
            , let r = negate 1 in 
                assertApproxEqual "has correct length (negative radius)" 1e-6  (pi / 3) . Path.pathLength 
                    $ TwoD.Path2D.arc TwoD.Path2D.Clockwise r (V2 0 0) (V2 1 0)
            ]
        , testCase "coincident start and end" $ 
            let p = TwoD.Path2D.arc TwoD.Path2D.Clockwise 1 (unit _x) (unit _x)
                in assertApproxEqual "has correct length" 1e-6 0 .  Path.pathLength $ p
        ]
    , testGroup "arcVia" 
        [ testCase "collinear points" $
            let o = V2 1.2 3
                d = V2 3 1.2
                p :: TwoD.Path2D.Path2D = Path.arcVia o (o + d ^* 0.4) (o + d)
            in assertApproxEqual "has correct length" 1e-6 (norm d) .  Path.pathLength $ p
        ]
    , testGroup "takePathFraction"
        [ testCase "line" $
            assertApproxEqual "correct length" 1e-6 0.1 . Path.pathLength . Path.takePathFraction 0.1 $ (Path.line zero (unit _z) :: Path.Path) 
        , testCase "tiny fraction" $
            assertApproxEqual "correct length" 1e-6 0 . Path.pathLength . Path.takePathFraction 1e-9 $ (Path.line zero (unit _z) :: Path.Path) 
        , testCase "small fraction & small length" $
            assertApproxEqual "correct length" 1e-6 0 . Path.pathLength . Path.takePathFraction 1e-5 $ (Path.line zero (unit _z ^* 1e-3) :: Path.Path) 
        ]
    , testGroup "repeatLooping"
        [ testCase "line" $
            let seg = Path.line (angle (pi/18)) (angle (3 * pi/18))
            in assertApproxEqual "has correct length" 1e-6 18 ((Path.pathLength . TwoD.Path2D.repeatLooping $ seg) / (Path.pathLength seg))
        , testCase "line over negative x axis" $
            let seg = Path.line (angle (pi + pi/18)) (angle (pi - pi/18))
            in assertApproxEqual "has correct length" 1e-6 18 ((Path.pathLength . TwoD.Path2D.repeatLooping $ seg) / (Path.pathLength seg))
        , testCase "line over positive y axis" $
            let seg = Path.line (angle (pi/2 + pi/18)) (angle (pi/2 - pi/18))
            in assertApproxEqual "has correct length" 1e-6 18 ((Path.pathLength . TwoD.Path2D.repeatLooping $ seg) / (Path.pathLength seg))
        , testCase "line over negative y axis" $
            let seg = Path.line (angle (3*pi/2 + pi/18)) (angle (3*pi/2 - pi/18))
            in assertApproxEqual "has correct length" 1e-6 18 ((Path.pathLength . TwoD.Path2D.repeatLooping $ seg) / (Path.pathLength seg))
        , testCase "line over negative y axis (reversed)" $
            let seg = Path.line (angle (3*pi/2 - pi/18)) (angle (3*pi/2 + pi/18))
            in assertApproxEqual "has correct length" 1e-6 18 ((Path.pathLength . TwoD.Path2D.repeatLooping $ seg) / (Path.pathLength seg))
        ]
        , testGroup "transforms" $
            let square :: Path.Path = Path.pathFrom zero 
                    [ Path.lineTo (unit _x)
                    , Path.lineTo (unit _x + unit _y)
                    , Path.lineTo (unit _y)
                    , Path.lineTo zero
                    ]
                routeAroundCube :: Path.Path = Path.pathFrom zero 
                    [ Path.lineTo (unit _x)
                    , Path.lineTo (unit _x + unit _z)
                    , Path.lineTo (unit _x + unit _y + unit _z)
                    , Path.lineTo (unit _y + unit _z)
                    , Path.lineTo (unit _y)
                    , Path.lineTo zero
                    ]
            in  [ testCase "scale by zero" .
                    assertEqual "length is zero" 0.0 . Path.pathLength . Transforms.scale zero $ square
                , testCase "scale by vector with zero element" (
                    let p = Transforms.scale (V3 1 0.5 0) routeAroundCube
                    in (assertApproxEqual "length is correct"  1e-6 3.0 . Path.pathLength $ p )            
                        <> (assertBool "has no degenerate segments" . all ((> 0 ) . Path.pathLength) . Path.splitPath $ p )
                )
                , testCase "uScale by zero" .
                    assertEqual "length is zero" 0.0 . Path.pathLength . Transforms.uScale 0 $ square
                , testCase "scale that collapses the whole path" (
                    let p :: Path.Path = Transforms.scale (V3 1 1 0) (Path.line (V3 1 2 0) (V3 1 2 5))
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point" (Just (V3 1 2 0, V3 1 2 0)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                , testCase "matTransform by zero matrix" (
                    let p = Transforms.matTransform zero square
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point at the origin" (Just (zero, zero)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                , testCase "matTransform by translation with zero linear part" (
                    let p = Transforms.matTransform (V3 (V4 0 0 0 1) (V4 0 0 0 2) (V4 0 0 0 3)) square
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point at the translation" (Just (V3 1 2 3, V3 1 2 3)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                , testCase "matTransform by singular diagonal matrix" (
                    let p = Transforms.matTransform (V3 (V4 1 0 0 0) (V4 0 0.5 0 0) (V4 0 0 0 0)) routeAroundCube
                    in (assertApproxEqual "length is correct" 1e-6 3.0 . Path.pathLength $ p)
                        <> (assertBool "has no degenerate segments" . all ((> 0) . Path.pathLength) . Path.splitPath $ p)
                )
                , testCase "matTransform by singular matrix that is not axis aligned" (
                    let p = Transforms.matTransform (V3 (V4 1 0 0 0) (V4 0 1 0 0) (V4 1 1 0 0)) routeAroundCube
                    in (assertApproxEqual "length is correct" 1e-6 (4 * sqrt 2) . Path.pathLength $ p)
                        <> (assertBool "has no degenerate segments" . all ((> 0) . Path.pathLength) . Path.splitPath $ p)
                )
                , testCase "matTransform by rank one matrix" (
                    let p = Transforms.matTransform (V3 (V4 1 0 0 0) (V4 0 0 0 0) (V4 0 0 0 0)) routeAroundCube
                    in (assertApproxEqual "length is correct" 1e-6 2.0 . Path.pathLength $ p)
                        <> (assertBool "has no degenerate segments" . all ((> 0) . Path.pathLength) . Path.splitPath $ p)
                )
                , testCase "matTransform that collapses the whole path" (
                    let p :: Path.Path = Transforms.matTransform (V3 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 0 7)) (Path.line (V3 1 2 0) (V3 1 2 5))
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point" (Just (V3 1 2 7, V3 1 2 7)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                ]
        , testGroup "transforms2D" $
            let square :: TwoD.Path2D.Path2D = Path.pathFrom zero 
                    [ Path.lineTo (unit _x)
                    , Path.lineTo (unit _x + unit _y)
                    , Path.lineTo (unit _y)
                    , Path.lineTo zero
                    ]
                triangle :: TwoD.Path2D.Path2D = Path.pathFrom zero 
                    [ Path.lineTo (unit _x)
                    , Path.lineTo (unit _y)
                    , Path.lineTo zero
                    ]
                vertical :: TwoD.Path2D.Path2D = Path.line (V2 3 1) (V2 3 5)
            in  [ testCase "scale2D by zero" .
                    assertEqual "length is zero" 0.0 . Path.pathLength . TwoD.Transforms.scale2D zero $ square
                , testCase "scale2D by vector with zero element" (
                    let p = TwoD.Transforms.scale2D (V2 1 0) square
                    in (assertApproxEqual "length is correct" 1e-6 2.0 . Path.pathLength $ p)
                        <> (assertBool "has no degenerate segments" . all ((> 0) . Path.pathLength) . Path.splitPath $ p)
                )
                , testCase "uScale2D by zero" .
                    assertEqual "length is zero" 0.0 . Path.pathLength . TwoD.Transforms.uScale2D 0 $ square
                , testCase "scale2D that collapses the whole path" (
                    let p = TwoD.Transforms.scale2D (V2 1 0) vertical
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point" (Just (V2 3 0, V2 3 0)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                , testCase "matTransform2D by zero matrix" (
                    let p = TwoD.Transforms.matTransform2D zero square
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point at the origin" (Just (zero, zero)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                , testCase "matTransform2D by translation with zero linear part" (
                    let p = TwoD.Transforms.matTransform2D (V2 (V3 0 0 1) (V3 0 0 2)) square
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point at the translation" (Just (V2 1 2, V2 1 2)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                , testCase "matTransform2D by singular diagonal matrix" (
                    let p = TwoD.Transforms.matTransform2D (V2 (V3 1 0 0) (V3 0 0 0)) square
                    in (assertApproxEqual "length is correct" 1e-6 2.0 . Path.pathLength $ p)
                        <> (assertBool "has no degenerate segments" . all ((> 0) . Path.pathLength) . Path.splitPath $ p)
                )
                , testCase "matTransform2D by singular matrix that is not axis aligned" (
                    let p = TwoD.Transforms.matTransform2D (V2 (V3 1 1 0) (V3 1 1 0)) triangle
                    in (assertApproxEqual "length is correct" 1e-6 (2 * sqrt 2) . Path.pathLength $ p)
                        <> (assertBool "has no degenerate segments" . all ((> 0) . Path.pathLength) . Path.splitPath $ p)
                )
                , testCase "matTransform2D that collapses the whole path" (
                    let p = TwoD.Transforms.matTransform2D (V2 (V3 1 0 0) (V3 0 0 7)) vertical
                    in (assertEqual "length is zero" 0.0 . Path.pathLength $ p)
                        <> (assertEqual "is a single point" (Just (V2 3 7, V2 3 7)) . Path.pathEndpoints $ p)
                        <> (assertEqual "reversed length is zero" 0.0 . Path.pathLength . Path.reversePath $ p)
                )
                ]
    , testGroup "svg export"
        [ testCase "exporting part of a bezier leaves the path and its source unchanged" $ 
            let source = TwoD.Path2D.bezier (V2 0 0) (V2 1 2) (V2 3 2) (V2 4 0) :: TwoD.Path2D.Path2D
                half = Path.takePathFraction 0.5 source
                export = void . evaluate . length . show . path2DToPathCommands $ half
            in export
                <> (assertApproxEqual "source length is unchanged" 1e-6 5.268364935508777 . Path.pathLength $ source)
                <> (assertApproxEqual "exported path length is unchanged" 1e-6 2.634182467542634 . Path.pathLength $ half)
                <> (assertApproxEqual "exported path still ends halfway along the source" 1e-6 0 . maybe 1 (norm . subtract (V2 2 1.5) . snd) . Path.pathEndpoints $ half)
        ]
    ]
