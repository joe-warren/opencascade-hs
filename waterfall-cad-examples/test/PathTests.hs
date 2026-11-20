{-# LANGUAGE ScopedTypeVariables #-}
module PathTests
( pathTests
) where

import qualified Waterfall.Path as Path
import qualified Waterfall.TwoD.Path2D as TwoD.Path2D
import qualified Waterfall.Transforms as Transforms
import Linear (V3 (..), V2 (..), zero, angle, unit, norm, _x, _y, _z, (^*))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
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
                ]
    ]