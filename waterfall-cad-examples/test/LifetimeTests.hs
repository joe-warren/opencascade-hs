module LifetimeTests
( lifetimeTests
) where

import qualified Waterfall
import Linear (V2 (..), V3 (..), (*^), unit, _x, _y, _z, zero)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( testCase, assertBool )
import Control.Exception (evaluate, bracket)
import System.Mem (performMajorGC)
import Control.Monad (forM, forM_, forever)
import Control.Concurrent (killThread, forkIO)

gcEvaluate :: a -> IO a
gcEvaluate x = do
    performMajorGC 
    v <- evaluate x
    performMajorGC
    return v

lifetimeTests :: TestTree 
lifetimeTests = testGroup "Lifetime Tests"
    [ testCase "reusing a solid across unions" $ 
        let base = Waterfall.scale (V3 10 10 0.2) Waterfall.centeredCube
        in forM_ [0 .. 19 :: Int] $ \i -> do    
            let pieces = 
                    [   let d = V3 (fromIntegral j - 3.5) (fromIntegral i / 10) 0
                        in Waterfall.translate d Waterfall.unitCube 
                    | j <- [0 .. 7 :: Int] 
                    ]
            v <- gcEvaluate . Waterfall.volume $ mconcat (base : pieces)
            assertBool ("union " <> show i <> " has sufficient volume" ) (v > 20)
    , testCase "diagram lines outlive the diagram" $ do
        let solid = mconcat 
                [ Waterfall.translate (fromIntegral j *^ unit _x) Waterfall.unitSphere
                | j <- [0..7 ::Int ]
                ] 
        ls <- gcEvaluate $ Waterfall.diagramLines Waterfall.OutLine Waterfall.Visible (Waterfall.solidDiagram (V3 1 1 1) solid)
        lengths <- forM ls (gcEvaluate . Waterfall.pathLength)
        assertBool "diagram has lines" (not . null $ ls)
        assertBool "lines have length" (all (>0) lengths)
    , testCase "paths derived from a shape" $ do
        paths <- gcEvaluate $ Waterfall.shapePaths (Waterfall.unitSquare <> Waterfall.translate2D (V2 0.5 0.5) Waterfall.unitSquare) 
        pieces <- gcEvaluate $ concatMap Waterfall.splitPath paths
        joined <- gcEvaluate (mconcat pieces)
        len <- gcEvaluate $ Waterfall.pathLength joined
        assertBool "shape has an outline" (not . null $ paths)
        assertBool "outline splits into more segments " (length pieces > length paths)
        assertBool "joined outline has a length" (len > 0)
    , testCase "spliced and reversed paths" $ do
        let segment = Waterfall.line2D zero (V2 1 0.5)
            manySegments = Waterfall.pathFrom2D zero (replicate 10 $ Waterfall.splice segment)
        len <- gcEvaluate . Waterfall.pathLength . Waterfall.reversePath $ manySegments
        assertBool "spliced path has expected length" (abs (len - 10 * sqrt 1.25) < 1e-6)
    , testCase "extruded paths" $ do
        let square = Waterfall.pathFrom2D zero    
                [ Waterfall.lineTo (unit _x)
                , Waterfall.lineTo (V2 1 1)
                , Waterfall.lineTo (unit _y)
                , Waterfall.lineTo zero
                ] :: Waterfall.Path2D
        v <- gcEvaluate . Waterfall.volume . Waterfall.prism 1 . Waterfall.makeShape $ square
        assertBool "prism from path has unit volume" (abs (v - 1) < 1e-6)
    , testCase "booleans while gc is running concurrently" $
        bracket 
            (forkIO . forever $ performMajorGC)
            killThread 
            . const $ do
                let frame i =
                        let t = fromIntegral i / 10
                            base = Waterfall.scale (V3 10 10 1) Waterfall.centeredCube
                            holes = mconcat [ Waterfall.translate (V3 (fromIntegral j - 4) t 0) (Waterfall.scale (V3 0.4 0.4 3) Waterfall.centeredCylinder) | j <- [0..8 :: Int] ]
                            painted = Waterfall.colourSolid Waterfall.red (Waterfall.translate (V3 0 0 1) (Waterfall.scale (V3 2 2 2) Waterfall.centeredCube))
                        in Waterfall.rotate (unit _z) t ((base `Waterfall.difference` holes) <> painted)
                        
                forM_ [0 .. 100 :: Int] $ \i -> do
                    let s = frame i
                    v <- evaluate (Waterfall.volume s)
                    let run = " ( run no. " <> show i <> ")"
                    assertBool ("volume is positive" <> run) (v > 0)
                    ls <- evaluate (Waterfall.diagramLines Waterfall.SharpLine Waterfall.Visible (Waterfall.solidDiagram (V3 1 1 1) s))
                    lineLength <- evaluate (sum (map Waterfall.pathLength ls))
                    assertBool ("diagram has lines" <> run) (lineLength > 0)
    ]
