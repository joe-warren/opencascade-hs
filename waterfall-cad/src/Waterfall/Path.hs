{-|
Paths in 3D space.

This module exposes functions with the same names as "Waterfall.TwoD.Path2D", and if used together they should be imported qualified.
-}
module Waterfall.Path
( Path
, line
, lineTo
, lineRelative
, arcVia
, arcViaTo
, arcViaRelative
, bezier
, bezierTo
, bezierRelative
, pathFrom
, pathFromTo
) where

import Waterfall.Internal.Path (Path(..), joinPaths)
import Control.Arrow (second)
import Data.Foldable (traverse_, foldl')
import Linear.V3 (V3(..))
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt 
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GC.MakeArcOfCircle as MakeArcOfCircle
import qualified OpenCascade.NCollection.Array1 as NCollection.Array1
import qualified OpenCascade.Geom.BezierCurve as BezierCurve
import OpenCascade.Inheritance (upcast)
import Foreign.Ptr
import Data.Acquire

v3ToPnt :: V3 Double -> Acquire (Ptr GP.Pnt)
v3ToPnt (V3 x y z) = GP.Pnt.new x y z

edgesToPath :: Acquire [Ptr TopoDS.Edge] -> Path
edgesToPath es = Path $ do
    edges <- es
    builder <- MakeWire.new
    liftIO $ traverse_ (MakeWire.addEdge builder) edges
    MakeWire.wire builder

-- | A straight line between two points
line :: V3 Double -> V3 Double -> Path
line start end = edgesToPath $ do
    pt1 <- v3ToPnt start
    pt2 <- v3ToPnt end
    pure <$> MakeEdge.fromPnts pt1 pt2

-- | Version of `line` designed to work with `pathFrom`
lineTo :: V3 Double -> V3 Double -> (V3 Double, Path)
lineTo end = \start -> (end, line start end) 

-- | Version of `line` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the endpoint
-- relative to the start of the line, rather than in absolute space.
lineRelative :: V3 Double -> V3 Double -> (V3 Double, Path)
lineRelative dEnd = do
    end <- (+ dEnd)
    lineTo end

-- | Section of a circle based on three arguments, the start point, a point on the arc, and the endpoint
arcVia :: V3 Double -> V3 Double -> V3 Double -> Path
arcVia start mid end = edgesToPath $ do
    s <- v3ToPnt start
    m <- v3ToPnt mid
    e <- v3ToPnt end
    theArc <- MakeArcOfCircle.from3Pnts s m e
    pure <$> MakeEdge.fromCurve (upcast theArc)

-- | Version of `arcVia` designed to work with `pathFrom`
--
-- The first argument is a point on the arc
-- The second argument is the endpoint of the arc
arcViaTo :: V3 Double -> V3 Double -> V3 Double -> (V3 Double, Path)
arcViaTo mid end = \start -> (end, arcVia start mid end) 

-- | Version of `arcVia` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the midpoint and endpoint
-- relative to the start of the line, rather than in absolute space.
arcViaRelative :: V3 Double -> V3 Double -> V3 Double -> (V3 Double, Path)
arcViaRelative dMid dEnd = do
    mid <- (+ dMid) 
    end <- (+ dEnd) 
    arcViaTo mid end

-- | Bezier curve of order 3
-- 
-- The arguments are, the start of the curve, the two control points, and the end of the curve
bezier :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> Path
bezier start controlPoint1 controlPoint2 end = edgesToPath $ do
    s <- v3ToPnt start
    c1 <- v3ToPnt controlPoint1
    c2 <- v3ToPnt controlPoint2
    e <- v3ToPnt end
    arr <- NCollection.Array1.newGPPntArray 1 4
    liftIO $ do 
        NCollection.Array1.setValueGPPnt arr 1 s
        NCollection.Array1.setValueGPPnt arr 2 c1
        NCollection.Array1.setValueGPPnt arr 3 c2
        NCollection.Array1.setValueGPPnt arr 4 e
    b <- BezierCurve.toHandle =<< BezierCurve.fromPnts arr
    pure <$> MakeEdge.fromCurve (upcast b)

-- | Version of `bezier` designed to work with `pathFrom`
bezierTo :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> (V3 Double, Path)
bezierTo controlPoint1 controlPoint2 end = \start -> (end, bezier start controlPoint1 controlPoint2 end) 

-- | Version of `bezier` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the control points and the endpoint
-- relative to the start of the line, rather than in absolute space.
bezierRelative :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> (V3 Double, Path)
bezierRelative dControlPoint1 dControlPoint2 dEnd = do
    controlPoint1 <- (+ dControlPoint1)
    controlPoint2 <- (+ dControlPoint2)
    end <- (+ dEnd)
    bezierTo controlPoint1 controlPoint2 end

-- | When combining paths, we're generally interested in pairs of paths that share a common endpoint.
--
-- Rather than having to repeat these common endpoints, `pathFrom` can be used to combine a list of path components.
-- 
-- Where a path component is a function from a start point, to a tuple of an end point, and a path; @V2 Double -> (V2 Double, Path2D)@. 
-- 
-- A typical use of `pathFrom` uses a list of functions with the suffix \"To\" or \"Relative\", e.g:
--
-- @
--Path.pathFrom zero 
--    [ Path.bezierRelative (V3 0 0 0.5) (V3 0.5 0.5 0.5) (V3 0.5 0.5 1)
--    , Path.bezierRelative (V3 0 0 0.5) (V3 (-0.5) (-0.5) 0.5) (V3 (-0.5) (-0.5) 1)
--    , Path.arcViaRelative (V3 0 1 1) (V3 0 2 0)
--    , Path.lineTo (V3 0 2 0) 
--    ] @
pathFrom :: V3 Double -> [V3 Double -> (V3 Double, Path)] -> Path
pathFrom start commands = snd $ pathFromTo commands start 
     
-- | Combines a list of "path components", as used by `pathFrom`
pathFromTo :: [V3 Double -> (V3 Double, Path)] -> V3 Double -> (V3 Double, Path)
pathFromTo commands start = 
    let go (pos, paths) cmd = second (:paths) (cmd pos)
        (end, allPaths) = foldl' go (start, []) commands
     in (end, joinPaths allPaths)




