{-# LANGUAGE ScopedTypeVariables #-}
{-|
Paths in 2D space.

This module exposes functions with the same names as "Waterfall.Path", and if used together they should be imported qualified.
-}
module Waterfall.TwoD.Path2D
( Path2D
, Sense (..)
, line
, lineTo
, lineRelative
, arc
, arcTo
, arcRelative
, arcVia
, arcViaTo
, arcViaRelative
, bezier
, bezierTo
, bezierRelative
, pathFrom
, pathFromTo
, repeatLooping
, closeLoop
) where 

import Waterfall.TwoD.Internal.Path2D (Path2D(..), joinPaths)
import Waterfall.TwoD.Transforms (rotate2D)
import qualified Waterfall.Internal.Edges as Internal.Edges
import Control.Arrow (second)
import Data.Foldable (traverse_, foldl')
import Linear.V2 (V2(..))
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.))
import Linear ((^*), _xy, distance, normalize, unangle)
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

v2ToPnt :: V2 Double -> Acquire (Ptr GP.Pnt)
v2ToPnt (V2 x y) = GP.Pnt.new x y 0

edgesToPath :: Acquire [Ptr TopoDS.Edge] -> Path2D
edgesToPath es = Path2D $ do
    edges <- es
    builder <- MakeWire.new
    liftIO $ traverse_ (MakeWire.addEdge builder) edges
    MakeWire.wire builder

-- | A straight line between two points
line :: V2 Double -> V2 Double -> Path2D
line start end = edgesToPath $ do
    pt1 <- v2ToPnt start
    pt2 <- v2ToPnt end
    pure <$> MakeEdge.fromPnts pt1 pt2

-- | Version of `line` designed to work with `pathFrom`
lineTo :: V2 Double -> V2 Double -> (V2 Double, Path2D)
lineTo end = \start -> (end, line start end) 

-- | Version of `line` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the endpoint
-- relative to the start of the line, rather than in absolute space.
lineRelative :: V2 Double -> V2 Double -> (V2 Double, Path2D)
lineRelative dEnd = do
    end <- (+ dEnd)
    lineTo end

-- | Section of a circle based on three arguments, the start point, a point on the arc, and the endpoint
arcVia :: V2 Double -> V2 Double -> V2 Double -> Path2D
arcVia start mid end = edgesToPath $ do
    s <- v2ToPnt start
    m <- v2ToPnt mid
    e <- v2ToPnt end
    theArc <- MakeArcOfCircle.from3Pnts s m e
    pure <$> MakeEdge.fromCurve (upcast theArc)

-- | Version of `arcVia` designed to work with `pathFrom`
--
-- The first argument is a point on the arc.
-- The second argument is the endpoint of the arc
arcViaTo :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcViaTo mid end = \start -> (end, arcVia start mid end) 


-- | Version of `arcVia` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the midpoint and endpoint
-- relative to the start of the line, rather than in absolute space.
arcViaRelative :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcViaRelative dMid dEnd = do
    mid <- (+ dMid) 
    end <- (+ dEnd) 
    arcViaTo mid end

data Sense = Clockwise | Counterclockwise deriving (Eq, Show)


-- | Section of a circle, with a given radius, that lies between two points.
--
-- This may fail, if the radius is less than half of the distance between the points.
--
-- In general, `arcVia` is the \"safer\" way to construct an arc
arc :: Sense -> Double -> V2 Double -> V2 Double -> Path2D 
arc sense radius start end = 
    let mid = (start + end) ^* 0.5
        (V2 dx dy) = normalize $ end - start
        rotD = case sense of    
                Clockwise -> V2 dy (-dx)
                Counterclockwise -> V2 (-dy) dx
        dse = distance start end 
        tangent = radius - sqrt (radius * radius - dse * dse / 4) 
        arcMid = mid + rotD ^* tangent
    in if dse > 2 * radius
            then error "points too far apart in arc"
            else arcVia start arcMid end  

-- | Version of `arc` designed to work with `pathFrom`
arcTo :: Sense -> Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcTo sense radius end = \start -> (end, arc sense radius start end) 

-- | Version of `arc` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the endpoint
-- relative to the start of the line, rather than in absolute space.
arcRelative :: Sense -> Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcRelative sense radius dEnd = do
    end <- (+ dEnd)
    arcTo sense radius end

-- | Bezier curve of order 3
-- 
-- The arguments are, the start of the curve, the two control points, and the end of the curve
bezier :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Path2D
bezier start controlPoint1 controlPoint2 end = edgesToPath $ do
    s <- v2ToPnt start
    c1 <- v2ToPnt controlPoint1
    c2 <- v2ToPnt controlPoint2
    e <- v2ToPnt end
    arr <- NCollection.Array1.newGPPntArray 1 4
    liftIO $ do 
        NCollection.Array1.setValueGPPnt arr 1 s
        NCollection.Array1.setValueGPPnt arr 2 c1
        NCollection.Array1.setValueGPPnt arr 3 c2
        NCollection.Array1.setValueGPPnt arr 4 e
    b <- BezierCurve.toHandle =<< BezierCurve.fromPnts arr
    pure <$> MakeEdge.fromCurve (upcast b)
 
-- | Version of `bezier` designed to work with `pathFrom`
bezierTo :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
bezierTo controlPoint1 controlPoint2 end = \start -> (end, bezier start controlPoint1 controlPoint2 end) 

-- | Version of `bezier` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the control points and the endpoint
-- relative to the start of the line, rather than in absolute space.
bezierRelative :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
bezierRelative dControlPoint1 dControlPoint2 dEnd = do
    controlPoint1 <- (+ dControlPoint1)
    controlPoint2 <- (+ dControlPoint2)
    end <- (+ dEnd)
    bezierTo controlPoint1 controlPoint2 end

-- | When combining paths, we're generally interested in pairs of paths that share a common endpoint.
--
-- Rather than having to repeat these common endpoints, `pathFrom` can be used to combine a list of path components.
-- 
-- Where a path component is a function from a start point, to a tuple of an end point, and a path; @ V2 Double -> (V2 Double, Path2D) @. 
-- 
-- A typical use of `pathFrom` uses a list of functions with the suffix \"To\" or \"Relative\", e.g.
--
-- @
--Path2D.pathFrom (V2 (-1) (-1)) 
--    [ Path2D.arcViaTo (V2 (-1.5) 0) (V2 (-1) 1)
--    , Path2D.lineTo (V2 1 1)
--    , Path2D.bezierTo (V2 1.5 1) (V2 1.5 (-1)) (V2 1 (-1))
--    , Path2D.lineTo (V2 (-1) (-1))
--    ] @
pathFrom :: V2 Double -> [V2 Double -> (V2 Double, Path2D)] -> Path2D
pathFrom start commands = snd $ pathFromTo commands start 
     
-- | Combines a list of "path components", as used by `pathFrom`
pathFromTo :: [V2 Double -> (V2 Double, Path2D)] -> V2 Double -> (V2 Double, Path2D)
pathFromTo commands start = 
    let go (pos, paths) cmd = second (:paths) (cmd pos)
        (end, allPaths) = foldl' go (start, []) commands
     in (end, joinPaths allPaths)

-- | Given a Path where both endpoints are equidistant from the origin.
--
-- And which subtends an angle \( φ \) from the origin that evenly divides a complete revolution, such that \(n φ = 2 π \).
-- 
-- Replicates the path \( n \) times, rotating it by \( φ \), until the resulting path completes one revolution around the origin.
--
-- This can be used to construct paths with rotational symmetry, such as regular polygons, or gears.
repeatLooping :: Path2D -> Path2D
repeatLooping p = Path2D $ do
    path <- runPath p 
    (s, e) <- liftIO . Internal.Edges.wireEndpoints $ path
    let a = unangle (e ^. _xy) - unangle (s ^. _xy)
    let times :: Integer = abs . round $ pi * 2 / a 
    runPath $ mconcat [rotate2D (negate (fromIntegral n) * a) p | n <- [0..times]]

-- | Given a path, return a new path with the endpoints joined by a straight line.
closeLoop :: Path2D -> Path2D
closeLoop p = Path2D $ do
    path <- runPath p
    (s, e) <- liftIO . Internal.Edges.wireEndpoints $ path
    runPath $ mconcat [p, line (e ^. _xy)  (s ^. _xy)]



