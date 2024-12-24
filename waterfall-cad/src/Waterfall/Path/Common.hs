{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Paths in 2D / 3D space.

This module defines functions that can be used with "Waterfall.Path" or "Waterfall.TwoD.Path2D".
Those modules both export monomorphized variants of the functions defined in this module
-}
module Waterfall.Path.Common 
( AnyPath ()
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
, pathEndpoints
, splice
, closeLoop
, reversePath
) where
import Data.Acquire
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import Foreign.Ptr
import Waterfall.Internal.Path (Path (..))
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.Internal.Finalizers (unsafeFromAcquire, toAcquire)
import Control.Arrow (second)
import Data.Foldable (foldl', traverse_)
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import qualified OpenCascade.GC.MakeArcOfCircle as MakeArcOfCircle
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import qualified OpenCascade.NCollection.Array1 as NCollection.Array1
import qualified OpenCascade.Geom.BezierCurve as BezierCurve
import qualified OpenCascade.GP.Trsf as GP.Trsf
import qualified OpenCascade.GP.Vec as  GP.Vec
import qualified OpenCascade.BRepBuilderAPI.Transform as BRepBuilderAPI.Transform
import Data.Proxy (Proxy (..))
import Linear (V3 (..), V2 (..), _xy, Epsilon, nearZero)
import qualified OpenCascade.GP.Pnt as GP.Pnt
import Control.Lens ((^.))
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import Waterfall.Internal.Edges (wireEndpoints, reverseWire)
import Control.Monad ((<=<))

-- | Class used to abstract over constructing `Path` and `Path2D` 
-- 
-- There are instances for @AnyPath (V3 Double) Path@
-- and for @AnyPath (V2 Double) Path2D@
class AnyPath point path | path -> point where
    fromWire :: Acquire (Ptr TopoDS.Wire) -> path
    toWire :: path -> Acquire (Ptr TopoDS.Wire)
    pointToGPPnt :: Proxy path -> point -> Acquire (Ptr GP.Pnt)
    v3ToPoint :: Proxy path -> V3 Double -> point 

edgesToPath :: (AnyPath point path) => Acquire [Ptr TopoDS.Edge] -> path
edgesToPath es = fromWire $ do
    edges <- es
    builder <- MakeWire.new
    liftIO $ traverse_ (MakeWire.addEdge builder) edges
    MakeWire.wire builder

-- | A straight line between two points
line :: forall point path. (AnyPath point path) => point -> point -> path
line start end = edgesToPath $ do
    pt1 <- pointToGPPnt (Proxy :: Proxy path) start
    pt2 <- pointToGPPnt (Proxy :: Proxy path) end
    pure <$> MakeEdge.fromPnts pt1 pt2

-- | Version of `line` designed to work with `pathFrom`
lineTo :: (AnyPath point path) => point -> point -> (point, path)
lineTo end = \start -> (end, line start end) 

-- | Version of `line` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the endpoint
-- relative to the start of the line, rather than in absolute space.
lineRelative :: (AnyPath point path, Num point) => point -> point -> (point, path)
lineRelative dEnd = do
    end <- (+ dEnd)
    lineTo end

-- | Section of a circle based on three arguments, the start point, a point on the arc, and the endpoint
arcVia :: forall point path. (AnyPath point path) => point -> point -> point -> path
arcVia start mid end = edgesToPath $ do
    s <- pointToGPPnt (Proxy :: Proxy path) start
    m <- pointToGPPnt (Proxy :: Proxy path) mid
    e <- pointToGPPnt (Proxy :: Proxy path) end
    theArc <- MakeArcOfCircle.from3Pnts s m e
    pure <$> MakeEdge.fromCurve (upcast theArc)

-- | Version of `arcVia` designed to work with `pathFrom`
--
-- The first argument is a point on the arc
-- The second argument is the endpoint of the arc
arcViaTo :: (AnyPath point path) => point -> point -> point -> (point, path)
arcViaTo mid end = \start -> (end, arcVia start mid end) 

-- | Version of `arcVia` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the midpoint and endpoint
-- relative to the start of the line, rather than in absolute space.
arcViaRelative :: (AnyPath point path, Num point) => point -> point -> point -> (point, path)
arcViaRelative dMid dEnd = do
    mid <- (+ dMid) 
    end <- (+ dEnd) 
    arcViaTo mid end

-- | Bezier curve of order 3
-- 
-- The arguments are, the start of the curve, the two control points, and the end of the curve
bezier :: forall point path. (AnyPath point path) => point -> point -> point -> point -> path
bezier start controlPoint1 controlPoint2 end = edgesToPath $ do
    s <- pointToGPPnt (Proxy :: Proxy path) start
    c1 <- pointToGPPnt (Proxy :: Proxy path) controlPoint1
    c2 <- pointToGPPnt (Proxy :: Proxy path) controlPoint2
    e <- pointToGPPnt (Proxy :: Proxy path) end
    arr <- NCollection.Array1.newGPPntArray 1 4
    liftIO $ do 
        NCollection.Array1.setValueGPPnt arr 1 s
        NCollection.Array1.setValueGPPnt arr 2 c1
        NCollection.Array1.setValueGPPnt arr 3 c2
        NCollection.Array1.setValueGPPnt arr 4 e
    b <- BezierCurve.toHandle =<< BezierCurve.fromPnts arr
    pure <$> MakeEdge.fromCurve (upcast b)

-- | Version of `bezier` designed to work with `pathFrom`
bezierTo :: (AnyPath point path) => point -> point -> point -> point -> (point, path)
bezierTo controlPoint1 controlPoint2 end = \start -> (end, bezier start controlPoint1 controlPoint2 end) 

-- | Version of `bezier` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the control points and the endpoint
-- relative to the start of the line, rather than in absolute space.
bezierRelative :: (AnyPath point path, Num point) => point -> point -> point -> point -> (point, path)
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
-- Path.pathFrom zero 
--    [ Path.bezierRelative (V3 0 0 0.5) (V3 0.5 0.5 0.5) (V3 0.5 0.5 1)
--    , Path.bezierRelative (V3 0 0 0.5) (V3 (-0.5) (-0.5) 0.5) (V3 (-0.5) (-0.5) 1)
--    , Path.arcViaRelative (V3 0 1 1) (V3 0 2 0)
--    , Path.lineTo (V3 0 2 0) 
--    ] @
pathFrom :: (Monoid path) => point -> [point -> (point, path)] -> path
pathFrom start commands = snd $ pathFromTo commands start 
     
-- | Combines a list of "path components", as used by `pathFrom`
pathFromTo :: (Monoid path) => [point -> (point, path)] -> point -> (point, path)
pathFromTo commands start = 
    let go (pos, paths) cmd = second (:paths) (cmd pos)
        (end, allPaths) = foldl' go (start, []) commands
     in (end, mconcat . reverse $ allPaths)

-- | Returns the start and end of a `Path`
pathEndpoints :: forall point path. (AnyPath point path) => path -> (point, point)
pathEndpoints path = unsafeFromAcquire $ do
    wire <- toWire path
    (s, e) <- liftIO $ wireEndpoints wire
    return (v3ToPoint (Proxy :: Proxy path) s, v3ToPoint (Proxy :: Proxy path) e)

-- | Convert a path into a function that can be used as an argument to `pathFrom`
--
-- Takes a path, and returns a function which takes a new start point for the path, and returns 
-- tupled, the path translated onto the new start point, and the new endpoint 
splice :: forall point path. (AnyPath point path, Num point) => path -> point -> (point, path)
splice path pnt = 
    let res = unsafeFromAcquire $ do
            wire <- toWire path
            (s, e) <- liftIO $ wireEndpoints wire
            let s' = v3ToPoint (Proxy :: Proxy path) s
                e' = v3ToPoint (Proxy :: Proxy path) e
            gp <- pointToGPPnt (Proxy :: Proxy path) pnt
            p <- liftIO $ gpPntToV3 gp
            let (V3 x y z) = p - s
            trsf <- GP.Trsf.new
            vec <- GP.Vec.new x y z
            liftIO $ GP.Trsf.setTranslation trsf vec
            oldWire <- toWire path
            newWire <- (liftIO . unsafeDowncast) =<< BRepBuilderAPI.Transform.transform (upcast oldWire) trsf True 
            return (pnt + e' - s', newWire)
    in (fst res, fromWire (fmap snd . toAcquire $ res))

-- | Given a path, return a new path with the endpoints joined by a straight line.
closeLoop :: (AnyPath point path, Monoid path, Epsilon point) => path -> path
closeLoop p = 
    let (s, e) = pathEndpoints p
     in if nearZero (s - e) 
            then p
            else p <> line e s

reversePath :: (AnyPath point path) => path -> path
reversePath = fromWire . (reverseWire <=< toWire)

instance AnyPath (V3 Double) Path where
    fromWire :: Acquire (Ptr TopoDS.Wire) -> Path
    fromWire = Path . unsafeFromAcquire
    pointToGPPnt :: Proxy Path -> V3 Double -> Acquire (Ptr GP.Pnt)
    pointToGPPnt _ (V3 x y z) = GP.Pnt.new x y z 
    toWire :: Path -> Acquire (Ptr TopoDS.Wire)
    toWire (Path ptr) = toAcquire ptr
    v3ToPoint :: Proxy Path -> V3 Double -> V3 Double
    v3ToPoint _ = id

instance AnyPath (V2 Double) Path2D where
    fromWire :: Acquire (Ptr TopoDS.Wire) -> Path2D
    fromWire = Path2D . unsafeFromAcquire
    pointToGPPnt :: Proxy Path2D -> V2 Double -> Acquire (Ptr GP.Pnt)
    pointToGPPnt _ (V2 x y) = GP.Pnt.new x y 0
    toWire :: Path2D -> Acquire (Ptr TopoDS.Wire)
    toWire (Path2D ptr) = toAcquire ptr
    v3ToPoint :: Proxy Path2D -> V3 Double -> V2 Double
    v3ToPoint _  = (^. _xy)
