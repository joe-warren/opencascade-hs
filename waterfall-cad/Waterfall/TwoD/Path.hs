module Waterfall.TwoD.Path 
( Path
, line
, lineTo
, arc
, arcTo
, bezier
, bezierTo
, pathFrom
) where 

import Waterfall.TwoD.Internal.Path (Path(..), joinPaths)
import Control.Arrow (second)
import Data.Foldable (traverse_, foldl')
import Linear.V2 (V2(..))
import Control.Monad.IO.Class (liftIO)
import qualified Linear.V2 as V2
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt 
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GC.MakeArcOfCircle as MakeArcOfCircle
import qualified OpenCascade.Geom as Geom
import qualified OpenCascade.NCollection.Array1 as NCollection.Array1
import qualified OpenCascade.Geom.BezierCurve as BezierCurve
import OpenCascade.Inheritance (upcast)
import Foreign.Ptr
import Data.Acquire

v2ToPnt :: V2 Double -> Acquire (Ptr GP.Pnt)
v2ToPnt (V2 x y) = GP.Pnt.new x y 0

edgesToPath :: Acquire [Ptr TopoDS.Edge] -> Path 
edgesToPath es = Path $ do
    edges <- es
    builder <- MakeWire.new
    liftIO $ traverse_ (MakeWire.addEdge builder) edges
    MakeWire.wire builder

line :: V2 Double -> V2 Double -> Path
line start end = edgesToPath $ do
    pt1 <- v2ToPnt start
    pt2 <- v2ToPnt end
    pure <$> MakeEdge.fromPnts pt1 pt2

lineTo :: V2 Double -> V2 Double -> (V2 Double, Path)
lineTo end = \start -> (end, line start end) 

arc :: V2 Double -> V2 Double -> V2 Double -> Path
arc start mid end = edgesToPath $ do
    s <- v2ToPnt start
    m <- v2ToPnt mid
    e <- v2ToPnt end
    arc <- MakeArcOfCircle.from3Pnts s m e
    pure <$> MakeEdge.fromCurve (upcast arc)

arcTo :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path)
arcTo mid end = \start -> (end, arc start mid end) 

bezier :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Path
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

    
bezierTo :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path)
bezierTo controlPoint1 controlPoint2 end = \start -> (end, bezier start controlPoint1 controlPoint2 end) 

pathFrom :: V2 Double -> [(V2 Double -> (V2 Double, Path))] -> Path
pathFrom start commands = 
    let go (pos, paths) cmd = second (:paths) (cmd pos)
        (_, allPaths) = foldl' go (start, []) commands
     in joinPaths allPaths


