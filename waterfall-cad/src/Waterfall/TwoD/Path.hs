module Waterfall.TwoD.Path 
( Path
, Sense (..)
, line
, lineTo
, arc
, arcTo
, arcVia
, arcViaTo
, bezier
, bezierTo
, pathFrom
, pathFromTo
) where 

import Waterfall.TwoD.Internal.Path (Path(..), joinPaths)
import Control.Arrow (second)
import Data.Foldable (traverse_, foldl')
import Linear.V2 (V2(..))
import Control.Monad.IO.Class (liftIO)
import qualified Linear.V2 as V2
import Linear ((^*), distance, normalize)
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

arcVia :: V2 Double -> V2 Double -> V2 Double -> Path
arcVia start mid end = edgesToPath $ do
    s <- v2ToPnt start
    m <- v2ToPnt mid
    e <- v2ToPnt end
    theArc <- MakeArcOfCircle.from3Pnts s m e
    pure <$> MakeEdge.fromCurve (upcast theArc)

arcViaTo :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path)
arcViaTo mid end = \start -> (end, arcVia start mid end) 

data Sense = Clockwise | Counterclockwise deriving (Eq, Show)

arc :: Sense -> Double -> V2 Double -> V2 Double -> Path 
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

arcTo :: Sense -> Double -> V2 Double -> V2 Double -> (V2 Double, Path)
arcTo sense radius end = \start -> (end, arc sense radius start end) 

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
pathFrom start commands = snd $ pathFromTo commands start 

     
pathFromTo :: [(V2 Double -> (V2 Double, Path))] -> V2 Double -> (V2 Double, Path)
pathFromTo commands start = 
    let go (pos, paths) cmd = second (:paths) (cmd pos)
        (end, allPaths) = foldl' go (start, []) commands
     in (end, joinPaths allPaths)




