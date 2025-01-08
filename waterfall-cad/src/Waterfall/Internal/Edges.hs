module Waterfall.Internal.Edges
( edgeEndpoints
, wireEndpoints
, allWireEndpoints
, allWires
, wireTangent
, reverseEdge
, reverseWire
, intersperseLines
, joinWires
) where

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.Geom.Curve as Geom.Curve
import qualified OpenCascade.BRepTools.WireExplorer as WireExplorer
import qualified OpenCascade.TopExp.Explorer as Explorer 
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopTools.ShapeMapHasher as TopTools.ShapeMapHasher
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import Waterfall.Internal.FromOpenCascade (gpPntToV3, gpVecToV3)
import Data.Acquire
import Control.Monad.IO.Class (liftIO)
import Linear (V3 (..), distance)
import Foreign.Ptr
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Control.Monad (when)
import Waterfall.Internal.ToOpenCascade (v3ToPnt)
import Data.Foldable (traverse_)
import OpenCascade.Inheritance (unsafeDowncast)

edgeEndpoints :: Ptr TopoDS.Edge -> IO (V3 Double, V3 Double)
edgeEndpoints edge = (`with` pure) $ do
    curve <- BRep.Tool.curve edge
    p1 <- liftIO . BRep.Tool.curveParamFirst $ edge
    p2 <- liftIO . BRep.Tool.curveParamLast $ edge
    s <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p1
    e <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p2
    return (s, e)

allWireEndpoints :: Ptr TopoDS.Wire -> IO [(V3 Double, V3 Double)]
allWireEndpoints wire = with (WireExplorer.fromWire wire) $ \explorer -> do
    let runToEnd = do
            edge <- WireExplorer.current explorer
            points <- edgeEndpoints edge
            WireExplorer.next explorer
            more <- WireExplorer.more explorer
            if more 
                then (points:) <$> runToEnd
                else pure [points]
    runToEnd

allSubShapes :: ShapeEnum.ShapeEnum -> Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Shape]
allSubShapes t s = do 
    explorer <- Explorer.new s t
    let go visited = do
            isMore <- liftIO $ Explorer.more explorer
            if isMore 
                then do
                    v <- liftIO $ Explorer.value explorer
                    hash <- liftIO $ TopTools.ShapeMapHasher.hash v
                    let add = if hash `elem` visited then id else (v:) 
                    liftIO $ Explorer.next explorer
                    add <$> go visited
                else return []
    go []

allWires :: Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Wire]
allWires s = traverse (liftIO . unsafeDowncast) =<<  allSubShapes ShapeEnum.Wire s 
    
wireEndpoints :: Ptr TopoDS.Wire -> IO (V3 Double, V3 Double)
wireEndpoints wire = with (WireExplorer.fromWire wire) $ \explorer -> do
    v1 <- WireExplorer.current explorer
    (s, _) <- edgeEndpoints v1
    let runToEnd = do
            edge <- WireExplorer.current explorer
            (_s, e') <- edgeEndpoints edge
            WireExplorer.next explorer
            more <- WireExplorer.more explorer
            if more 
                then runToEnd
                else pure e'
    e <- runToEnd
    return (s, e)

edgeTangent :: Ptr TopoDS.Edge -> IO (V3 Double)
edgeTangent e = (`with` pure) $ do
    curve <- BRep.Tool.curve e
    p1 <- liftIO . BRep.Tool.curveParamFirst $ e
    liftIO . gpVecToV3 =<< Geom.Curve.dn curve p1 1

wireTangent :: Ptr TopoDS.Wire -> IO (V3 Double)
wireTangent wire = with (WireExplorer.fromWire wire) $ \explorer -> do
    v1 <- WireExplorer.current explorer
    edgeTangent v1

reverseEdge :: Ptr TopoDS.Edge -> Acquire (Ptr TopoDS.Edge)
reverseEdge e = do
    curve <- BRep.Tool.curve e 
    firstP <- liftIO $ BRep.Tool.curveParamFirst e
    lastP <- liftIO $ BRep.Tool.curveParamLast e
    firstP' <- liftIO $ Geom.Curve.reversedParameter curve firstP
    lastP' <- liftIO $ Geom.Curve.reversedParameter curve lastP
    curve' <- Geom.Curve.reversed curve
    MakeEdge.fromCurveAndParameters curve' lastP' firstP' 

reverseWire :: Ptr TopoDS.Wire -> Acquire (Ptr TopoDS.Wire) 
reverseWire wire = do
    explorer <- WireExplorer.fromWire wire
    makeWire <- MakeWire.new
    let runToEnd = do
            edge <- liftIO $ WireExplorer.current explorer
            edge' <- reverseEdge edge
            liftIO $ WireExplorer.next explorer
            more <- liftIO $ WireExplorer.more explorer
            when more runToEnd
            liftIO $ MakeWire.addEdge makeWire edge'
    runToEnd
    MakeWire.wire makeWire

line' :: V3 Double -> V3 Double -> Acquire (Ptr TopoDS.Wire)
line' s e = do
    builder <- MakeWire.new
    pt1 <- v3ToPnt s
    pt2 <- v3ToPnt e
    edge <- MakeEdge.fromPnts pt1 pt2
    liftIO $ MakeWire.addEdge builder edge
    MakeWire.wire builder
    
intersperseLines :: [Ptr TopoDS.Wire] -> Acquire [Ptr TopoDS.Wire]
intersperseLines [] = pure []
intersperseLines [x] = pure [x]
intersperseLines (a:b:xs) = do
    (_, ea) <- liftIO $ wireEndpoints a
    (sb, _) <- liftIO $ wireEndpoints b
    if distance ea sb < 1e-6
            then (a :) <$> intersperseLines (b:xs)
            else (a :) <$> ((:) <$> line' ea sb <*> intersperseLines (b:xs))

joinWires :: [Ptr TopoDS.Wire] -> Acquire (Ptr TopoDS.Wire)
joinWires wires = do
    builder <- MakeWire.new
    let addWire wire = do 
            explorer <- WireExplorer.fromWire wire
            let runToEnd = do
                    edge <- liftIO $ WireExplorer.current explorer
                    liftIO $ MakeWire.addEdge builder edge
                    liftIO $ WireExplorer.next explorer
                    more <- liftIO $ WireExplorer.more explorer
                    when more runToEnd
            runToEnd
    traverse_ addWire $ wires
    MakeWire.wire builder

