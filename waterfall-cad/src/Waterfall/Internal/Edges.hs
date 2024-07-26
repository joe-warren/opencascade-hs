module Waterfall.Internal.Edges
( edgeEndpoints
, wireEndpoints
, wireTangent
, reverseEdge
, reverseWire
) where

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.Geom.Curve as Geom.Curve
import qualified OpenCascade.BRepTools.WireExplorer as WireExplorer
import qualified OpenCascade.ShapeExtend.WireData as WireData
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import Waterfall.Internal.FromOpenCascade (gpPntToV3, gpVecToV3)
import Data.Acquire
import Control.Monad.IO.Class (liftIO)
import Linear (V3 (..))
import Foreign.Ptr
import OpenCascade.Inheritance (unsafeDowncast, upcast)
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Control.Monad (when)


edgeEndpoints :: Ptr TopoDS.Edge -> IO (V3 Double, V3 Double)
edgeEndpoints edge = (`with` pure) $ do
    curve <- BRep.Tool.curve edge
    p1 <- liftIO . BRep.Tool.curveParamFirst $ edge
    p2 <- liftIO . BRep.Tool.curveParamLast $ edge
    s <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p1
    e <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p2
    return (s, e)

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
    wire' <- MakeWire.wire makeWire
    liftIO $ TopoDS.Shape.reverse (upcast wire')
    return wire'

{--
reverseWire :: Ptr TopoDS.Wire -> Acquire (Ptr TopoDS.Wire) 
reverseWire wire = do
    wire' <- liftIO . unsafeDowncast =<< TopoDS.Shape.copy (upcast wire)
    wireData <- WireData.fromWireChainedAndManifold wire' False True
    liftIO $ WireData.reverse wireData
    wire'' <- WireData.wire wireData
    --liftIO $ TopoDS.Shape.complement (upcast wire'')
    --liftIO $ TopoDS.Shape.reverse (upcast wire'')
    return wire''
--}