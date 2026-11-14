module Waterfall.Internal.Edges
( edgeEndpoints
, edgeValue
, wireEndpoints
, allSubShapesWithCopy
, allWireEndpoints
, allWires
, allEdges
, wireEdges
, wireTangentStart
, buildEdgeCurve3D
, reverseEdge
, reverseWire
, intersperseLines
, joinWires
, splitWires
, edgeToWire
, wireLength
, truncateWire
) where

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.Geom.Curve as Geom.Curve
import qualified OpenCascade.BRepTools.WireExplorer as WireExplorer
import qualified OpenCascade.TopExp.Explorer as Explorer 
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import qualified OpenCascade.BRepLib as BRepLib
import Waterfall.Internal.NearZero (nearZero)
import OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import Waterfall.Internal.FromOpenCascade (gpPntToV3, gpVecToV3)
import qualified OpenCascade.BRepAdaptor.Curve as BRepAdaptor.Curve
import qualified OpenCascade.GCPnts.AbscissaPoint as AbscissaPoint
import qualified OpenCascade.GProp.GProps as GProps
import qualified OpenCascade.BRepGProp as BRepGProp
import Data.Acquire
import Control.Monad.IO.Class (liftIO)
import Linear (V3 (..), distance, normalize)
import Foreign.Ptr
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import Control.Monad (when, (<=<))
import Waterfall.Internal.ToOpenCascade (v3ToPnt)
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import qualified Waterfall.Internal.ShapeMap as ShapeMap
import qualified OpenCascade.TopAbs.Orientation as TopAbs.Orientation
import qualified OpenCascade.TopExp as TopExp

edgeEndpoints :: Ptr TopoDS.Edge -> IO (V3 Double, V3 Double)
edgeEndpoints edge = (`with` pure) $ do
    curve <- BRep.Tool.curve edge
    p1 <- liftIO . BRep.Tool.curveParamFirst $ edge
    p2 <- liftIO . BRep.Tool.curveParamLast $ edge
    s <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p1
    e <- (liftIO . gpPntToV3) =<< Geom.Curve.value curve p2
    isForward <- liftIO $ edgeIsForward edge
    if isForward
        then return (s, e)
        else return (e, s)

edgeValue :: Ptr TopoDS.Edge -> Double -> IO (V3 Double)
edgeValue edge v = (`with` pure) $ do
    curve <- BRep.Tool.curve edge
    p1 <- liftIO . BRep.Tool.curveParamFirst $ edge
    p2 <- liftIO . BRep.Tool.curveParamLast $ edge
    isForward <- liftIO $ edgeIsForward edge
    let v' = if isForward then v else (1-v)
    let p' = (1-v') * p1 + v' * p2
    (liftIO . gpPntToV3) =<< Geom.Curve.value curve p'

allWireEndpoints :: Ptr TopoDS.Wire -> IO [(V3 Double, V3 Double)]
allWireEndpoints wire = with (WireExplorer.fromWire =<< forwardWire wire) $ \explorer -> do
    let runToEnd = do
            more <- WireExplorer.more explorer
            if more then do
                edge <- WireExplorer.current explorer
                points <- edgeEndpoints edge
                WireExplorer.next explorer
                (points:) <$> runToEnd
            else pure []
    runToEnd

allSubShapesWithCopy :: ShapeEnum.ShapeEnum -> Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Shape]
allSubShapesWithCopy t s = do 
    explorer <- Explorer.new s t
    let go acc = do
            isMore <- liftIO $ Explorer.more explorer
            if isMore 
                then do
                    v <- liftIO $ Explorer.value explorer
                    isPresent <- liftIO $ ShapeMap.lookup acc v
                    (newAcc, add) <- case isPresent of 
                        Nothing -> do 
                            v' <- TopoDS.Shape.copy v
                            newMap <- liftIO $ ShapeMap.addIfAbsent (v', ()) acc
                            return (newMap, (v':))
                        Just _ -> pure (acc, id) 
                    liftIO $ Explorer.next explorer
                    add <$> go newAcc
                else return []
    go ShapeMap.empty

allEdges :: Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Edge]
allEdges s = traverse (liftIO . unsafeDowncast) =<< allSubShapesWithCopy ShapeEnum.Edge s 

allWires :: Ptr TopoDS.Shape -> Acquire [Ptr TopoDS.Wire]
allWires s = traverse (forwardWire <=< liftIO . unsafeDowncast) =<< allSubShapesWithCopy ShapeEnum.Wire s

wireFromEdges :: [Ptr TopoDS.Edge] -> Acquire (Ptr TopoDS.Wire)
wireFromEdges edges = do
    builder <- MakeWire.new
    let addAll [] = pure ()
        addAll (e:es) = do
                MakeWire.addEdge builder e
                done <- MakeWire.isDone builder
                when done (addAll es)
    liftIO $ addAll edges
    MakeWire.wire builder

forwardWire :: Ptr TopoDS.Wire -> Acquire (Ptr TopoDS.Wire)
forwardWire wire = do
    edges <- wireEdgesWithoutReversing wire
    allForward <- and <$> liftIO (traverse edgeIsForward edges)
    if allForward 
        then return wire
        else do
            wireFromEdges =<< wireEdges wire

edgeIsForward :: Ptr TopoDS.Edge -> IO Bool
edgeIsForward = 
    fmap (== TopAbs.Orientation.Forward) . TopoDS.Shape.orientation . upcast

wireEndpoints :: Ptr TopoDS.Wire -> IO (V3 Double, V3 Double)
wireEndpoints wire = with (WireExplorer.fromWire =<< forwardWire wire) $ \explorer -> do
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

edgeTangentStart :: Ptr TopoDS.Edge -> IO (V3 Double)
edgeTangentStart e = (`with` pure) $ do
    curve <- BRep.Tool.curve e
    isForward <- liftIO $ edgeIsForward e
    p1 <- if isForward
        then liftIO . BRep.Tool.curveParamFirst $ e
        else liftIO . BRep.Tool.curveParamLast $ e
    let reverseTangent =
         if isForward
            then id
            else negate   
    reverseTangent <$> (liftIO . gpVecToV3 =<< Geom.Curve.dn curve p1 1)


edgeTangentEnd :: Ptr TopoDS.Edge -> IO (V3 Double)
edgeTangentEnd e = (`with` pure) $ do
    curve <- BRep.Tool.curve e
    isForward <- liftIO $ edgeIsForward e
    p1 <- if isForward
        then liftIO . BRep.Tool.curveParamLast $ e
        else liftIO . BRep.Tool.curveParamFirst $ e
    let reverseTangent =
         if isForward
            then id
            else negate   
    reverseTangent <$> (liftIO . gpVecToV3 =<< Geom.Curve.dn curve p1 1)

wireTangentStart :: Ptr TopoDS.Wire -> IO (V3 Double)
wireTangentStart wire = with (WireExplorer.fromWire wire) $ \explorer -> do
    v1 <- WireExplorer.current explorer
    edgeTangentStart v1

forwardEdge :: Ptr TopoDS.Edge -> Acquire (Ptr TopoDS.Edge)
forwardEdge e = do
    isForward <- liftIO $ edgeIsForward e
    if isForward 
        then return e
        else do
            curve <- BRep.Tool.curve e 
            firstP <- liftIO $ BRep.Tool.curveParamFirst e
            lastP <- liftIO $ BRep.Tool.curveParamLast e
            firstP' <- liftIO $ Geom.Curve.reversedParameter curve firstP
            lastP' <- liftIO $ Geom.Curve.reversedParameter curve lastP
            firstVertex <- TopExp.firstVertex e False
            lastVertex <- TopExp.lastVertex e False
            curve' <- Geom.Curve.reversed curve
            MakeEdge.fromCurveVerticesAndParameters curve' lastVertex firstVertex lastP' firstP' 

reverseEdge :: Ptr TopoDS.Edge -> Acquire (Ptr TopoDS.Edge)
reverseEdge e = do
    curve <- BRep.Tool.curve e 
    firstP <- liftIO $ BRep.Tool.curveParamFirst e
    lastP <- liftIO $ BRep.Tool.curveParamLast e
    firstVertex <- TopExp.firstVertex e True
    lastVertex <- TopExp.lastVertex e True
    isForward <- liftIO $ edgeIsForward e
    if not isForward 
        then do
            MakeEdge.fromCurveVerticesAndParameters curve lastVertex firstVertex firstP lastP
        else do
            firstP' <- liftIO $ Geom.Curve.reversedParameter curve firstP
            lastP' <- liftIO $ Geom.Curve.reversedParameter curve lastP
            curve' <- Geom.Curve.reversed curve
            MakeEdge.fromCurveVerticesAndParameters curve' lastVertex firstVertex lastP' firstP' 

wireEdgesWithoutReversing :: Ptr TopoDS.Wire -> Acquire [Ptr TopoDS.Edge]
wireEdgesWithoutReversing wire = do
    explorer <- WireExplorer.fromWire wire
    let runToEnd = do
            more <- liftIO $ WireExplorer.more explorer
            if not more
                then pure []
                else do
                    edge <- (liftIO . unsafeDowncast)
                        =<< TopoDS.Shape.copy . upcast
                        =<< (liftIO $ WireExplorer.current explorer)

                    liftIO $ WireExplorer.next explorer
                    (edge :) <$> runToEnd
    runToEnd

wireEdges :: Ptr TopoDS.Wire -> Acquire [Ptr TopoDS.Edge]
wireEdges wire = do
    explorer <- WireExplorer.fromWire wire
    let runToEnd = do
            more <- liftIO $ WireExplorer.more explorer
            if not more
                then pure []
                else do
                    edge <- forwardEdge
                        =<< (liftIO . unsafeDowncast) 
                        =<< TopoDS.Shape.copy . upcast 
                        =<< (liftIO $ WireExplorer.current explorer)
                    liftIO $ WireExplorer.next explorer
                    (edge :) <$> runToEnd
    runToEnd

reverseWire :: Ptr TopoDS.Wire -> Acquire (Ptr TopoDS.Wire) 
reverseWire wire = do
    wireFromEdges
    =<< fmap reverse . traverse reverseEdge
    =<< wireEdges wire 

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
joinWires wires =
    wireFromEdges
        =<< (concat <$> traverse wireEdges wires)

edgeToWire :: Ptr TopoDS.Edge -> Acquire (Ptr TopoDS.Wire)
edgeToWire edge = do
    builder <- MakeWire.new
    liftIO $ MakeWire.addEdge builder edge
    MakeWire.wire builder

splitWires :: Ptr TopoDS.Wire -> Acquire [Ptr TopoDS.Wire]
splitWires wire = do
    edges <- wireEdges $ wire
    let pairWithTangents edge = do
            s' <- normalize <$> edgeTangentStart edge
            e' <- normalize <$> edgeTangentEnd edge
            return (edge, s', e')
    edgesWithTangents <- liftIO $ traverse pairWithTangents edges

    let shouldPair (_, _, e') (_, s', _) = all nearZero (s' - e')
    let getEdgeFromPair (e, _, _) = e
    let groupEdgesBy _f [] = []
        groupEdgesBy _f (e:[]) = [[e]]
        groupEdgesBy f (a:b:rest) = 
            if f a b 
                then case groupEdgesBy f (b:rest) of 
                        (h:t) -> (a:h):t
                        [] -> [[a]]
                else [a] : groupEdgesBy f (b:rest)

    traverse wireFromEdges (fmap getEdgeFromPair <$> groupEdgesBy shouldPair edgesWithTangents)


buildEdgeCurve3D :: Ptr TopoDS.Edge -> Acquire (Ptr TopoDS.Edge)
buildEdgeCurve3D edge = do
    edge' <- (liftIO . unsafeDowncast) =<< TopoDS.Shape.copy (upcast edge)
    _ <- liftIO $ BRepLib.buildCurve3d edge' 1e-5 GeomAbs.Shape.C1 14 0
    return edge'

shapeLength :: Ptr TopoDS.Shape -> IO Double
shapeLength shape = (`with` pure) $ do
    gProp <- GProps.new
    liftIO $ BRepGProp.linearProperties shape gProp False False
    liftIO $ GProps.mass gProp

edgeLength :: Ptr TopoDS.Edge -> IO Double
edgeLength = shapeLength . upcast

wireLength :: Ptr TopoDS.Wire -> IO Double
wireLength = shapeLength . upcast

truncateWire :: Double -> Ptr TopoDS.Wire -> Acquire (Ptr TopoDS.Wire)
truncateWire targetLength wire = do
    explorer <- WireExplorer.fromWire =<< forwardWire wire
    builder <- MakeWire.new
    let go remaining = do
            more <- liftIO $ WireExplorer.more explorer
            when (more && not (nearZero remaining)) $ do
                edge <- liftIO $ WireExplorer.current explorer
                len <- liftIO $ edgeLength edge
                if len <= remaining
                    then do
                        liftIO $ MakeWire.addEdge builder edge
                        liftIO $ WireExplorer.next explorer
                        go (remaining - len)
                    else do
                        curve <- BRep.Tool.curve edge
                        firstP <- liftIO $ BRep.Tool.curveParamFirst edge
                        adaptor <- BRepAdaptor.Curve.fromEdge edge
                        absPnt <- AbscissaPoint.fromCurveAbscissaAndParam adaptor remaining firstP
                        cutParam <- liftIO $ AbscissaPoint.parameter absPnt
                        trimmedEdge <- MakeEdge.fromCurveAndParameters curve firstP cutParam
                        liftIO $ MakeWire.addEdge builder trimmedEdge
    go targetLength
    MakeWire.wire builder

