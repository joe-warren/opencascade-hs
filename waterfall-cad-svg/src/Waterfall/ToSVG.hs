{-|
Convert Waterfall data into [SVG](https://developer.mozilla.org/en-US/docs/Web/SVG)
-}
module Waterfall.ToSVG
( path2DToPathCommands
) where

import qualified Waterfall
import qualified Graphics.Svg as Svg
import Linear ( _xy, V2 (..), nearZero)
import Control.Lens ((^.))
import Control.Monad ((<=<), unless, when)
import Foreign.Ptr (Ptr)
import Control.Monad.IO.Class (liftIO)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import qualified Waterfall.Internal.Path.Common as Internal.Path.Common
import qualified Waterfall.Internal.Edges as Internal.Edges
import qualified Waterfall.Internal.Finalizers as Internal.Finalizers
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.BRepAdaptor.Curve as BRepAdaptor.Curve
import qualified OpenCascade.Geom as Geom
import qualified OpenCascade.GeomAbs.CurveType as GeomAbs.CurveType
import qualified OpenCascade.Geom.Curve as Geom.Curve
import qualified OpenCascade.Geom.BezierCurve as Geom.BezierCurve
import qualified OpenCascade.Geom.BSplineCurve as Geom.BSplineCurve
import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import qualified OpenCascade.GeomConvert.BSplineCurveToBezierCurve as GeomConvert.BSplineCurveToBezierCurve
import qualified OpenCascade.GeomConvert.ApproxCurve as GeomConvert.ApproxCurve
import qualified OpenCascade.ShapeConstruct.Curve as ShapeConstruct.Curve
import qualified OpenCascade.BRep.Tool as BRep.Tool
import OpenCascade.Handle (Handle)
import OpenCascade.Inheritance (upcast)
import Data.Acquire (Acquire)
import qualified OpenCascade.GeomAdaptor.Curve as GeomAdaptor.Curve

lineToPathCommand :: Ptr TopoDS.Edge -> IO [Svg.PathCommand]
lineToPathCommand edge = do
    (s, e) <- Internal.Edges.edgeEndpoints edge
    return 
        [ Svg.MoveTo Svg.OriginAbsolute . pure $ s ^. _xy
        , Svg.LineTo Svg.OriginAbsolute . pure $ e ^. _xy
        ]

bezierCurveToPathCommand :: Ptr TopoDS.Edge -> Ptr (Handle Geom.BezierCurve) -> Acquire [Svg.PathCommand]
bezierCurveToPathCommand edge bezier = do
    isRational <- liftIO $ Geom.BezierCurve.isRational bezier
    nbPoles <- liftIO $ Geom.BezierCurve.nbPoles bezier
    if nbPoles > 4 || isRational
        then liftIO $ discretizedEdgePathCommand edge
        else do 
            poles <- traverse ((liftIO . gpPntToV3) <=< Geom.BezierCurve.pole bezier) [1..nbPoles]
            case poles of 
                [s, e] -> return
                        [ Svg.MoveTo Svg.OriginAbsolute . pure $ s ^. _xy
                        , Svg.LineTo Svg.OriginAbsolute . pure $ e ^. _xy
                        ]
                [s, cp, e] -> return
                        [ Svg.MoveTo Svg.OriginAbsolute . pure $ s ^. _xy
                        , Svg.QuadraticBezier Svg.OriginAbsolute . pure $ (cp ^. _xy, e ^. _xy)
                        ]
                [s, cp1, cp2, e] -> return
                        [ Svg.MoveTo Svg.OriginAbsolute . pure $ s ^. _xy
                        , Svg.CurveTo Svg.OriginAbsolute . pure $ (cp1 ^. _xy, cp2 ^. _xy, e ^. _xy)
                        ]
                _ -> liftIO $ discretizedEdgePathCommand edge

bezierToPathCommand :: Ptr TopoDS.Edge -> Ptr BRepAdaptor.Curve.Curve -> Acquire [Svg.PathCommand]
bezierToPathCommand edge curve = do 
    firstParam <- liftIO $ BRep.Tool.curveParamFirst edge
    lastParam <- liftIO $ BRep.Tool.curveParamLast edge
    bezier <- BRepAdaptor.Curve.bezier curve
    liftIO $ Geom.BezierCurve.segment bezier firstParam lastParam 
    bezierCurveToPathCommand edge bezier

convertBSpline :: Ptr TopoDS.Edge -> Ptr (Handle Geom.BSplineCurve) -> Acquire [Svg.PathCommand]
convertBSpline edge someBSpline = do
    firstParameter <- liftIO $ Geom.Curve.firstParameter (upcast someBSpline)
    lastParameter <- liftIO $ Geom.Curve.lastParameter (upcast someBSpline)
    converter <- GeomConvert.BSplineCurveToBezierCurve.fromHandle someBSpline -- ParametersAndTolerance someBSpline firstParameter lastParameter 1e-3
    nbArcs <- liftIO $ GeomConvert.BSplineCurveToBezierCurve.nbArcs converter
    mconcat <$> traverse (bezierCurveToPathCommand edge <=< GeomConvert.BSplineCurveToBezierCurve.arc converter) [1 .. nbArcs]

approximateCurveToPathCommand :: Ptr TopoDS.Edge -> Acquire [Svg.PathCommand]
approximateCurveToPathCommand edge = do
    gc <- BRep.Tool.curve edge
    firstParam <- liftIO $ BRep.Tool.curveParamFirst edge
    lastParam <- liftIO $ BRep.Tool.curveParamLast edge
    scc <- ShapeConstruct.Curve.new
    curve <- ShapeConstruct.Curve.convertToBSpline scc gc firstParam lastParam 1e-3
    preciseBSplineToPathCommand edge curve 

preciseBSplineToPathCommand :: Ptr TopoDS.Edge -> Ptr (Handle Geom.BSplineCurve)-> Acquire [Svg.PathCommand]
preciseBSplineToPathCommand edge curve = do
    nbPoles <- liftIO $ Geom.BSplineCurve.nbPoles curve
    isRational <- liftIO $ Geom.BSplineCurve.isRational curve
    let needsApproximating = (nbPoles > 4 || isRational)
    if not needsApproximating
        then convertBSpline edge curve
        else do
            approximator <- GeomConvert.ApproxCurve.fromCurveToleranceOrderSegmentsAndDegree (upcast curve) 1e-3 GeomAbs.Shape.C0 100 3
            done <- liftIO $ GeomConvert.ApproxCurve.isDone approximator
            if done
                then do
                    newCurve <- GeomConvert.ApproxCurve.curve approximator
                    convertBSpline edge newCurve
                else 
                    liftIO $ discretizedEdgePathCommand edge
    
bsplineToPathCommand :: Ptr TopoDS.Edge -> Ptr BRepAdaptor.Curve.Curve -> Acquire [Svg.PathCommand]
bsplineToPathCommand edge curve = return [] {--do 
    firstParamEdge <- liftIO $ BRep.Tool.curveParamFirst edge
    lastParamEdge <- liftIO $ BRep.Tool.curveParamLast edge
    bc <- BRepAdaptor.Curve.bspline curve
    firstParamCurve <- liftIO $ Geom.Curve.firstParameter (upcast bc)
    lastParamCurve <- liftIO $ Geom.Curve.lastParameter (upcast bc)
    let curveAlreadyFine = (nearZero (V2 firstParamCurve lastParamCurve - V2 firstParamEdge lastParamEdge)) 
    unless curveAlreadyFine (liftIO $ Geom.BSplineCurve.segment bc firstParamEdge lastParamEdge 1e-5)
    when curveAlreadyFine (liftIO $ print "curve already fine")
    if curveAlreadyFine 
        then discretizedCurvePathCommand firstParamEdge lastParamEdge (upcast bc)
        else preciseBSplineToPathCommand edge bc --}

discretizedEdgePathCommand :: Ptr TopoDS.Edge -> IO [Svg.PathCommand]
discretizedEdgePathCommand edge = do
    s <- Internal.Edges.edgeValue edge 0 
    ps <- traverse (Internal.Edges.edgeValue edge . (/10) .  fromIntegral) [1..(10::Integer)]
    return 
        [ Svg.MoveTo Svg.OriginAbsolute . pure $ s ^. _xy
        , Svg.LineTo Svg.OriginAbsolute $ (^. _xy) <$> ps
        ]

discretizedCurvePathCommand' ::  Ptr (Handle Geom.Curve) -> Acquire [Svg.PathCommand]
discretizedCurvePathCommand' curve = do
    f <- liftIO $ Geom.Curve.firstParameter curve
    l <- liftIO $ Geom.Curve.lastParameter curve
    discretizedCurvePathCommand f l curve

discretizedCurvePathCommand :: Double -> Double -> Ptr (Handle Geom.Curve) -> Acquire [Svg.PathCommand]
discretizedCurvePathCommand firstParameter lastParameter curve = do
    let value = (liftIO . gpPntToV3) <=< Geom.Curve.value curve  
    s <- value firstParameter
    let f x = firstParameter * (1-x) + lastParameter * x
    ps <- traverse (value . f . (/10) .  fromIntegral) [1..(10::Integer)]
    return 
        [ Svg.MoveTo Svg.OriginAbsolute . pure $ s ^. _xy
        , Svg.LineTo Svg.OriginAbsolute $ (^. _xy) <$> ps
        ]
        
edgeToPathCommand :: Ptr TopoDS.Edge -> [Svg.PathCommand]
edgeToPathCommand edge = Internal.Finalizers.unsafeFromAcquire $ do
    adaptor <- BRepAdaptor.Curve.fromEdge edge
    curveType <- liftIO $ BRepAdaptor.Curve.curveType adaptor
    case curveType of 
        GeomAbs.CurveType.Line -> liftIO $ lineToPathCommand edge
        GeomAbs.CurveType.BezierCurve -> bezierToPathCommand edge adaptor
        -- GeomAbs.CurveType.BSplineCurve -> return [] -- bsplineToPathCommand edge adaptor
        _ -> approximateCurveToPathCommand edge

path2DToPathCommands :: Waterfall.Path2D -> [Svg.PathCommand]
path2DToPathCommands (Path2D theRawPath) = case theRawPath of  
    Internal.Path.Common.EmptyRawPath -> []
    Internal.Path.Common.SinglePointRawPath _ -> []
    Internal.Path.Common.ComplexRawPath wire -> 
        Internal.Finalizers.unsafeFromAcquireT $
            foldMap edgeToPathCommand <$> Internal.Edges.wireEdges wire
