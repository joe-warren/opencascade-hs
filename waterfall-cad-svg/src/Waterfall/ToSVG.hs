{-|
Convert Waterfall data into [SVG](https://developer.mozilla.org/en-US/docs/Web/SVG)
-}
module Waterfall.ToSVG
( path2DToPathCommands
) where

import qualified Waterfall
import qualified Graphics.Svg as Svg
import Linear ( _xy)
import Control.Lens ((^.))
import Control.Monad ((<=<))
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
import qualified OpenCascade.Geom.BezierCurve as Geom.BezierCurve
import qualified OpenCascade.Geom.BSplineCurve as Geom.BSplineCurve
import qualified OpenCascade.GeomAbs.Shape as GeomAbs.Shape
import qualified OpenCascade.GeomConvert.BSplineCurveToBezierCurve as GeomConvert.BSplineCurveToBezierCurve
import qualified OpenCascade.GeomConvert.ApproxCurve as GeomConvert.ApproxCurve
import qualified OpenCascade.ShapeConstruct.Curve as ShapeConstruct.Curve
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
    bezier <- BRepAdaptor.Curve.bezier curve
    bezierCurveToPathCommand edge bezier

convertBSpline :: Ptr TopoDS.Edge -> Ptr (Handle Geom.BSplineCurve) -> Acquire [Svg.PathCommand]
convertBSpline edge someBSpline = do
    converter <- GeomConvert.BSplineCurveToBezierCurve.fromHandle someBSpline
    nbArcs <- liftIO $ GeomConvert.BSplineCurveToBezierCurve.nbArcs converter
    mconcat <$> traverse (bezierCurveToPathCommand edge <=< GeomConvert.BSplineCurveToBezierCurve.arc converter) [1..nbArcs]

approximateCurveToPathCommand :: Ptr TopoDS.Edge -> Ptr BRepAdaptor.Curve.Curve -> Acquire [Svg.PathCommand]
approximateCurveToPathCommand edge curve = do
    scc <- ShapeConstruct.Curve.new
    firstParam <- liftIO $ BRepAdaptor.Curve.firstParameter curve
    lastParam <- liftIO $ BRepAdaptor.Curve.lastParameter curve
    let convertToBSpline curve' = ShapeConstruct.Curve.convertToBSpline scc curve' firstParam lastParam 1e-6
    curve' <- fmap upcast . convertToBSpline =<< GeomAdaptor.Curve.curve =<< BRepAdaptor.Curve.curve curve
    approximator <- GeomConvert.ApproxCurve.fromCurveToleranceOrderSegmentsAndDegree curve' 0.05 GeomAbs.Shape.C0 50 3
    done <- liftIO $ GeomConvert.ApproxCurve.isDone approximator
    if done 
        then do
            newCurve <- GeomConvert.ApproxCurve.curve approximator
            convertBSpline edge newCurve
        else 
            liftIO $ discretizedEdgePathCommand edge

bsplineToPathCommand :: Ptr TopoDS.Edge -> Ptr BRepAdaptor.Curve.Curve -> Acquire [Svg.PathCommand]
bsplineToPathCommand edge curve = do 
    bspline <- BRepAdaptor.Curve.bspline curve
    isRational <- liftIO $ Geom.BSplineCurve.isRational bspline
    nbPoles <- liftIO $ Geom.BSplineCurve.nbPoles bspline
    if nbPoles > 4 || isRational
        then approximateCurveToPathCommand edge curve
        else convertBSpline edge bspline

discretizedEdgePathCommand :: Ptr TopoDS.Edge -> IO [Svg.PathCommand]
discretizedEdgePathCommand edge = do
    s <- Internal.Edges.edgeValue edge 0 
    ps <- traverse (Internal.Edges.edgeValue edge . (/10) .  fromIntegral) [1..(10::Integer)]
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
        GeomAbs.CurveType.BSplineCurve -> bsplineToPathCommand edge adaptor
        _ -> approximateCurveToPathCommand edge adaptor

path2DToPathCommands :: Waterfall.Path2D -> [Svg.PathCommand]
path2DToPathCommands (Path2D theRawPath) = case theRawPath of  
    Internal.Path.Common.EmptyRawPath -> []
    Internal.Path.Common.SinglePointRawPath _ -> []
    Internal.Path.Common.ComplexRawPath wire -> 
        Internal.Finalizers.unsafeFromAcquireT $
            foldMap edgeToPathCommand <$> Internal.Edges.wireEdges wire
