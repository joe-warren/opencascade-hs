{-# Language OverloadedStrings #-}
{-|
Convert "Waterfall" data into [SVG](https://developer.mozilla.org/en-US/docs/Web/SVG)
-}
module Waterfall.SVG.ToSVG
( path2DToPathCommands
, diagramToSvg
, writeDiagramSVG
) where

import qualified Waterfall
import qualified Graphics.Svg as Svg
import qualified Graphics.Svg.CssTypes as Svg.Css
import Linear (_xy, _x, _y, V2 (..), nearZero)
import Control.Lens ((^.), (&), (.~))
import Foreign.Ptr (Ptr)
import Control.Monad ((<=<)) 
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
import qualified OpenCascade.BRep.Tool as BRep.Tool
import OpenCascade.Handle (Handle)
import OpenCascade.Inheritance (upcast)
import Data.Acquire (Acquire)
import Codec.Picture.Types (PixelRGBA8 (..))

lineToPathCommand :: Ptr TopoDS.Edge -> IO [Svg.PathCommand]
lineToPathCommand edge = do
    (_s, e) <- Internal.Edges.edgeEndpoints edge
    return 
        [ Svg.LineTo Svg.OriginAbsolute . pure $ e ^. _xy
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
                [_s, e] -> return . pure .
                        Svg.LineTo Svg.OriginAbsolute . pure $ e ^. _xy
                [_s, cp, e] -> return . pure .
                        Svg.QuadraticBezier Svg.OriginAbsolute . pure $ (cp ^. _xy, e ^. _xy)
                [_s, cp1, cp2, e] -> return . pure .
                        Svg.CurveTo Svg.OriginAbsolute . pure $ (cp1 ^. _xy, cp2 ^. _xy, e ^. _xy)
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
    
discretizedEdgePathCommand :: Ptr TopoDS.Edge -> IO [Svg.PathCommand]
discretizedEdgePathCommand edge = do
    ps <- traverse (Internal.Edges.edgeValue edge . (/10) .  fromIntegral) [1..(10::Integer)]
    return . pure .
        Svg.LineTo Svg.OriginAbsolute $ (^. _xy) <$> ps

edgeToPathCommand :: Maybe (V2 Double) -> Ptr TopoDS.Edge -> (Maybe (V2 Double), [Svg.PathCommand])
edgeToPathCommand curPos edge = Internal.Finalizers.unsafeFromAcquire $ do
    startPos <- liftIO $ (^. _xy) <$> Internal.Edges.edgeValue edge 0
    endPos <- liftIO $ (^. _xy) <$> Internal.Edges.edgeValue edge 1
    let hasntMoved = nearZero . (startPos -) <$> curPos
    let addMoveCommand = 
            case hasntMoved of 
                Just True -> id
                _ -> ((Svg.MoveTo Svg.OriginAbsolute . pure $ startPos) :)
    adaptor <- BRepAdaptor.Curve.fromEdge edge
    curveType <- liftIO $ BRepAdaptor.Curve.curveType adaptor
    thisSegment <-
        case curveType of 
            GeomAbs.CurveType.Line -> liftIO $ lineToPathCommand edge
            GeomAbs.CurveType.BezierCurve -> bezierToPathCommand edge adaptor
            -- GeomAbs.CurveType.BSplineCurve -> There's some argument for special casing this, but we don't need to
            _ -> approximateCurveToPathCommand edge
    return (Just endPos, addMoveCommand thisSegment)

-- | Convert a `Waterfall.Path2D` into a list of `Svg.PathCommands`
path2DToPathCommands :: Waterfall.Path2D -> [Svg.PathCommand]
path2DToPathCommands (Path2D theRawPath) = case theRawPath of  
    Internal.Path.Common.EmptyRawPath -> []
    Internal.Path.Common.SinglePointRawPath _ -> []
    Internal.Path.Common.ComplexRawPath wire -> 
        Internal.Finalizers.unsafeFromAcquireT $
            mconcat 
                . fmap snd
                . scanr (flip (edgeToPathCommand . fst)) (Nothing, []) 
                <$> Internal.Edges.wireEdges wire

-- | Convert a `Waterfall.Diagram` into an SVG document
-- 
-- The diagram paths have the classes "edge", "visible"\/"hidden" and "sharp"\/"outline"
diagramToSvg :: Waterfall.Diagram -> Svg.Document
diagramToSvg diagram = 
    case Waterfall.diagramBoundingBox diagram of 
        Nothing -> Svg.Document Nothing Nothing Nothing [] mempty mempty mempty mempty
        Just (lo, hi) -> 
            let w = Just . Svg.Num $ (hi - lo) ^. _x + 4
                h = Just . Svg.Num $ (hi - lo) ^. _y + 4
                d' = Waterfall.translate2D (2 + negate lo) diagram
                paths lt visibility =
                    path2DToPathCommands =<<
                        Waterfall.diagramLines lt visibility d'
                styles = 
                    [ Svg.Css.CssRule 
                        [[Svg.Css.AllOf [Svg.Css.OfClass "edge"]]]
                        [Svg.Css.CssDeclaration "fill" [[Svg.Css.CssIdent "None"]]]
                    , Svg.Css.CssRule
                        [[Svg.Css.AllOf [Svg.Css.OfClass "edge", Svg.Css.OfClass "visible"]]]
                        [Svg.Css.CssDeclaration "stroke" [[Svg.Css.CssColor $ PixelRGBA8 0 0 0 255]]]
                    , Svg.Css.CssRule
                        [[Svg.Css.AllOf [Svg.Css.OfClass "edge", Svg.Css.OfClass "hidden"]]]
                        [Svg.Css.CssDeclaration "stroke" [[Svg.Css.CssColor $ PixelRGBA8 200 200 255 255]]]
                    ]
                document e = Svg.Document Nothing w h [e] mempty mempty styles mempty
                drawAttrs classes = mempty 
                    & Svg.attrClass .~ classes
                pathOf lt visibility classes = Svg.PathTree $ Svg.Path (drawAttrs classes) (paths lt visibility)
                group children = Svg.GroupTree $ Svg.Group mempty children Nothing Svg.defaultSvg
            in document . group $
                    [ pathOf lineType visibility ["edge", ltClass, vClass]
                        | (visibility, vClass) <- [(Waterfall.Hidden, "hidden"), (Waterfall.Visible, "visible")]
                        , (lineType, ltClass) <- [(Waterfall.SharpLine, "sharp"), (Waterfall.OutLine, "outline")]
                    ]
                    
-- | Write a `Waterfall.Diagram`, to an SVG file at the specified location 
writeDiagramSVG :: FilePath -> Waterfall.Diagram -> IO ()
writeDiagramSVG path = Svg.saveXmlFile path . diagramToSvg