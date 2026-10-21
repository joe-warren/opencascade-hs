{-# Language OverloadedStrings #-}
{-|
Convert "Waterfall" data into [SVG](https://developer.mozilla.org/en-US/docs/Web/SVG)
-}
module Waterfall.SVG.ToSVG
( path2DToPathCommands
, module Waterfall.SVG.ToSVG.LineStyle
, lineStyleCss
, diagramToSvgWithLineStyle
, diagramToSvg
, writeDiagramSVG
) where

import qualified Waterfall
import Waterfall.SVG.ToSVG.LineStyle
import qualified Graphics.Svg as Svg
import qualified Graphics.Svg.CssTypes as Svg.Css
import Waterfall.Internal.NearZero (nearZero)
import Linear (_xy, _x, _y, V2 (..))
import Control.Lens ((^.), (&), (.~), (<&>))
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
import Data.Traversable (mapAccumM)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.DeepSeq (NFData (rnf))

-- SVG.PathCommand doesn't have an NFData instance
-- which is needed for it to be the target of an `unsafeFromAcquire`
newtype ForceablePathCommand = ForceablePathCommand 
    { getForceablePathCommand :: Svg.PathCommand
    }

instance NFData ForceablePathCommand where
    rnf (ForceablePathCommand (Svg.MoveTo o ps)) = (o `seq` ()) `seq` rnf ps
    rnf (ForceablePathCommand (Svg.LineTo o ps)) = (o `seq` ()) `seq` rnf ps
    rnf (ForceablePathCommand (Svg.HorizontalTo o cs)) = (o `seq` ()) `seq` rnf cs
    rnf (ForceablePathCommand (Svg.VerticalTo o cs)) = (o `seq` ()) `seq` rnf cs
    rnf (ForceablePathCommand (Svg.CurveTo o ps)) = (o `seq` ()) `seq` rnf ps
    rnf (ForceablePathCommand (Svg.SmoothCurveTo o ps)) = (o `seq` ()) `seq` rnf ps
    rnf (ForceablePathCommand (Svg.QuadraticBezier o ps)) = (o `seq` ()) `seq` rnf ps
    rnf (ForceablePathCommand (Svg.SmoothQuadraticBezierCurveTo o ps)) = (o `seq` ()) `seq` rnf ps
    rnf (ForceablePathCommand (Svg.EllipticalArc o as)) = (o `seq` ()) `seq` rnf as
    rnf (ForceablePathCommand Svg.EndPath) = ()


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

edgeToPathCommand ::  Maybe (V2 Double) -> Ptr TopoDS.Edge -> Acquire (Maybe (V2 Double), [Svg.PathCommand])
edgeToPathCommand curPos edge = do 
        startPos <- liftIO $ (^. _xy) <$> Internal.Edges.edgeValue edge 0
        endPos <- liftIO $ (^. _xy) <$> Internal.Edges.edgeValue edge 1
        let hasntMoved = all nearZero . (startPos -) <$> curPos
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

shapeToPathCommands :: Waterfall.Shape -> [Svg.PathCommand]
shapeToPathCommands = 
    let stripMoveTo = take 1 <> filter (not . isMoveTo) . drop 1
        isMoveTo (Svg.MoveTo _ _) = True
        isMoveTo _ = False
    in concatMap (stripMoveTo. path2DToPathCommands)
        . Waterfall.shapePaths 
    
colourToPixel :: Waterfall.Colour -> PixelRGBA8
colourToPixel (Waterfall.Colour r g b) =
    let conv = round . (* 255) . min 1 . max 0
     in PixelRGBA8 (conv r) (conv g) (conv b) 255

fillToPath :: (Waterfall.Paint, Waterfall.Shape) -> Svg.Path
fillToPath (paint, shape) =
    let colour = fromMaybe (Waterfall.Colour 1 1 1) (paint ^. Waterfall.paintColour <|> Waterfall.defaultPaint ^. Waterfall.paintColour)
        opacity = fromMaybe (1.0)  (paint ^. Waterfall.paintOpacity <|> Waterfall.defaultPaint ^. Waterfall.paintOpacity)
        setOpacity = if opacity == 1.0 
            then id 
            else Svg.drawAttr . Svg.fillOpacity .~ (pure . realToFrac $ opacity)

    in Svg.defaultSvg
        & Svg.pathDefinition .~ shapeToPathCommands shape
        & Svg.drawAttr . Svg.fillColor .~ (pure . Svg.ColorRef . colourToPixel $ colour)
        & setOpacity
        
-- | Convert a `Waterfall.Path2D` into a list of `Svg.PathCommands`
path2DToPathCommands :: Waterfall.Path2D -> [Svg.PathCommand]
path2DToPathCommands (Path2D theRawPath) = case theRawPath of  
    Internal.Path.Common.EmptyRawPath -> []
    Internal.Path.Common.SinglePointRawPath _ -> []
    Internal.Path.Common.ComplexRawPath wire -> 
        fmap getForceablePathCommand 
            . Internal.Finalizers.unsafeFromAcquire $
                fmap (
                fmap ForceablePathCommand 
                . concat 
                . snd ) 
                . mapAccumM edgeToPathCommand Nothing 
                =<< (Internal.Edges.wireEdges 
                    =<< Internal.Finalizers.acquireForeignPtr wire)



-- | The inline CSS used to style an SVG diagram
lineStyleCss :: DiagramStyle -> [Svg.Css.CssRule]
lineStyleCss style =
    let resolve l fallback line defaultLine = fromMaybe fallback (line ^. l <|> defaultLine ^. l)
        strokeDeclarations line defaultLine =
            [ Svg.Css.CssDeclaration "stroke" [[Svg.Css.CssColor . colourToPixel $ resolve lineColour (Waterfall.Colour 0 0 0) line defaultLine]]
            , Svg.Css.CssDeclaration "stroke-opacity" [[Svg.Css.CssNumber . Svg.Num $ resolve lineOpacity 1 line defaultLine]]
            , Svg.Css.CssDeclaration "stroke-width" [[Svg.Css.CssNumber . Svg.Num $ resolve lineWidth 1 line defaultLine]]
            ]
    in
    [ Svg.Css.CssRule
        [[Svg.Css.AllOf [Svg.Css.OfClass "edge"]]]
        [ Svg.Css.CssDeclaration "fill" [[Svg.Css.CssIdent "None"]]
        , Svg.Css.CssDeclaration "fill-opacity" [[Svg.Css.CssNumber (Svg.Num 0)]]
        , Svg.Css.CssDeclaration "stroke-linecap" [[Svg.Css.CssIdent "round"]]
        ]
    , Svg.Css.CssRule
        [[Svg.Css.AllOf [Svg.Css.OfClass "visible"], Svg.Css.AllOf[Svg.Css.OfClass "edge"]]]
        (strokeDeclarations (style ^. diagramVisibleLine) (defaultDiagramStyle ^. diagramVisibleLine))
    , Svg.Css.CssRule
        [[Svg.Css.AllOf [Svg.Css.OfClass "hidden"], Svg.Css.AllOf [Svg.Css.OfClass "edge"]]]
        (strokeDeclarations (style ^. diagramHiddenLine) (defaultDiagramStyle ^. diagramHiddenLine))
    , Svg.Css.CssRule
        [[Svg.Css.AllOf [Svg.Css.OfClass "fill"]]]
        [ Svg.Css.CssDeclaration "fill-rule" [[Svg.Css.CssIdent "evenodd"]]]
    ]

-- | Convert a `Waterfall.Diagram` into an SVG document
-- 
-- The diagram paths have the class "edge"
-- and are grouped within elements with class "sharp"\/"outline" and "visible"\/"hidden"
-- 
-- The diagram fills have the class "fill"
diagramToSvgWithLineStyle :: DiagramStyle -> Waterfall.Diagram -> Svg.Document
diagramToSvgWithLineStyle lineStyle diagram = 
    case Waterfall.diagramBoundingBox diagram of 
        Nothing -> Svg.Document Nothing Nothing Nothing [] mempty mempty mempty mempty
        Just (lo, hi) -> 
            let 
                w' = (hi - lo) ^. _x + 4
                h' = (hi - lo) ^. _y + 4           
                w = Just . Svg.Num $ w'
                h = Just . Svg.Num $ h'
                viewbox = Just (lo ^. _x - 2, lo ^. _y - 2, w', h')
                paths lt visibility =
                    path2DToPathCommands =<<
                        Waterfall.diagramLines lt visibility diagram
                styles = lineStyleCss lineStyle
                document e = Svg.Document viewbox w h [e] mempty mempty styles mempty
                pathOf lt visibility = 
                        Svg.defaultSvg 
                            & Svg.pathDefinition .~ paths lt visibility
                            & Svg.drawAttr . Svg.attrClass .~ ["edge"]
                            & Svg.PathTree
                    
                fills = 
                    Svg.defaultSvg 
                        & Svg.groupChildren .~ 
                            (Waterfall.getDiagramFills diagram
                                <&> (fillToPath
                                    >>> Svg.drawAttr . Svg.attrClass .~ ["fill"]
                                    >>> Svg.PathTree
                                )
                            )
                        & Svg.GroupTree
                    
                group children
                     = Svg.defaultSvg
                        & Svg.groupChildren .~ children
                        & Svg.GroupTree 
            in document . group . (fills :) $
                    [ Svg.defaultSvg 
                        & Svg.groupChildren .~
                            [ Svg.defaultSvg 
                                & Svg.groupChildren .~ [ pathOf lineType visibility ]
                                & Svg.drawAttr . Svg.attrClass .~ [ltClass]
                                & Svg.GroupTree
                            | (lineType, ltClass) <- [(Waterfall.SharpLine, "sharp"), (Waterfall.OutLine, "outline")]
                            ]  
                        & Svg.drawAttr . Svg.attrClass .~ [vClass]
                        & Svg.GroupTree
                    | (visibility, vClass) <- [(Waterfall.Hidden, "hidden"), (Waterfall.Visible, "visible")]
                    ]
                    
-- | Convert a `Waterfall.Diagram` into an SVG document
-- 
-- The diagram paths have the class "edge"
-- and are grouped within elements with class "sharp"\/"outline" and "visible"\/"hidden"
-- 
-- The diagram fills have the class "fill"
diagramToSvg :: Waterfall.Diagram -> Svg.Document
diagramToSvg = diagramToSvgWithLineStyle defaultDiagramStyle

-- | Write a `Waterfall.Diagram`, to an SVG file at the specified location 
writeDiagramSVG :: FilePath -> Waterfall.Diagram -> IO ()
writeDiagramSVG path = Svg.saveXmlFile path . diagramToSvg