{-# LANGUAGE TupleSections #-}
module Waterfall.SVG 
( SVGError (..)
, convertPathCommands
, parsePath
, convertTransform
, convertTree
, convertDocument
, readSVG
) where

import qualified Waterfall
import qualified Data.Attoparsec.Text as Atto
import Graphics.Svg.PathParser (pathParser)
import qualified Graphics.Svg as Svg
import qualified Data.Text as T
import Linear (V3 (..), V2 (..), zero, Metric (norm), normalize, (^*), (*^), _x, _y, unit)
import Control.Lens ((^.), ala, each)
import Data.Monoid (Endo (..))
import Control.Arrow (second)
import Data.Foldable (foldl')
import Control.Monad (join, (<=<))
import Data.Maybe (catMaybes)
import Data.Function ((&))

data SVGErrorKind = SVGIOError | SVGParseError | SVGPathError | SVGTransformError | SVGNumberError
    deriving (Eq, Ord, Show)

data SVGError = SVGError SVGErrorKind String
        deriving (Eq, Ord, Show)

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fn (a, b, c, d, e, f) = fn a b c d e f

pathFromToWithControlPoint :: [Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))] -> V2 Double -> Either SVGError (V2 Double, Waterfall.Path2D)
pathFromToWithControlPoint commands start = 
    let go (cp, (pos, paths)) cmd = second (second (:paths)) <$> cmd cp pos
        go' b a = join (go <$> b <*> pure a)
    in case foldl' go' (Right (Nothing, (start, []))) commands of
        Right (_cp, (end, allPaths)) -> Right (end, mconcat . reverse $ allPaths)
        Left err -> Left err

ellipseToRelative :: Double -> Double -> Double -> Bool -> Bool -> V2 Double -> V2 Double -> (V2 Double, Waterfall.Path2D)
ellipseToRelative rx ry angleDeg largeArcFlag sweepFlag relativeEnd =
    let angleRads = angleDeg * pi / 180
        scaleFac = ry / rx
        transformForward :: Waterfall.Transformable2D a => a -> a
        transformForward = Waterfall.rotate2D (angleRads) . Waterfall.scale2D (V2 (1/scaleFac) 1)
        transformBack :: Waterfall.Transformable2D a => a -> a
        transformBack = Waterfall.scale2D (V2 (scaleFac) 1) . Waterfall.rotate2D (-angleRads)
        relativeEndTransformed@(V2 retX retY) = transformBack relativeEnd
        transformedDistance = norm relativeEndTransformed
        halfTD = transformedDistance * 0.5
        perp = normalize (V2 (-retY) retX)  
        p1 = if sweepFlag == largeArcFlag then negate perp else perp
        p2 = if largeArcFlag then p1 else negate p1
        radius = max ry halfTD 
        centerPerpDistance = sqrt (radius * radius - halfTD * halfTD)
        center = (relativeEndTransformed ^* 0.5) + (p1 ^* centerPerpDistance)
        midPoint = center + (p2 ^* radius)
        
        in Waterfall.splice . transformForward $ Waterfall.arcVia zero midPoint relativeEndTransformed 

quadraticBezierAbsolute :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Waterfall.Path2D)
quadraticBezierAbsolute p0 p1 p2 = (p2, Waterfall.bezier2D p0 (p0 + ((p1 - p0) ^* (2/3))) (p2 + ((p1 - p2) ^* (2/3))) p2)

curveToAbsolute :: (V2 Double, V2 Double, V2 Double) -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
curveToAbsolute (cp1, cp2, cp3) _ cp0 = Right (Just (cp3 + cp3 - cp2), Waterfall.bezierTo2D cp1 cp2 cp3 cp0)

curveToRelative :: (V2 Double, V2 Double, V2 Double) -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
curveToRelative (cp1, cp2, cp3) _ cp0 = curveToAbsolute (cp0 + cp1, cp0 + cp2, cp0 + cp3) Nothing cp0

quadraticBezierAbsolute' :: (V2 Double, V2 Double) -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
quadraticBezierAbsolute' (cp1, cp2) _ cp0 = Right (Just (cp2 + cp2 - cp1), quadraticBezierAbsolute cp0 cp1 cp2)

quadraticBezierRelative' :: (V2 Double, V2 Double) -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
quadraticBezierRelative' (cp1, cp2) _ cp0 = quadraticBezierAbsolute' (cp0 + cp1, cp0 + cp2) Nothing cp0

ellipseToAbsolute :: Double -> Double -> Double -> Bool -> Bool -> V2 Double -> V2 Double -> (V2 Double, Waterfall.Path2D)
ellipseToAbsolute rx ry angleDeg largeArcFlag sweepFlag absoluteEnd start =
    ellipseToRelative rx ry angleDeg largeArcFlag sweepFlag (absoluteEnd - start) start

smoothCurveToAbsolute :: (V2 Double, V2 Double) -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
smoothCurveToAbsolute _ Nothing _ = Left (SVGError SVGPathError "S command must follow either an S, s, C or c command")
smoothCurveToAbsolute (cp2, cp3) (Just cp1) cp0 = Right (Just (cp3 + cp3 - cp2), Waterfall.bezierTo2D cp1 cp2 cp3 cp0) 

smoothCurveToRelative ::  (V2 Double, V2 Double) -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
smoothCurveToRelative _ Nothing _ = Left (SVGError SVGPathError "s command must follow either an S, s, C or c command")
smoothCurveToRelative (cp2, cp3) cp1 cp0 = smoothCurveToAbsolute (cp0 + cp2, cp0 + cp3) cp1 cp0

smoothQuadraticBezierCurveToAbsolute :: V2 Double -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
smoothQuadraticBezierCurveToAbsolute _ Nothing _ = Left (SVGError SVGPathError "T command must follow either an T, t, Q or q command")
smoothQuadraticBezierCurveToAbsolute cp2 (Just cp1) cp0 = Right (Just (cp2 + cp2 - cp1), quadraticBezierAbsolute cp0 cp1 cp2)

smoothQuadraticBezierCurveToRelative :: V2 Double -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
smoothQuadraticBezierCurveToRelative _ Nothing _ = Left (SVGError SVGPathError "t command must follow either an T, t, Q or q command")
smoothQuadraticBezierCurveToRelative cp2 cp1 cp0 = smoothQuadraticBezierCurveToRelative (cp0 + cp2) cp1 cp0

convertPathCommands :: [Svg.PathCommand] -> Either SVGError [Waterfall.Path2D]
convertPathCommands cs =
    let
        relativeLocation _ Svg.OriginAbsolute v = v
        relativeLocation curPos Svg.OriginRelative v = curPos + v
        buildPathInProgress (origin, segments) = 
            pathFromToWithControlPoint segments origin
        withoutControlPoint f _cp o = Right (Nothing, f o)
        go (cmd:rest) pathInProgress@(o, segments) paths = 
            let goSegment ss = go rest (o, segments <> ss) paths  
            in case cmd of
                (Svg.MoveTo origin (v:vs)) ->
                    let restPlusImplicitLineTo =
                            case vs of
                                [] -> rest
                                implicitLineTos -> Svg.LineTo origin implicitLineTos : rest
                    in if null segments
                        then go restPlusImplicitLineTo (relativeLocation o origin v, []) paths
                        else case buildPathInProgress pathInProgress of
                            Right (currentPosition, newPath) ->  go restPlusImplicitLineTo (relativeLocation currentPosition origin v, []) (newPath : paths)
                            Left err -> Left err
                (Svg.MoveTo _ []) -> Left (SVGError SVGPathError "Empty MoveTo command")
                (Svg.LineTo Svg.OriginAbsolute vs) -> goSegment (withoutControlPoint . Waterfall.lineTo2D <$> vs )
                (Svg.LineTo Svg.OriginRelative vs) -> goSegment (withoutControlPoint . Waterfall.lineRelative2D <$> vs)
                (Svg.HorizontalTo Svg.OriginAbsolute ds) -> 
                    let f d v@(V2 _x y) = let v' = V2 d y in (v', Waterfall.line2D v v')
                        in goSegment (withoutControlPoint . f <$> ds) 
                (Svg.HorizontalTo Svg.OriginRelative ds) -> 
                    let f d v = let v' = v + V2 d 0 in (v', Waterfall.line2D v v')
                    in goSegment (withoutControlPoint . f <$> ds)
                (Svg.VerticalTo Svg.OriginAbsolute ds) -> 
                    let f d v@(V2 x _y) = let v' = V2 x d in (v', Waterfall.line2D v v')
                     in goSegment (withoutControlPoint . f <$> ds) 
                (Svg.VerticalTo Svg.OriginRelative ds) -> 
                    let f d v = let v' = v + V2 0 d in (v', Waterfall.line2D v v')
                     in goSegment (withoutControlPoint . f <$> ds) 
                (Svg.CurveTo Svg.OriginAbsolute points) -> goSegment (curveToAbsolute <$> points)
                (Svg.CurveTo Svg.OriginRelative points) -> goSegment (curveToRelative <$> points)
                (Svg.EllipticalArc Svg.OriginAbsolute points) -> goSegment (withoutControlPoint . uncurry6 ellipseToAbsolute <$> points)
                (Svg.EllipticalArc Svg.OriginRelative points) -> goSegment (withoutControlPoint . uncurry6 ellipseToRelative <$> points)
                Svg.QuadraticBezier Svg.OriginAbsolute points -> goSegment (quadraticBezierAbsolute' <$> points)
                Svg.QuadraticBezier Svg.OriginRelative points -> goSegment (quadraticBezierRelative' <$> points)
                Svg.SmoothCurveTo Svg.OriginAbsolute points -> goSegment (smoothCurveToAbsolute <$> points)
                Svg.SmoothCurveTo Svg.OriginRelative points -> goSegment (smoothCurveToRelative <$> points)
                Svg.SmoothQuadraticBezierCurveTo Svg.OriginAbsolute points -> goSegment (smoothQuadraticBezierCurveToAbsolute <$> points)
                Svg.SmoothQuadraticBezierCurveTo Svg.OriginRelative points -> goSegment (smoothQuadraticBezierCurveToRelative <$> points)
                Svg.EndPath -> 
                    if null segments 
                        then go rest (o, []) paths
                        else case buildPathInProgress pathInProgress of
                                 Right (_, newPath) -> go rest (o, []) (Waterfall.closeLoop newPath : paths)
                                 Left err -> Left err
        go [] pathInProgress@(_o, segments) paths = 
            if null segments 
                then Right paths
                else (:paths) . snd <$> buildPathInProgress pathInProgress
    in go cs (zero, []) []

parsePath :: String -> Either SVGError [Waterfall.Path2D]
parsePath s =
    case Atto.parseOnly (pathParser <* Atto.endOfInput) (T.pack s) of 
        Right r -> convertPathCommands r
        Left msg -> Left (SVGError SVGParseError msg)

convertTransform :: Waterfall.Transformable2D a => Svg.Transformation -> Either SVGError (a -> a)
convertTransform (Svg.TransformMatrix a b c d e f) = Right $ Waterfall.matTransform2D (V2 (V3 a c e) (V3 b d f))
convertTransform (Svg.Translate x y) = Right $ Waterfall.translate2D (V2 x y)
convertTransform (Svg.Scale v Nothing) = Right $ Waterfall.uScale2D v
convertTransform (Svg.Scale x (Just y)) = Right $ Waterfall.scale2D (V2 x y)
convertTransform (Svg.Rotate angleDeg center) = 
    let center' = maybe zero (uncurry V2) center 
        fwd = Waterfall.translate2D (negate center')
        angleRad = angleDeg * pi / 180
        back = Waterfall.translate2D center'
     in Right (back . Waterfall.rotate2D angleRad . fwd)
convertTransform (Svg.SkewX x) = Right $ Waterfall.matTransform2D (V2 (V3 x 0 0) (V3 0 1 0))
convertTransform (Svg.SkewY y) = Right $ Waterfall.matTransform2D (V2 (V3 1 0 0) (V3 0 y 0))
convertTransform Svg.TransformUnknown = Left . (SVGError SVGTransformError) $ "Unknown Transform"

chain :: [a -> a] -> a -> a
chain = ala Endo foldMap

svgDPI :: Svg.Dpi
svgDPI = 300

convertNumber :: Svg.Number -> Either SVGError Double
convertNumber n = 
    -- toUserUnit should guarantee we either get a Num, Em, or Percent value here
    -- of which only Num is supported
    case Svg.toUserUnit svgDPI n of 
        Svg.Num v -> Right v
        Svg.Px _ -> Left (SVGError SVGNumberError "Unexpected Px value")
        Svg.Em _ -> Left (SVGError SVGNumberError "Unsupported Em value")
        Svg.Percent _ -> Left (SVGError SVGNumberError "Unsupported Percent value")
        Svg.Pc _ ->  Left (SVGError SVGNumberError "Unexpected Pc value")
        Svg.Inches _ -> Left (SVGError SVGNumberError "Unexpected Inches value")
        Svg.Mm _ -> Left (SVGError SVGNumberError "Unexpected Mm value")
        Svg.Cm _ -> Left (SVGError SVGNumberError "Unexpected Cm value")
        Svg.Point _ -> Left (SVGError SVGNumberError "Unexpected Point value")


convertPoint :: Svg.Point -> Either SVGError (V2 Double)
convertPoint = fmap (uncurry V2) . each convertNumber 

convertCircle :: Svg.Circle -> Either SVGError [Waterfall.Path2D]
convertCircle circle = do 
    center <- convertPoint (circle ^. Svg.circleCenter)
    radius <- circle ^. Svg.circleRadius & convertNumber
    return
        . fmap (Waterfall.translate2D center . Waterfall.uScale2D radius)
        . Waterfall.shapePaths 
        $ Waterfall.unitCircle

convertPoints :: [Svg.RPoint] -> [Waterfall.Path2D]
convertPoints (h:t) = pure $ Waterfall.pathFrom h (Waterfall.lineTo <$> t)
convertPoints [] = []

convertPolyLine  :: Svg.PolyLine -> [Waterfall.Path2D]
convertPolyLine polyLine = convertPoints (polyLine ^. Svg.polyLinePoints)

wrap :: [a] -> [a]
wrap (h:t) = h:t <> [h] 
wrap [] = []

convertPolygon  :: Svg.Polygon -> [Waterfall.Path2D]
convertPolygon polygon = convertPoints (polygon ^. Svg.polygonPoints & wrap)

convertLine :: Svg.Line -> Either SVGError Waterfall.Path2D
convertLine line = 
    Waterfall.line 
        <$> convertPoint (line ^. Svg.linePoint1)
        <*> convertPoint (line ^. Svg.linePoint2)
        
convertEllipse :: Svg.Ellipse -> Either SVGError [Waterfall.Path2D]
convertEllipse ellipse = do 
    center <- convertPoint (ellipse ^. Svg.ellipseCenter)
    rX <- ellipse ^. Svg.ellipseXRadius & convertNumber
    rY <- ellipse ^. Svg.ellipseYRadius & convertNumber
    return 
        . fmap (Waterfall.translate2D center . Waterfall.scale2D (V2 rX rY))
        . Waterfall.shapePaths
        $ Waterfall.unitCircle

convertRectangle :: Svg.Rectangle -> Either SVGError [Waterfall.Path2D]
convertRectangle rect = do
    upperLeft <- convertPoint (rect ^. Svg.rectUpperLeftCorner)    
    (rX', rY') <- each convertNumber (rect ^. Svg.rectCornerRadius)
    w <- convertNumber (rect ^. Svg.rectWidth)
    h <- convertNumber (rect ^. Svg.rectHeight)
    let rX = min rX' (w/2)
    let rY = min rY' (h/2)
    let w' = w - 2 * rX
    let h' = h - 2 * rY
    let quarterCircle = Waterfall.arcVia (negate $ unit _y) (normalize (V2 1 (-1))) (unit _x)
    let scaleBevel = Waterfall.scale2D (V2 rX rY)
    if rX == 0 || rY == 0 
        then Waterfall.unitSquare &
                Waterfall.scale2D (V2 w h) &
                Waterfall.translate2D upperLeft &
                Waterfall.shapePaths & 
                return
        else return . pure . Waterfall.pathFrom (V2 rX 0) . catMaybes $
                [ if w' > 0 then Just (Waterfall.lineRelative (w' *^ unit _x)) else Nothing
                , quarterCircle 
                    & scaleBevel 
                    & Waterfall.splice
                    & pure
                , if h' > 0 then Just (Waterfall.lineRelative (h' *^ unit _y)) else Nothing
                , quarterCircle 
                    & Waterfall.rotate2D (pi/2)
                    & scaleBevel 
                    & Waterfall.splice
                    & pure
                , if w' > 0 then Just (Waterfall.lineRelative (negate (w' *^ unit _x))) else Nothing
                , quarterCircle 
                    & Waterfall.rotate2D pi
                    & scaleBevel 
                    & Waterfall.splice
                    & pure
                , if h' > 0 then Just (Waterfall.lineRelative (negate (h' *^ unit _y))) else Nothing
                , quarterCircle 
                    & Waterfall.rotate2D (-pi/2)
                    & scaleBevel 
                    & Waterfall.splice
                    & pure
                ]

convertTree :: Svg.Tree -> Either SVGError [Waterfall.Path2D]
convertTree tree = do
    transform <- maybe (pure id) (fmap chain . traverse convertTransform) (tree ^. Svg.drawAttr . Svg.drawAttributes . Svg.transform)
    fmap transform <$> case tree of
        Svg.PathTree path -> convertPathCommands (path ^. Svg.pathDefinition)
        Svg.GroupTree group ->  mconcat <$> traverse convertTree (group ^. Svg.groupChildren)
        Svg.SymbolTree sym ->  mconcat <$> traverse convertTree (sym ^. Svg.groupOfSymbol . Svg.groupChildren)
        Svg.CircleTree circle -> convertCircle circle
        Svg.PolyLineTree polyLine -> pure $ convertPolyLine polyLine
        Svg.PolygonTree polygon -> pure $ convertPolygon polygon
        Svg.LineTree line -> pure <$> convertLine line
        Svg.EllipseTree ellipse -> convertEllipse ellipse
        Svg.RectangleTree rectangle -> convertRectangle rectangle
        _ -> Right []

convertDocument :: Svg.Document -> Either SVGError [Waterfall.Path2D]
convertDocument doc = fmap mconcat . traverse convertTree $ (doc ^. Svg.elements) 

readSVG ::FilePath -> IO (Either SVGError [Waterfall.Path2D])
readSVG path = 
    let fileReadErr = Left . SVGError SVGIOError $ "Failed to read svg from file: " <> path
    in ( convertDocument <=< maybe fileReadErr Right) <$> Svg.loadSvgFile path 