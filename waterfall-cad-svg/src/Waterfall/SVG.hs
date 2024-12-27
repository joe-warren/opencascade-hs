{-# LANGUAGE TupleSections #-}
module Waterfall.SVG 
( SVGError (..)
, convertPathCommands
, parsePath
) where

import qualified Waterfall
import qualified Data.Attoparsec.Text as Atto
import Graphics.Svg.PathParser (pathParser)
import Graphics.Svg.Types as Svg
import qualified Data.Text as T
import Linear (V2 (..), zero, Metric (norm), normalize, (^*))
import Control.Arrow (second)
import Data.Foldable (foldl')
import Control.Monad (join)

data SVGError = SVGParseError String | SVGPathError String
    deriving (Eq, Ord, Show)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

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
smoothCurveToAbsolute _ Nothing _ = Left (SVGPathError "S command must follow either an S, s, C or c command")
smoothCurveToAbsolute (cp2, cp3) (Just cp1) cp0 = Right (Just (cp3 + cp3 - cp2), Waterfall.bezierTo2D cp1 cp2 cp3 cp0) 

smoothCurveToRelative ::  (V2 Double, V2 Double) -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
smoothCurveToRelative _ Nothing _ = Left (SVGPathError "s command must follow either an S, s, C or c command")
smoothCurveToRelative (cp2, cp3) cp1 cp0 = smoothCurveToAbsolute (cp0 + cp2, cp0 + cp3) cp1 cp0

smoothQuadraticBezierCurveToAbsolute :: V2 Double -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
smoothQuadraticBezierCurveToAbsolute _ Nothing _ = Left (SVGPathError "T command must follow either an T, t, Q or q command")
smoothQuadraticBezierCurveToAbsolute cp2 (Just cp1) cp0 = Right (Just (cp2 + cp2 - cp1), quadraticBezierAbsolute cp0 cp1 cp2)

smoothQuadraticBezierCurveToRelative :: V2 Double -> Maybe (V2 Double) -> V2 Double -> Either SVGError (Maybe (V2 Double), (V2 Double, Waterfall.Path2D))
smoothQuadraticBezierCurveToRelative _ Nothing _ = Left (SVGPathError "t command must follow either an T, t, Q or q command")
smoothQuadraticBezierCurveToRelative cp2 cp1 cp0 = smoothQuadraticBezierCurveToRelative (cp0 + cp2) cp1 cp0

convertPathCommands :: [Svg.PathCommand] -> Either SVGError [Waterfall.Path2D]
convertPathCommands cs =
    let
        relativeLocation' _ Svg.OriginAbsolute v = last v
        relativeLocation' curPos Svg.OriginRelative v = curPos + sum v
        buildPathInProgress (origin, segments) = 
            pathFromToWithControlPoint segments origin
        withoutControlPoint f _cp o = Right (Nothing, f o)
        go (cmd:rest) pathInProgress@(o, segments) paths = 
            let goSegment ss = go rest (o, segments <> ss) paths  
            in case cmd of
                (Svg.MoveTo origin v) -> 
                    if null segments 
                        then go rest (relativeLocation' o origin v, []) paths
                        else case buildPathInProgress pathInProgress of
                            Right (currentPosition, newPath) ->  go rest (relativeLocation' currentPosition origin v, []) (newPath : paths)
                            Left err -> Left err
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
        Left msg -> Left (SVGParseError msg)

