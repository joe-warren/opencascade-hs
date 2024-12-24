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

data SVGError = SVGParseError String | SVGPathError String
    deriving (Eq, Ord, Show)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fn (a, b, c, d, e, f) = fn a b c d e f

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

ellipseToAbsolute :: Double -> Double -> Double -> Bool -> Bool -> V2 Double -> V2 Double -> (V2 Double, Waterfall.Path2D)
ellipseToAbsolute rx ry angleDeg largeArcFlag sweepFlag absoluteEnd start =
    ellipseToRelative rx ry angleDeg largeArcFlag sweepFlag (absoluteEnd - start) start


convertPathCommands :: [Svg.PathCommand] -> Either SVGError [Waterfall.Path2D]
convertPathCommands cs =
    let
        -- relativeLocation _ Svg.OriginAbsolute v = v
        -- relativeLocation curPos Svg.OriginRelative v = curPos + v
        relativeLocation' _ Svg.OriginAbsolute v = last v
        relativeLocation' curPos Svg.OriginRelative v = curPos + sum v
        buildPathInProgress (origin, segments) = 
            Waterfall.pathFromTo2D segments origin
        
        go (cmd:rest) pathInProgress@(o, segments) paths = 
            let goSegment ss = go rest (o, segments <> ss) paths  
            in case cmd of
                (Svg.MoveTo origin v) -> 
                    if null segments 
                        then go rest (relativeLocation' o origin v, []) paths
                        else let (currentPosition, newPath) = buildPathInProgress pathInProgress
                              in go rest (relativeLocation' currentPosition origin v, []) (newPath : paths)
                (Svg.LineTo Svg.OriginAbsolute vs) -> goSegment (Waterfall.lineTo2D <$> vs )
                (Svg.LineTo Svg.OriginRelative vs) -> goSegment (Waterfall.lineRelative2D <$> vs)
                (Svg.HorizontalTo Svg.OriginAbsolute ds) -> 
                    let f d v@(V2 _x y) = let v' = V2 d y in (v', Waterfall.line2D v v')
                        in goSegment (f <$> ds) 
                (Svg.HorizontalTo Svg.OriginRelative ds) -> 
                    let f d v = let v' = v + V2 d 0 in (v', Waterfall.line2D v v')
                    in goSegment (f <$> ds)
                (Svg.VerticalTo Svg.OriginAbsolute ds) -> 
                    let f d v@(V2 x _y) = let v' = V2 x d in (v', Waterfall.line2D v v')
                     in goSegment (f <$> ds) 
                (Svg.VerticalTo Svg.OriginRelative ds) -> 
                    let f d v = let v' = v + V2 0 d in (v', Waterfall.line2D v v')
                     in goSegment (f <$> ds) 
                (Svg.CurveTo Svg.OriginAbsolute points) -> goSegment (uncurry3 Waterfall.bezierTo2D <$> points)
                (Svg.CurveTo Svg.OriginRelative points) -> goSegment (uncurry3 Waterfall.bezierRelative2D <$> points)
                (Svg.EllipticalArc Svg.OriginAbsolute points) -> goSegment (uncurry6 ellipseToAbsolute <$> points)
                (Svg.EllipticalArc Svg.OriginRelative points) -> goSegment (uncurry6 ellipseToRelative <$> points)
                Svg.EndPath -> 
                    if null segments 
                        then go rest (o, []) paths
                        else let (_, newPath) = buildPathInProgress pathInProgress
                              in go rest (o, []) (Waterfall.closeLoop newPath : paths)
                Svg.SmoothCurveTo _ _ -> Left (SVGPathError "Smooth curves not supported")
                Svg.QuadraticBezier _ _ -> Left (SVGPathError "Quadratic bezier not supported")
                Svg.SmoothQuadraticBezierCurveTo _ _ -> Left (SVGPathError "Smooth QuadraticBezier curves not supported")
        go [] pathInProgress@(_o, segments) paths = 
            if null segments 
                then Right paths
                else Right $ snd (buildPathInProgress pathInProgress) : paths
    in go cs (zero, []) []

parsePath :: String -> Either SVGError [Waterfall.Path2D]
parsePath s =
    case Atto.parseOnly (pathParser <* Atto.endOfInput) (T.pack s) of 
        Right r -> convertPathCommands r
        Left msg -> Left (SVGParseError msg)

