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
import Linear (V2 (..), zero)

data SVGError = SVGParseError String | SVGPathError String

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

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
                Svg.EndPath -> 
                    if null segments 
                        then go rest (o, []) paths
                        else let (_, newPath) = buildPathInProgress pathInProgress
                              in go rest (o, []) (Waterfall.closeLoop newPath : paths)
                Svg.SmoothCurveTo _ _ -> Left (SVGPathError "Smooth curves not supported")
                Svg.QuadraticBezier _ _ -> Left (SVGPathError "Quadratic bezier not supported")
                Svg.SmoothQuadraticBezierCurveTo _ _ -> Left (SVGPathError "Smooth QuadraticBezier curves not supported")
                Svg.EllipticalArc _ _ -> Left (SVGPathError "Elliptical Arc not supported")
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

