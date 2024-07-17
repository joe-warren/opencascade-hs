{-# LANGUAGE ScopedTypeVariables #-}
{-|
Paths in 2D space.
-}
module Waterfall.TwoD.Path2D
( Path2D
, Sense (..)
, module Waterfall.Path.Common
, arc
, arcTo
, arcRelative
, repeatLooping
, closeLoop
-- $ reexports
, line2D
, lineTo2D
, lineRelative2D
, arcVia2D
, arcViaTo2D
, arcViaRelative2D
, bezier2D
, bezierTo2D
, bezierRelative2D
, pathFrom2D
, pathFromTo2D
, pathEndpoints2D
) where 

import Waterfall.TwoD.Internal.Path2D (Path2D(..))
import Waterfall.TwoD.Transforms (rotate2D)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import qualified Waterfall.Internal.Edges as Internal.Edges
import Linear.V2 (V2(..))
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.))
import Linear ((^*), _xy, distance, normalize, unangle)
import Waterfall.Path.Common

data Sense = Clockwise | Counterclockwise deriving (Eq, Show)

-- | Section of a circle, with a given radius, that lies between two points.
--
-- This may fail, if the radius is less than half of the distance between the points.
--
-- In general, `arcVia` is the \"safer\" way to construct an arc.
--
-- `arc` is not polymorphic, as it would not be possible to define an arc in 3D space in this way.
arc :: Sense -> Double -> V2 Double -> V2 Double -> Path2D 
arc sense radius start end = 
    let mid = (start + end) ^* 0.5
        (V2 dx dy) = normalize $ end - start
        rotD = case sense of    
                Clockwise -> V2 dy (-dx)
                Counterclockwise -> V2 (-dy) dx
        dse = distance start end 
        tangent = radius - sqrt (radius * radius - dse * dse / 4) 
        arcMid = mid + rotD ^* tangent
    in if dse > 2 * radius
            then error "points too far apart in arc"
            else arcVia start arcMid end  

-- | Version of `arc` designed to work with `pathFrom`
arcTo :: Sense -> Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcTo sense radius end = \start -> (end, arc sense radius start end) 

-- | Version of `arc` designed to work with `pathFrom`
-- 
-- With relative points; specifying the distance of the endpoint
-- relative to the start of the line, rather than in absolute space.
arcRelative :: Sense -> Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcRelative sense radius dEnd = do
    end <- (+ dEnd)
    arcTo sense radius end

-- | Given a Path where both endpoints are equidistant from the origin.
--
-- And which subtends an angle \( φ \) from the origin that evenly divides a complete revolution, such that \(n φ = 2 π \).
-- 
-- Replicates the path \( n \) times, rotating it by \( φ \), until the resulting path completes one revolution around the origin.
--
-- This can be used to construct paths with rotational symmetry, such as regular polygons, or gears.
repeatLooping :: Path2D -> Path2D
repeatLooping p = Path2D . unsafeFromAcquire $ do
    path <- toAcquire . rawPath $ p 
    (s, e) <- liftIO . Internal.Edges.wireEndpoints $ path
    let a = unangle (e ^. _xy) - unangle (s ^. _xy)
    let times :: Integer = abs . round $ pi * 2 / a 
    toAcquire . rawPath . mconcat $ [rotate2D (negate (fromIntegral n) * a) p | n <- [0..times]]

-- | Given a path, return a new path with the endpoints joined by a straight line.
closeLoop :: Path2D -> Path2D
closeLoop p = Path2D . unsafeFromAcquire $ do
    path <- toAcquire . rawPath $ p
    (s, e) <- liftIO . Internal.Edges.wireEndpoints $ path
    toAcquire .rawPath . mconcat $ [p, line (e ^. _xy)  (s ^. _xy)]

-- $reexports
--
-- reexports from Waterfall.Path.Common, but monomorphised

-- | `line`, with the type fixed to `Path2D`
line2D :: V2 Double -> V2 Double -> Path2D
line2D = line 

-- | `lineTo`, with the type fixed to `Path2D`
lineTo2D :: V2 Double -> V2 Double -> (V2 Double, Path2D)
lineTo2D = lineTo

-- | `lineRelative`, with the type fixed to `Path2D`
lineRelative2D :: V2 Double -> V2 Double -> (V2 Double, Path2D)
lineRelative2D = lineRelative

-- | `arcVia`, with the type fixed to `Path2D`
arcVia2D :: V2 Double -> V2 Double -> V2 Double -> Path2D
arcVia2D = arcVia

-- | `arcViaTo`, with the type fixed to `Path2D`
arcViaTo2D :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcViaTo2D = arcViaTo

-- | `arcViaRelative`, with the type fixed to `Path2D`
arcViaRelative2D :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Path2D)
arcViaRelative2D = arcViaRelative

-- | `bezier`, with the type fixed to `Path2D`
bezier2D :: V2 Double -> V2 Double -> V2 Double -> V2 Double ->  Path2D
bezier2D = bezier

-- | `bezierTo`, with the type fixed to `Path2D`
bezierTo2D :: V2 Double -> V2 Double -> V2 Double -> V2 Double ->  (V2 Double, Path2D)
bezierTo2D = bezierTo

-- | `bezierRelative`, with the type fixed to `Path2D`
bezierRelative2D :: V2 Double -> V2 Double -> V2 Double -> V2 Double ->  (V2 Double, Path2D)
bezierRelative2D = bezierRelative

-- | `pathFrom`, with the type fixed to `Path2D`
pathFrom2D :: V2 Double -> [V2 Double -> (V2 Double, Path2D)] -> Path2D
pathFrom2D = pathFrom

-- | `pathFromTo`, with the type fixed to `Path2D`
pathFromTo2D :: [V2 Double -> (V2 Double, Path2D)] -> V2 Double -> (V2 Double, Path2D)
pathFromTo2D = pathFromTo

-- | `pathEndpoints`, with the type fixed to `Path2D` 
pathEndpoints2D :: Path2D -> (V2 Double, V2 Double)
pathEndpoints2D = pathEndpoints