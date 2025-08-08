module Waterfall.TwoD.Shape
( Shape
, makeShape
, shapePaths
, unitCircle
, unitSquare
, centeredSquare
, unitPolygon
) where

import Waterfall.TwoD.Internal.Shape (Shape (..))
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.TwoD.Transforms (translate2D, rotate2D)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import Waterfall.Internal.Edges (allWires)
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import OpenCascade.Inheritance (upcast)
import Linear (unit, _x, _y, zero, V2 (..))
import Waterfall.Path.Common (pathFrom, arcViaTo, lineTo, line)
import Waterfall.Internal.Path.Common (RawPath(ComplexRawPath))

-- | Construct a 2D Shape from a closed path 
makeShape :: Path2D -> Shape
makeShape (Path2D (ComplexRawPath r)) = Shape . unsafeFromAcquire  $ do
    p <- toAcquire r
    upcast <$> (MakeFace.face =<< MakeFace.fromWire p False)
makeShape _ = Shape . unsafeFromAcquire $
    upcast <$> (MakeFace.face =<< MakeFace.new)

-- | Get the paths back from a 2D shape
-- 
-- Ideally:
--
-- @
-- shapePaths . fromPath ≡ pure
-- @
shapePaths :: Shape -> [Path2D] 
shapePaths (Shape r) = fmap (Path2D . ComplexRawPath) . unsafeFromAcquire $ do
    s <- toAcquire r 
    allWires s 

-- | Circle with radius 1, centered on the origin
unitCircle :: Shape
unitCircle = makeShape $ pathFrom (unit _x)
                [ arcViaTo (unit _y) (negate $ unit _x)
                , arcViaTo (negate $ unit _y) (unit _x)
                ]

-- | Square with side length of 1, one vertex on the origin, another on \( (1, 1) \)
unitSquare :: Shape
unitSquare =
    makeShape $ pathFrom zero
        [ lineTo (unit _x)
        , lineTo (V2 1 1)
        , lineTo (unit _y)
        , lineTo zero
        ]

-- | Square with side length of 1, centered on the origin
centeredSquare :: Shape
centeredSquare = translate2D (V2 (-0.5) (-0.5)) unitSquare

-- | \(n\) sided Polygon, centered on the origin
-- 
-- Ill-defined when n <= 2
unitPolygon :: Integer -> Shape
unitPolygon n = 
    let n' = fromIntegral n
        points = [
            rotate2D (2 * pi * fromIntegral i / n') (unit _x)
            | i <- [0..n]
            ]
        paths = mconcat [
            line a b
            | (a, b) <- zip points (tail points)
            ]
        in makeShape paths