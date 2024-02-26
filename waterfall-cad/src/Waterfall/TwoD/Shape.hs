module Waterfall.TwoD.Shape
( Shape
, fromPath
, unitCircle
, unitSquare
, centeredSquare
) where

import Waterfall.TwoD.Internal.Shape (Shape (..))
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.TwoD.Transforms (translate2D)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import OpenCascade.Inheritance (upcast)
import Linear (unit, _x, _y, zero, V2 (..))
import Waterfall.Path.Common (pathFrom, arcViaTo, lineTo)

-- | Construct a 2D Shape from a closed path 
fromPath :: Path2D -> Shape
fromPath (Path2D r)= Shape . unsafeFromAcquire  $ do
    p <- toAcquire r
    upcast <$> (MakeFace.face =<< MakeFace.fromWire p False)

-- | Circle with radius 1, centered on the origin
unitCircle :: Shape
unitCircle = fromPath $ pathFrom (unit _x)
                [ arcViaTo (unit _y) (negate $ unit _x)
                , arcViaTo (negate $ unit _y) (unit _x)
                ]

-- | Square with side length of 1, one vertex on the origin, another on \( (1, 1) \)
unitSquare :: Shape
unitSquare =
    fromPath $ pathFrom zero
        [ lineTo (unit _x)
        , lineTo (V2 1 1)
        , lineTo (unit _y)
        , lineTo zero
        ]

-- | Square with side length of 1, centered on the origin
centeredSquare :: Shape
centeredSquare = translate2D (V2 (-0.5) (-0.5)) unitSquare