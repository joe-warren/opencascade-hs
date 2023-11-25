module Waterfall.Revolution 
( revolution
) where

import Waterfall.Internal.Solid (Solid (..))
import Waterfall.TwoD.Internal.Path (Path (..))
import qualified OpenCascade.BRepPrimAPI.MakeRevol as MakeRevol
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.GP as GP
import OpenCascade.Inheritance (upcast)
import Waterfall.Transforms (rotate)
import Linear (unit, _x)

revolution :: Path -> Solid
revolution (Path runPath) = rotate (unit _x) (pi/2) . Solid $ do
    p <- runPath
    axis <- GP.oy -- revolve around the y axis
    revol <- MakeRevol.fromShapeAndAx1 (upcast p) axis True
    MakeShape.shape (upcast revol)