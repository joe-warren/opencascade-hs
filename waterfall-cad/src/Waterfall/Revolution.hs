module Waterfall.Revolution 
( revolution
) where

import Waterfall.Internal.Solid (Solid (..), debug)
import Waterfall.TwoD.Internal.Path (Path (..))
import qualified OpenCascade.BRepPrimAPI.MakeRevol as MakeRevol
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.GP as GP
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Waterfall.Transforms (rotate)
import Control.Monad.IO.Class (liftIO)
import Linear (unit, _x)

revolution :: Path -> Solid
revolution (Path runPath) = debug "revolution" .  rotate (unit _x) (pi/2) . Solid $ do
    p <- runPath
    axis <- GP.oy -- revolve around the y axis
    revol <- MakeRevol.fromShapeAndAx1 (upcast p) axis True
    shell <- MakeShape.shape (upcast revol)
    solidBuilder <- MakeSolid.new
    liftIO $ MakeSolid.add solidBuilder =<< unsafeDowncast shell
    MakeShape.shape (upcast solidBuilder)