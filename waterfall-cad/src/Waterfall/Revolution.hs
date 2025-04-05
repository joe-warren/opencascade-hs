module Waterfall.Revolution 
( revolution
) where

import Waterfall.Internal.Solid (Solid (..), solidFromAcquire)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Waterfall.Internal.Finalizers (toAcquire)
import qualified OpenCascade.BRepPrimAPI.MakeRevol as MakeRevol
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.GP as GP
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Waterfall.Transforms (rotate)
import Control.Monad.IO.Class (liftIO)
import Linear (unit, _x)
import Waterfall.Internal.Path.Common (RawPath(..))
import qualified Waterfall.Solids as Solids

-- | Construct a `Solid` of revolution from a `Path2D`.
--
-- The `Path2D` is rotated about the y axis, should have endpoints that lie on it ( \(x = 0\) ).
-- 
-- The resulting `Solid` is rotated such that the axis of revolution is the z axis.
revolution :: Path2D -> Solid
revolution (Path2D (ComplexRawPath theRawPath)) = 
    rotate (unit _x) (pi/2) . solidFromAcquire $ do
        p <- toAcquire theRawPath
        axis <- GP.oy -- revolve around the y axis
        revol <- MakeRevol.fromShapeAndAx1 (upcast p) axis True
        shell <- MakeShape.shape (upcast revol)
        solidBuilder <- MakeSolid.new
        liftIO $ MakeSolid.add solidBuilder =<< unsafeDowncast shell
        MakeShape.shape (upcast solidBuilder)
revolution _ = Solids.nowhere