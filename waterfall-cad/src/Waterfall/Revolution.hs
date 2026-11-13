module Waterfall.Revolution
( revolution
, revolutionByAngle
, tryRevolution
, tryRevolutionByAngle
) where

import Waterfall.Internal.Solid (Solid (..), solidFromAcquireWithCatch, emptyPaintMap)
import Waterfall.TwoD.Internal.Shape (Shape(Shape), acquireShape)
import Waterfall.TwoD.Shape as Shape
import Waterfall.Internal.Finalizers (acquireForeignPtr, unsafeFromAcquire)
import qualified OpenCascade.BRepPrimAPI.MakeRevol as MakeRevol
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.GP as GP
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Waterfall.Transforms (rotate)
import Waterfall.Error (WaterfallError)
import Control.Monad.IO.Class (liftIO)
import Linear (unit, _x, _y, _xy, V2 (V2))
import Data.Either (fromRight)
import qualified OpenCascade.Bnd.Box as Bnd.Box
import qualified OpenCascade.BRepBndLib as BRepBndLib
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import Control.Lens ((^.), (&))
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import Waterfall.Booleans (intersection, Boolean (difference))

splitShapeAboutAxis :: Shape -> [Shape]
splitShapeAboutAxis shape = unsafeFromAcquire $ do
    s <- acquireShape shape 
    bnd <- Bnd.Box.new
    liftIO $ BRepBndLib.addOptimal s bnd True False
    isVoid <- liftIO $ Bnd.Box.isVoid bnd
    if isVoid 
        then return []
        else do
            p1 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMin bnd
            p2 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMax bnd
            return $ if p2 ^. _x < 0 || p1 ^. _x > 0
                then [shape]
                else let lo = V2 0 (p1 ^. _y)
                         hi = (p2 ^. _xy) 
                         mask = Shape.unitSquare
                            & TwoD.Transforms.scale2D (hi - lo)
                            & TwoD.Transforms.translate2D (lo)
                    in [shape `intersection` mask, shape `difference` mask]

tryOneRevolutionByAngle :: Double -> Shape -> Either WaterfallError Solid
tryOneRevolutionByAngle angle (Shape shape) =
    fmap (rotate (unit _x) (pi/2)) . solidFromAcquireWithCatch emptyPaintMap $ do
        s <- acquireForeignPtr shape
        axis <- GP.oy -- revolve around the y axis
        revol <- MakeRevol.fromShapeAx1AndAngle s axis angle True
        MakeShape.shape (upcast revol)

-- | Version of `revolutionByAngle` that returns a `WaterfallError` on failure.
tryRevolutionByAngle :: Double -> Shape -> Either WaterfallError Solid
tryRevolutionByAngle angle =
    fmap mconcat . traverse (tryOneRevolutionByAngle angle) . splitShapeAboutAxis

-- | Version of `revolution` that returns a `WaterfallError` on failure.
tryRevolution :: Shape -> Either WaterfallError Solid
tryRevolution = tryRevolutionByAngle (2*pi)

-- | Construct a `Solid` of revolution from a `Shape`.
--
-- The `Shape` is rotated about the y axis
--
-- The resulting `Solid` is rotated such that the axis of revolution is the z axis.
revolution :: Shape -> Solid
revolution = fromRight mempty . tryRevolution

-- | Construct a Partial Revolution of a `Shape`
--
-- The `Shape is rotated about the y axis, 
-- and then rotated such that the axis of revolution is in the z-axis,
-- the start of the wedge lies in the xz plane, 
-- and the wedge direction is counter-clockwise about the z-axis.
revolutionByAngle :: Double -> Shape -> Solid
revolutionByAngle = (fromRight mempty .) . tryRevolutionByAngle