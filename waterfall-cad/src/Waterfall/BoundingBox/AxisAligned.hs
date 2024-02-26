module Waterfall.BoundingBox.AxisAligned
( axisAlignedBoundingBox
, aabbToSolid
) where
import Waterfall.Internal.Solid(Solid, acquireSolid)
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import Waterfall.Solids (box, volume)
import Waterfall.Transforms (translate)
import Linear (V3 (..), (^-^))
import qualified OpenCascade.Bnd.Box as Bnd.Box
import qualified OpenCascade.BRepBndLib as BRepBndLib
import Control.Monad.IO.Class (liftIO)

-- | Return the smallest Axis Aligned Bounding Box (AABB) that contains the Solid.
-- 
-- If computable, the AABB is returned in the form '(lo, hi)',
-- where 'lo' is the vertex of the box with the lowest individual values,
-- and 'hi' is the vertex with the highest values.
axisAlignedBoundingBox :: Solid -> Maybe (V3 Double, V3 Double)
axisAlignedBoundingBox s =  
    if volume s <= 0
        then Nothing 
        else Just . unsafeFromAcquire $ do
            solid <- acquireSolid s
            theBox <- Bnd.Box.new
            liftIO $ BRepBndLib.addOptimal solid theBox True False
            p1 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMin theBox
            p2 <- liftIO . gpPntToV3 =<< Bnd.Box.cornerMax theBox
            return (p1, p2)

-- | A cuboid, specified by two diagonal vertices.
--  
-- This can be used to make a solid from the output of `axisAlignedBoundingBox` 
aabbToSolid :: (V3 Double, V3 Double) -- ^ if this argument input is `(lo, hi)`, one vertex of the cuboid is placed at `lo`, the oposite vertex is at `hi`
    -> Solid
aabbToSolid (lo, hi) = translate lo $ box (hi ^-^ lo)