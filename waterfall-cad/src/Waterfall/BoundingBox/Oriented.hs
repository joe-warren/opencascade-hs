module Waterfall.BoundingBox.Oriented
( OrientedBoundingBox
, obbCenter
, obbSideX
, obbSideY
, obbSideZ
, orientedBoundingBox
, obbToSolid
) where

import Linear (V3 (..), normalize, (^*))
import Waterfall.Internal.Solid (Solid (..), acquireSolid, solidFromAcquire)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import Waterfall.Internal.FromOpenCascade (gpXYZToV3)
import Foreign.Ptr (Ptr)
import Waterfall.Solids (volume, box)
import Waterfall.Transforms (translate)
import qualified OpenCascade.Bnd.OBB as OBB
import OpenCascade.Bnd.OBB (OBB)
import OpenCascade.GP.Types (XYZ)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Ax3 as Ax3
import qualified OpenCascade.GP.Trsf as Trsf
import qualified OpenCascade.BRepBndLib as BRepBndLib
import qualified OpenCascade.BRepBuilderAPI.Transform  as BRepBuilderAPI.Transform
import Data.Acquire (Acquire)
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))

-- | An OrientedBoundingBox may be a tighter fit for a Shape than an axis aligned bounding box would be
-- 
data OrientedBoundingBox = OrientedBoundingBox { rawOBB :: Ptr OBB }

-- | Compute an OrientedBoundingBox for a solid
orientedBoundingBox :: Solid -> Maybe OrientedBoundingBox
orientedBoundingBox s = 
    if volume s == 0
        then Nothing 
        else Just . OrientedBoundingBox . unsafeFromAcquire $ do
            obb <- OBB.new
            solid <- acquireSolid s
            liftIO $ BRepBndLib.addOBB solid obb True True True
            return obb

queryOBB :: (Ptr OBB -> Acquire (V3 Double)) -> OrientedBoundingBox -> V3 Double
queryOBB f = unsafeFromAcquire . ( f <=< toAcquire . rawOBB)

-- | The center point of an `OrientedBoundingBox`
obbCenter :: OrientedBoundingBox -> V3 Double
obbCenter = queryOBB (liftIO . gpXYZToV3 <=< OBB.center)

getSide :: (Ptr OBB -> Acquire (Ptr XYZ)) -> (Ptr OBB -> IO Double) -> OrientedBoundingBox -> V3 Double
getSide fxyz fLength = queryOBB $ \obb -> do
    side <- liftIO . gpXYZToV3 =<< fxyz obb
    len <- liftIO $ fLength obb
    return $ normalize side ^* len

-- | The "X" side of the oriented bounding box.
--
-- This is measured from the center to one Face.
-- So the length of this vector is _half_ of the side length of the bounding box.
obbSideX :: OrientedBoundingBox -> V3 Double 
obbSideX = getSide OBB.xDirection OBB.xHSize
    
-- | The "Y" side of the oriented bounding box.
--
-- This is measured from the center to one face.
-- So the length of this vector is _half_ of the side length of the bounding box.
obbSideY :: OrientedBoundingBox -> V3 Double 
obbSideY = getSide OBB.yDirection OBB.yHSize
    
-- | the "Z" side of the oriented bounding box
--
-- This is measured from the center to one face.
-- So the length of this vector is _half_ of the side length of the bounding box.
obbSideZ :: OrientedBoundingBox -> V3 Double
obbSideZ =  getSide OBB.zDirection OBB.zHSize

-- | Reify an `OrientedBoundingBox` as a `Solid`
obbToSolid :: OrientedBoundingBox -> Solid
obbToSolid obb = solidFromAcquire $ do
    obb' <- toAcquire . rawOBB $ obb
    x <- liftIO . OBB.xHSize $ obb'
    y <- liftIO . OBB.yHSize $ obb'
    z <- liftIO . OBB.zHSize $ obb'
    let halfBox = V3 x y z
    unpositioned <- acquireSolid . translate (negate halfBox) $ box (V3 x y z ^* 2)
    position <- OBB.position obb'
    o <- Ax3.fromAx2 =<< GP.xoy
    trsf <- Trsf.new
    liftIO $ Trsf.setDisplacement trsf o position
    BRepBuilderAPI.Transform.transform unpositioned trsf True
