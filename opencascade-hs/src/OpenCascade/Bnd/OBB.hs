{-# LANGUAGE CApiFFI #-}
module OpenCascade.Bnd.OBB
( OBB
, new
, center
, xDirection, yDirection, zDirection
, xHSize, yHSize, zHSize
, position
) where

import OpenCascade.Bnd.Types
import OpenCascade.Bnd.Internal.Destructors (deleteOBB)
import OpenCascade.GP.Types (XYZ, Ax3)
import OpenCascade.GP.Internal.Destructors (deleteXYZ, deleteAx3)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..))
import Data.Coerce (coerce)

foreign import capi unsafe "hs_Bnd_OBB.h hs_new_Bnd_OBB" rawNew ::  IO (Ptr OBB)

new :: Acquire (Ptr OBB)
new = mkAcquire rawNew deleteOBB

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_center" rawCenter :: Ptr OBB -> IO (Ptr XYZ)

center :: Ptr OBB -> Acquire (Ptr XYZ)
center obb = mkAcquire (rawCenter obb) deleteXYZ

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_xDirection" rawXDirection :: Ptr OBB -> IO (Ptr XYZ)

xDirection :: Ptr OBB -> Acquire (Ptr XYZ)
xDirection obb = mkAcquire (rawXDirection obb) deleteXYZ

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_yDirection" rawYDirection :: Ptr OBB -> IO (Ptr XYZ)

yDirection :: Ptr OBB -> Acquire (Ptr XYZ)
yDirection obb = mkAcquire (rawYDirection obb) deleteXYZ

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_zDirection" rawZDirection :: Ptr OBB -> IO (Ptr XYZ)

zDirection :: Ptr OBB -> Acquire (Ptr XYZ)
zDirection obb = mkAcquire (rawZDirection obb) deleteXYZ

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_xHSize" rawXHSize :: Ptr OBB -> IO (CDouble)

xHSize :: Ptr OBB -> IO Double 
xHSize = coerce rawXHSize

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_yHSize" rawYHSize :: Ptr OBB -> IO (CDouble)

yHSize :: Ptr OBB -> IO Double 
yHSize = coerce rawYHSize

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_zHSize" rawZHSize :: Ptr OBB -> IO (CDouble)

zHSize :: Ptr OBB -> IO Double 
zHSize = coerce rawZHSize 

foreign import capi unsafe "hs_Bnd_OBB.h hs_Bnd_OBB_position" rawPosition :: Ptr OBB -> IO (Ptr Ax3)

position :: Ptr OBB -> Acquire (Ptr Ax3)
position obb = mkAcquire (rawPosition obb) deleteAx3