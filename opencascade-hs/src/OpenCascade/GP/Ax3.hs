{-# LANGUAGE  CApiFFI #-}
module OpenCascade.GP.Ax3 
( Ax3
, new
, fromAx2
, fromPntDirAndDir
, fromPntAndDir
, xReverse
, yReverse
, zReverse
, setAxis
, setDirection
, setLocation
, setXDirection
, setYDirection
, angle
, axis
, ax2
, direction
, location
, xDirection
, yDirection
, direct
, isCoplanar
, isCoplanarAx1
, mirror
, mirrored
, mirrorAx2
, mirroredAx2
, rotate
, rotated
, scale
, scaled
, transform
, transformed
, translate
, translated
, translateRelative
, translatedRelative
) where


import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.Ptr
import Foreign.C.Types
import OpenCascade.Internal.Bool (cBoolToBool)
import Data.Acquire 
import Data.Coerce (coerce)

foreign import capi unsafe "hs_gp_Ax3.h hs_new_gp_Ax3" rawNew :: IO (Ptr Ax3)

new :: Acquire (Ptr Ax3)
new = mkAcquire rawNew deleteAx3

foreign import capi unsafe "hs_gp_Ax3.h hs_new_gp_Ax3_fromAx2" rawFromAx2 :: Ptr Ax2 -> IO (Ptr Ax3)

fromAx2 :: Ptr Ax2 -> Acquire (Ptr Ax3)
fromAx2 ax2 = mkAcquire (rawFromAx2 ax2) deleteAx3

foreign import capi unsafe "hs_gp_Ax3.h hs_new_gp_Ax3_fromPntDirAndDir" rawFromPntDirAndDir :: Ptr Pnt -> Ptr Dir -> Ptr Dir -> IO (Ptr Ax3)

fromPntDirAndDir :: Ptr Pnt -> Ptr Dir -> Ptr Dir -> Acquire (Ptr Ax3)
fromPntDirAndDir pnt u v = mkAcquire (rawFromPntDirAndDir pnt u v) deleteAx3 

foreign import capi unsafe "hs_gp_Ax3.h hs_new_gp_Ax3_fromPntAndDir" rawFromPntAndDir :: Ptr Pnt -> Ptr Dir -> IO (Ptr Ax3)

fromPntAndDir :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Ax3)
fromPntAndDir pnt dir = mkAcquire (rawFromPntAndDir pnt dir) deleteAx3 

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_xReverse" xReverse :: Ptr Ax3 -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_yReverse" yReverse :: Ptr Ax3 -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_zReverse" zReverse :: Ptr Ax3 -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_setAxis" setAxis :: Ptr Ax3 -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_setDirection" setDirection :: Ptr Ax3 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_setLocation" setLocation :: Ptr Ax3 -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_setXDirection" setXDirection :: Ptr Ax3 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_setYDirection" setYDirection :: Ptr Ax3 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_angle" rawAngle :: Ptr Ax3 -> Ptr Ax3 -> IO CDouble 

angle :: Ptr Ax3 -> Ptr Ax3 -> IO Double 
angle = coerce rawAngle

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_axis" rawAxis :: Ptr Ax3  -> IO (Ptr Ax1)

axis :: Ptr Ax3 -> Acquire (Ptr Ax1)
axis this = mkAcquire (rawAxis this) deleteAx1

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_ax2" rawAx2 :: Ptr Ax3  -> IO (Ptr Ax2)

ax2 :: Ptr Ax3 -> Acquire (Ptr Ax2)
ax2 this = mkAcquire (rawAx2 this) deleteAx2

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_direction" rawDirection :: Ptr Ax3  -> IO (Ptr Dir)

direction :: Ptr Ax3 -> Acquire (Ptr Dir)
direction this = mkAcquire (rawDirection this) deleteDir

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_location" rawLocation :: Ptr Ax3 -> IO (Ptr Pnt)

location :: Ptr Ax3 -> Acquire (Ptr Pnt)
location this = mkAcquire (rawLocation this) deletePnt

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_xDirection" rawXDirection :: Ptr Ax3  -> IO (Ptr Dir)

xDirection :: Ptr Ax3 -> Acquire (Ptr Dir)
xDirection this = mkAcquire (rawXDirection this) deleteDir

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_yDirection" rawYDirection :: Ptr Ax3  -> IO (Ptr Dir)

yDirection :: Ptr Ax3 -> Acquire (Ptr Dir)
yDirection this = mkAcquire (rawYDirection this) deleteDir

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_direct" rawDirect :: Ptr Ax3  -> IO CBool

direct :: Ptr Ax3 -> IO Bool
direct = fmap cBoolToBool . rawDirect

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_isCoplanar" rawIsCoplanar :: Ptr Ax3 -> Ptr Ax3 -> CDouble -> CDouble -> IO CBool

isCoplanar :: Ptr Ax3 -> Ptr Ax3 -> Double -> Double -> IO Bool
isCoplanar a b linearTol angularTol =  cBoolToBool <$> rawIsCoplanar a b (coerce linearTol) (coerce angularTol)

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_isCoplanarAx1" rawIsCoplanarAx1 :: Ptr Ax3 -> Ptr Ax1 -> CDouble -> CDouble -> IO CBool

isCoplanarAx1 :: Ptr Ax3 -> Ptr Ax1 -> Double -> Double -> IO Bool
isCoplanarAx1 a b linearTol angularTol =  cBoolToBool <$> rawIsCoplanarAx1 a b (coerce linearTol) (coerce angularTol)

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_mirror" mirror:: Ptr Ax3 -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_mirrored" rawMirrored:: Ptr Ax3 -> Ptr Ax1 -> IO (Ptr Ax3)

mirrored :: Ptr Ax3 -> Ptr Ax1 -> Acquire (Ptr Ax3)
mirrored axis mirrorAxis = mkAcquire (rawMirrored axis mirrorAxis) deleteAx3

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_mirror_Ax2" mirrorAx2:: Ptr Ax3 -> Ptr Ax2 -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_mirrored_Ax2" rawMirroredAx2 :: Ptr Ax3 -> Ptr Ax2 -> IO (Ptr Ax3)

mirroredAx2 :: Ptr Ax3 -> Ptr Ax2 -> Acquire (Ptr Ax3)
mirroredAx2 axis mirrorAxis = mkAcquire (rawMirroredAx2 axis mirrorAxis) deleteAx3

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_rotate" rawRotate :: Ptr Ax3 -> Ptr Ax1 -> CDouble  -> IO ()

rotate :: Ptr Ax3 -> Ptr Ax1 -> Double -> IO ()
rotate = coerce rawRotate

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_rotated" rawRotated :: Ptr Ax3 -> Ptr Ax1  -> CDouble -> IO (Ptr Ax3)

rotated :: Ptr Ax3 -> Ptr Ax1 -> Double -> Acquire (Ptr Ax3)
rotated axis rotAxis angle = mkAcquire (rawRotated axis rotAxis (coerce angle)) deleteAx3

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_scale" rawScale :: Ptr Ax3 -> Ptr Pnt -> CDouble  -> IO ()

scale :: Ptr Ax3 -> Ptr Pnt -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_scaled" rawScaled :: Ptr Ax3 -> Ptr Pnt  -> CDouble -> IO (Ptr Ax3)

scaled :: Ptr Ax3 -> Ptr Pnt -> Double -> Acquire (Ptr Ax3)
scaled axis origin factor = mkAcquire (rawScaled axis origin (coerce factor)) deleteAx3


foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_transform" transform:: Ptr Ax3 -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_transformed" rawTransformed :: Ptr Ax3 -> Ptr Trsf -> IO (Ptr Ax3)

transformed :: Ptr Ax3 -> Ptr Trsf -> Acquire (Ptr Ax3)
transformed axis trsf = mkAcquire (rawTransformed axis trsf) deleteAx3


foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_translate" translate :: Ptr Ax3 -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_translated" rawTranslated :: Ptr Ax3 -> Ptr Vec -> IO (Ptr Ax3)

translated :: Ptr Ax3 -> Ptr Vec -> Acquire (Ptr Ax3)
translated axis vec = mkAcquire (rawTranslated axis vec) deleteAx3

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_translateRelative" translateRelative :: Ptr Ax3 -> Ptr Pnt -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Ax3.h hs_gp_Ax3_translatedRelative" rawTranslatedRelative :: Ptr Ax3 -> Ptr Pnt -> Ptr Pnt -> IO (Ptr Ax3)

translatedRelative :: Ptr Ax3 -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Ax3)
translatedRelative axis from to = mkAcquire (rawTranslatedRelative axis from to) deleteAx3