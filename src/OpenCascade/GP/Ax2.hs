{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Ax2
( Ax2
, new
, newAutoX
, location
, direction
, xDirection
, yDirection
, axis
, setLocation
, setDirection
, setXDirection
, setYDirection
, setAxis
, isCoplanar
, isCoplanarWithAx1
, mirror
, mirrored
, mirrorAboutPnt
, mirroredAboutPnt
, mirrorAboutAx1
, mirroredAboutAx1
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

import Prelude hiding (reverse)
import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.Ptr
import Foreign.C.Types
import Data.Acquire 
import Data.Coerce (coerce)

-- new and delete

foreign import capi unsafe "hs_gp_Ax2.h hs_new_gp_Ax2" rawNew :: Ptr Pnt -> Ptr Dir -> Ptr Dir -> IO (Ptr Ax2)

new :: Ptr Pnt -> Ptr Dir -> Ptr Dir -> Acquire (Ptr Ax2)
new origin axis vX = mkAcquire (rawNew origin axis vX) deleteAx2

foreign import capi unsafe "hs_gp_Ax2.h hs_new_gp_Ax2_autoX" rawNewAutoX :: Ptr Pnt -> Ptr Dir -> IO (Ptr Ax2)

newAutoX :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Ax2)
newAutoX origin axis = mkAcquire (rawNewAutoX origin axis) deleteAx2


-- getters

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Location" rawLocation :: Ptr Ax2 -> IO (Ptr Pnt)

location :: Ptr Ax2 -> Acquire (Ptr Pnt)
location ax2 = mkAcquire (rawLocation ax2) deletePnt

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Direction" rawDirection :: Ptr Ax2 -> IO (Ptr Dir)

direction :: Ptr Ax2 -> Acquire (Ptr Dir)
direction ax2 = mkAcquire (rawDirection ax2) deleteDir

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_XDirection" rawXDirection :: Ptr Ax2 -> IO (Ptr Dir)

xDirection :: Ptr Ax2 -> Acquire (Ptr Dir)
xDirection ax2 = mkAcquire (rawXDirection ax2) deleteDir

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_YDirection" rawYDirection :: Ptr Ax2 -> IO (Ptr Dir)

yDirection :: Ptr Ax2 -> Acquire (Ptr Dir)
yDirection ax2 = mkAcquire (rawYDirection ax2) deleteDir

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Axis" rawAxis :: Ptr Ax2 -> IO (Ptr Ax1)

axis :: Ptr Ax2 -> Acquire (Ptr Ax1)
axis ax2 = mkAcquire (rawAxis ax2) deleteAx1

-- setters

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_SetDirection" setDirection :: Ptr Ax2 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_SetLocation" setLocation :: Ptr Ax2 -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_SetXDirection" setXDirection :: Ptr Ax2 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_SetYDirection" setYDirection :: Ptr Ax2 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_SetAxis" setAxis :: Ptr Ax2 -> Ptr Ax1 -> IO ()

-- isCoplanar

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_IsCoplanar" rawIsCoplanar :: Ptr Ax2 -> Ptr Ax2 -> CDouble -> CDouble -> IO CBool

isCoplanar :: Ptr Ax2 -> Ptr Ax2 -> Double -> Double -> IO Bool
isCoplanar a b linearTolerance angularTolerance = (/= 0) <$> rawIsCoplanar a b (CDouble linearTolerance) (CDouble angularTolerance)

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_IsCoplanarWithAx1" rawIsCoplanarWithAx1 :: Ptr Ax2 -> Ptr Ax1 -> CDouble -> CDouble -> IO CBool

isCoplanarWithAx1 :: Ptr Ax2 -> Ptr Ax1 -> Double -> Double -> IO Bool
isCoplanarWithAx1 a b linearTolerance angularTolerance = (/= 0) <$> rawIsCoplanarWithAx1 a b (CDouble linearTolerance) (CDouble angularTolerance)

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Mirror" mirror :: Ptr Ax2 -> Ptr Ax2 -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Mirrored" rawMirrored :: Ptr Ax2 -> Ptr Ax2 -> IO (Ptr Ax2)

mirrored :: Ptr Ax2 -> Ptr Ax2 -> Acquire (Ptr Ax2)
mirrored axis mirror = mkAcquire (rawMirrored axis mirror) deleteAx2


foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_MirrorAboutPnt" mirrorAboutPnt :: Ptr Ax2 -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_MirroredAboutPnt" rawMirroredAboutPnt :: Ptr Ax2 -> Ptr Pnt -> IO (Ptr Ax2)

mirroredAboutPnt :: Ptr Ax2 -> Ptr Pnt -> Acquire (Ptr Ax2)
mirroredAboutPnt axis mirror = mkAcquire (rawMirroredAboutPnt axis mirror) deleteAx2


foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_MirrorAboutAx1" mirrorAboutAx1 :: Ptr Ax2 -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_MirroredAboutAx1" rawMirroredAboutAx1 :: Ptr Ax2 -> Ptr Ax1 -> IO (Ptr Ax2)

mirroredAboutAx1 :: Ptr Ax2 -> Ptr Ax1 -> Acquire (Ptr Ax2)
mirroredAboutAx1 axis mirror = mkAcquire (rawMirroredAboutAx1 axis mirror) deleteAx2


-- rotate/rotated 

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Rotate" rawRotate :: Ptr Ax2 -> Ptr Ax1 -> CDouble -> IO ()

rotate :: Ptr Ax2 -> Ptr Ax1 -> Double -> IO ()
rotate = coerce rawRotate

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Rotated" rawRotated :: Ptr Ax2 -> Ptr Ax1 -> CDouble -> IO (Ptr Ax2)

rotated :: Ptr Ax2 -> Ptr Ax1 -> Double -> Acquire (Ptr Ax2)
rotated axis axisOfRotation angle = mkAcquire (rawRotated axis axisOfRotation (CDouble angle)) deleteAx2

-- scale/scaled 

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Scale" rawScale :: Ptr Ax2 -> Ptr Pnt -> CDouble -> IO ()

scale :: Ptr Ax2 -> Ptr Pnt -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Scaled" rawScaled :: Ptr Ax2 -> Ptr Pnt -> CDouble -> IO (Ptr Ax2)

scaled :: Ptr Ax2 -> Ptr Pnt -> Double -> Acquire (Ptr Ax2)
scaled axis origin amount = mkAcquire (rawScaled axis origin (CDouble amount)) deleteAx2

-- transform/transformed 

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Transform" transform :: Ptr Ax2 -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Transformed" rawTransformed :: Ptr Ax2 -> Ptr Trsf -> IO (Ptr Ax2)

transformed :: Ptr Ax2 -> Ptr Trsf -> Acquire (Ptr Ax2)
transformed axis trsf = mkAcquire (rawTransformed axis trsf) deleteAx2


-- translate/translated

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Translate" translate :: Ptr Ax2 -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_Translated" rawTranslated :: Ptr Ax2 -> Ptr Vec -> IO (Ptr Ax2)

translated :: Ptr Ax2 -> Ptr Vec -> Acquire (Ptr Ax2)
translated axis vec = mkAcquire (rawTranslated axis vec) deleteAx2


foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_TranslateRelative" translateRelative :: Ptr Ax2 -> Ptr Pnt -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Ax2.h hs_gp_Ax2_TranslatedRelative" rawTranslatedRelative :: Ptr Ax2 -> Ptr Pnt -> Ptr Pnt -> IO (Ptr Ax2)

translatedRelative :: Ptr Ax2 -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Ax2)
translatedRelative axis from to = mkAcquire (rawTranslatedRelative axis from to) deleteAx2

