{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Ax1
( Ax1
, new
, location
, direction
, setLocation
, setDirection
, isCoaxial
, isNormal
, isOpposite
, isParallel
, angle
, reverse
, reversed
, mirror
, mirrored
, mirrorAboutPnt
, mirroredAboutPnt
, mirrorAboutAx2
, mirroredAboutAx2
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

foreign import capi unsafe "hs_gp_Ax1.h hs_new_gp_Ax1" rawNew :: Ptr Pnt -> Ptr Dir -> IO (Ptr Ax1)

new :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Ax1)
new origin dir = mkAcquire (rawNew origin dir) deleteAx1

-- getters

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Location" rawLocation :: Ptr Ax1 -> IO (Ptr Pnt)

location :: Ptr Ax1 -> Acquire (Ptr Pnt)
location ax1 = mkAcquire (rawLocation ax1) deletePnt

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Direction" rawDirection :: Ptr Ax1 -> IO (Ptr Dir)

direction :: Ptr Ax1 -> Acquire (Ptr Dir)
direction ax1 = mkAcquire (rawDirection ax1) deleteDir

-- setters

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_SetDirection" setDirection :: Ptr Ax1 -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_SetLocation" setLocation :: Ptr Ax1 -> Ptr Pnt -> IO ()

-- tests

-- isCoaxial

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_IsCoaxial" rawIsCoaxial :: Ptr Ax1 -> Ptr Ax1 -> CDouble -> CDouble -> IO CBool

isCoaxial :: Ptr Ax1 -> Ptr Ax1 -> Double -> Double -> IO Bool
isCoaxial a b angularTolerance linearTolerance = (/= 0) <$> rawIsCoaxial a b (CDouble angularTolerance) (CDouble linearTolerance)

-- isNormal

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_IsNormal" rawIsNormal :: Ptr Ax1 -> Ptr Ax1 -> CDouble -> IO CBool

isNormal :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO Bool
isNormal a b angularTolerance = (/= 0) <$> rawIsNormal a b (CDouble angularTolerance)

-- isOpposite

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_IsOpposite" rawIsOpposite :: Ptr Ax1 -> Ptr Ax1 -> CDouble -> IO CBool

isOpposite :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO Bool
isOpposite a b angularTolerance = (/= 0) <$> rawIsOpposite a b (CDouble angularTolerance)

-- isParallel

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_IsParallel" rawIsParallel :: Ptr Ax1 -> Ptr Ax1 -> CDouble -> IO CBool

isParallel :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO Bool
isParallel a b angularTolerance = (/= 0) <$> rawIsParallel a b (CDouble angularTolerance)

-- angle

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Angle" rawAngle :: Ptr Ax1 -> Ptr Ax1 -> IO CDouble

angle :: Ptr Ax1 -> Ptr Ax1 -> IO Double
angle = coerce rawAngle

-- reverse/reversed

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Reverse" reverse :: Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Reversed" rawReversed :: Ptr Ax1 -> IO (Ptr Ax1)

reversed :: Ptr Ax1 -> Acquire (Ptr Ax1)
reversed axis = mkAcquire (rawReversed axis) deleteAx1

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Mirror" mirror :: Ptr Ax1 -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Mirrored" rawMirrored :: Ptr Ax1 -> Ptr Ax1 -> IO (Ptr Ax1)

mirrored :: Ptr Ax1 -> Ptr Ax1 -> Acquire (Ptr Ax1)
mirrored axis mirror = mkAcquire (rawMirrored axis mirror) deleteAx1


foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_MirrorAboutPnt" mirrorAboutPnt :: Ptr Ax1 -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_MirroredAboutPnt" rawMirroredAboutPnt :: Ptr Ax1 -> Ptr Pnt -> IO (Ptr Ax1)

mirroredAboutPnt :: Ptr Ax1 -> Ptr Pnt -> Acquire (Ptr Ax1)
mirroredAboutPnt axis mirror = mkAcquire (rawMirroredAboutPnt axis mirror) deleteAx1


foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_MirrorAboutAx2" mirrorAboutAx2 :: Ptr Ax1 -> Ptr Ax2 -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_MirroredAboutAx2" rawMirroredAboutAx2 :: Ptr Ax1 -> Ptr Ax2 -> IO (Ptr Ax1)

mirroredAboutAx2 :: Ptr Ax1 -> Ptr Ax2 -> Acquire (Ptr Ax1)
mirroredAboutAx2 axis mirror = mkAcquire (rawMirroredAboutAx2 axis mirror) deleteAx1

-- rotate/rotated 

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Rotate" rawRotate :: Ptr Ax1 -> Ptr Ax1 -> CDouble -> IO ()

rotate :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO ()
rotate = coerce rawRotate

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Rotated" rawRotated :: Ptr Ax1 -> Ptr Ax1 -> CDouble -> IO (Ptr Ax1)

rotated :: Ptr Ax1 -> Ptr Ax1 -> Double -> Acquire (Ptr Ax1)
rotated axis axisOfRotation angle = mkAcquire (rawRotated axis axisOfRotation (CDouble angle)) deleteAx1

-- scale/scaled 

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Scale" rawScale :: Ptr Ax1 -> Ptr Pnt -> CDouble -> IO ()

scale :: Ptr Ax1 -> Ptr Pnt -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Scaled" rawScaled :: Ptr Ax1 -> Ptr Pnt -> CDouble -> IO (Ptr Ax1)

scaled :: Ptr Ax1 -> Ptr Pnt -> Double -> Acquire (Ptr Ax1)
scaled axis origin amount = mkAcquire (rawScaled axis origin (CDouble amount)) deleteAx1

-- transform/transformed 

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Transform" transform :: Ptr Ax1 -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Transformed" rawTransformed :: Ptr Ax1 -> Ptr Trsf -> IO (Ptr Ax1)

transformed :: Ptr Ax1 -> Ptr Trsf -> Acquire (Ptr Ax1)
transformed axis trsf = mkAcquire (rawTransformed axis trsf) deleteAx1


-- translate/translated

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Translate" translate :: Ptr Ax1 -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_Translated" rawTranslated :: Ptr Ax1 -> Ptr Vec -> IO (Ptr Ax1)

translated :: Ptr Ax1 -> Ptr Vec -> Acquire (Ptr Ax1)
translated axis vec = mkAcquire (rawTranslated axis vec) deleteAx1


foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_TranslateRelative" translateRelative :: Ptr Ax1 -> Ptr Pnt -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Ax1.h hs_gp_Ax1_TranslatedRelative" rawTranslatedRelative :: Ptr Ax1 -> Ptr Pnt -> Ptr Pnt -> IO (Ptr Ax1)

translatedRelative :: Ptr Ax1 -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Ax1)
translatedRelative axis from to = mkAcquire (rawTranslatedRelative axis from to) deleteAx1
