{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Ax2d
( Ax2d
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
, mirrorAboutPnt2d
, mirroredAboutPnt2d
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

foreign import capi unsafe "hs_gp_Ax2d.h hs_new_gp_Ax2d" rawNew :: Ptr Pnt2d -> Ptr Dir2d -> IO (Ptr Ax2d)

new :: Ptr Pnt2d -> Ptr Dir2d -> Acquire (Ptr Ax2d)
new origin dir = mkAcquire (rawNew origin dir) deleteAx2d

-- getters

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Location" rawLocation :: Ptr Ax2d -> IO (Ptr Pnt2d)

location :: Ptr Ax2d -> Acquire (Ptr Pnt2d)
location ax2d = mkAcquire (rawLocation ax2d) deletePnt2d

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Direction" rawDirection :: Ptr Ax2d -> IO (Ptr Dir2d)

direction :: Ptr Ax2d -> Acquire (Ptr Dir2d)
direction ax2d = mkAcquire (rawDirection ax2d) deleteDir2d

-- setters

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_SetDirection" setDirection :: Ptr Ax2d -> Ptr Dir2d -> IO ()

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_SetLocation" setLocation :: Ptr Ax2d -> Ptr Pnt2d -> IO ()

-- tests

-- isCoaxial

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_IsCoaxial" rawIsCoaxial :: Ptr Ax2d -> Ptr Ax2d -> CDouble -> CDouble -> IO CBool

isCoaxial :: Ptr Ax2d -> Ptr Ax2d -> Double -> Double -> IO Bool
isCoaxial a b angularTolerance linearTolerance = (/= 0) <$> rawIsCoaxial a b (CDouble angularTolerance) (CDouble linearTolerance)

-- isNormal

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_IsNormal" rawIsNormal :: Ptr Ax2d -> Ptr Ax2d -> CDouble -> IO CBool

isNormal :: Ptr Ax2d -> Ptr Ax2d -> Double -> IO Bool
isNormal a b angularTolerance = (/= 0) <$> rawIsNormal a b (CDouble angularTolerance)

-- isOpposite

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_IsOpposite" rawIsOpposite :: Ptr Ax2d -> Ptr Ax2d -> CDouble -> IO CBool

isOpposite :: Ptr Ax2d -> Ptr Ax2d -> Double -> IO Bool
isOpposite a b angularTolerance = (/= 0) <$> rawIsOpposite a b (CDouble angularTolerance)

-- isParallel

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_IsParallel" rawIsParallel :: Ptr Ax2d -> Ptr Ax2d -> CDouble -> IO CBool

isParallel :: Ptr Ax2d -> Ptr Ax2d -> Double -> IO Bool
isParallel a b angularTolerance = (/= 0) <$> rawIsParallel a b (CDouble angularTolerance)

-- angle

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Angle" rawAngle :: Ptr Ax2d -> Ptr Ax2d -> IO CDouble

angle :: Ptr Ax2d -> Ptr Ax2d -> IO Double
angle = coerce rawAngle

-- reverse/reversed

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Reverse" reverse :: Ptr Ax2d -> IO ()

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Reversed" rawReversed :: Ptr Ax2d -> IO (Ptr Ax2d)

reversed :: Ptr Ax2d -> Acquire (Ptr Ax2d)
reversed axis = mkAcquire (rawReversed axis) deleteAx2d

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Mirror" mirror :: Ptr Ax2d -> Ptr Ax2d -> IO ()

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Mirrored" rawMirrored :: Ptr Ax2d -> Ptr Ax2d -> IO (Ptr Ax2d)

mirrored :: Ptr Ax2d -> Ptr Ax2d -> Acquire (Ptr Ax2d)
mirrored axis mirror = mkAcquire (rawMirrored axis mirror) deleteAx2d


foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_MirrorAboutPnt2d" mirrorAboutPnt2d :: Ptr Ax2d -> Ptr Pnt2d -> IO ()

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_MirroredAboutPnt2d" rawMirroredAboutPnt2d :: Ptr Ax2d -> Ptr Pnt2d -> IO (Ptr Ax2d)

mirroredAboutPnt2d :: Ptr Ax2d -> Ptr Pnt2d -> Acquire (Ptr Ax2d)
mirroredAboutPnt2d axis mirror = mkAcquire (rawMirroredAboutPnt2d axis mirror) deleteAx2d

-- rotate/rotated 

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Rotate" rawRotate :: Ptr Ax2d -> Ptr Pnt2d -> CDouble -> IO ()

rotate :: Ptr Ax2d -> Ptr Pnt2d -> Double -> IO ()
rotate = coerce rawRotate

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Rotated" rawRotated :: Ptr Ax2d -> Ptr Pnt2d -> CDouble -> IO (Ptr Ax2d)

rotated :: Ptr Ax2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Ax2d)
rotated axis axisOfRotation angle = mkAcquire (rawRotated axis axisOfRotation (CDouble angle)) deleteAx2d

-- scale/scaled 

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Scale" rawScale :: Ptr Ax2d -> Ptr Pnt2d -> CDouble -> IO ()

scale :: Ptr Ax2d -> Ptr Pnt2d -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Scaled" rawScaled :: Ptr Ax2d -> Ptr Pnt2d -> CDouble -> IO (Ptr Ax2d)

scaled :: Ptr Ax2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Ax2d)
scaled axis origin amount = mkAcquire (rawScaled axis origin (CDouble amount)) deleteAx2d

-- transform/transformed 

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Transform" transform :: Ptr Ax2d -> Ptr Trsf2d -> IO ()

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Transformed" rawTransformed :: Ptr Ax2d -> Ptr Trsf2d -> IO (Ptr Ax2d)

transformed :: Ptr Ax2d -> Ptr Trsf2d -> Acquire (Ptr Ax2d)
transformed axis trsf = mkAcquire (rawTransformed axis trsf) deleteAx2d


-- translate/translated

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Translate" translate :: Ptr Ax2d -> Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_Translated" rawTranslated :: Ptr Ax2d -> Ptr Vec2d -> IO (Ptr Ax2d)

translated :: Ptr Ax2d -> Ptr Vec2d -> Acquire (Ptr Ax2d)
translated axis vec = mkAcquire (rawTranslated axis vec) deleteAx2d


foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_TranslateRelative" translateRelative :: Ptr Ax2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO ()

foreign import capi unsafe "hs_gp_Ax2d.h hs_gp_Ax2d_TranslatedRelative" rawTranslatedRelative :: Ptr Ax2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO (Ptr Ax2d)

translatedRelative :: Ptr Ax2d -> Ptr Pnt2d -> Ptr Pnt2d -> Acquire (Ptr Ax2d)
translatedRelative axis from to = mkAcquire (rawTranslatedRelative axis from to) deleteAx2d

