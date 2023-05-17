{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Pnt2d 
( Pnt2d
, new
, getX
, getY
, setX
, setY 
, distance
, squareDistance
, isEqual
, mirror
, mirrored
, mirrorAboutAx2d
, mirroredAboutAx2d
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
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 

-- new

foreign import capi unsafe "hs_gp_Pnt2d.h hs_new_gp_Pnt2d" rawNew :: CDouble -> CDouble -> IO (Ptr Pnt2d)

new :: Double -> Double -> Acquire (Ptr Pnt2d)
new x y = mkAcquire (rawNew (CDouble x) (CDouble y)) deletePnt2d

-- getters

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_X" rawX :: Ptr Pnt2d -> IO (CDouble)

getX :: Ptr Pnt2d -> IO Double
getX = coerce rawX

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Y" rawY :: Ptr Pnt2d -> IO (CDouble)

getY :: Ptr Pnt2d -> IO Double
getY = coerce rawY

-- setters

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_SetX" rawSetX :: Ptr Pnt2d -> CDouble -> IO ()

setX :: Ptr Pnt2d -> Double -> IO ()
setX = coerce rawSetX


foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_SetY" rawSetY :: Ptr Pnt2d -> CDouble -> IO ()

setY :: Ptr Pnt2d -> Double -> IO ()
setY = coerce rawSetY

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Distance" rawDistance :: Ptr Pnt2d -> Ptr Pnt2d -> IO CDouble

-- distance and quadrance

distance :: Ptr Pnt2d -> Ptr Pnt2d -> IO Double
distance = coerce rawDistance


foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_SquareDistance" rawSquareDistance :: Ptr Pnt2d -> Ptr Pnt2d -> IO CDouble

squareDistance :: Ptr Pnt2d -> Ptr Pnt2d -> IO Double
squareDistance = coerce rawSquareDistance

-- isEqual

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_IsEqual" rawIsEqual :: Ptr Pnt2d -> Ptr Pnt2d -> CDouble -> IO CBool

isEqual :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> IO Bool
isEqual a b tolerance = (/= 0) <$> rawIsEqual a b (CDouble tolerance)

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Mirror" mirror :: Ptr Pnt2d -> Ptr Pnt2d -> IO ()

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Mirrored" rawMirrored :: Ptr Pnt2d -> Ptr Pnt2d -> IO (Ptr Pnt2d)

mirrored :: Ptr Pnt2d -> Ptr Pnt2d -> Acquire (Ptr Pnt2d)
mirrored point axis = mkAcquire (rawMirrored point axis) deletePnt2d

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_MirrorAboutAx2d" mirrorAboutAx2d :: Ptr Pnt2d -> Ptr Ax2d -> IO ()

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_MirroredAboutAx2d" rawMirroredAboutAx2d :: Ptr Pnt2d -> Ptr Ax2d -> IO (Ptr Pnt2d)

mirroredAboutAx2d :: Ptr Pnt2d -> Ptr Ax2d -> Acquire (Ptr Pnt2d)
mirroredAboutAx2d point axis = mkAcquire (rawMirroredAboutAx2d point axis) deletePnt2d

-- rotate/rotated

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Rotate" rotate :: Ptr Pnt2d -> Ptr Pnt2d -> CDouble-> IO ()

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Rotated" rawRotated :: Ptr Pnt2d -> Ptr Pnt2d -> CDouble -> IO (Ptr Pnt2d)

rotated :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Pnt2d)
rotated point axis amount = mkAcquire (rawRotated point axis (CDouble amount)) deletePnt2d

-- scale/scaled

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Scale" scale :: Ptr Pnt2d -> Ptr Pnt2d -> CDouble-> IO ()

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Scaled" rawScaled :: Ptr Pnt2d -> Ptr Pnt2d -> CDouble -> IO (Ptr Pnt2d)

scaled :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Pnt2d)
scaled point origin amount = mkAcquire (rawScaled point origin (CDouble amount)) deletePnt2d

-- transform/transformed

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Transform" transform :: Ptr Pnt2d -> Ptr Trsf2d -> IO ()

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Transformed" rawTransformed :: Ptr Pnt2d -> Ptr Trsf2d -> IO (Ptr Pnt2d)

transformed :: Ptr Pnt2d -> Ptr Trsf2d -> Acquire (Ptr Pnt2d)
transformed point trsf = mkAcquire (rawTransformed point trsf) deletePnt2d

-- translate/translated

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Translate" translate :: Ptr Pnt2d -> Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_Translated" rawTranslated :: Ptr Pnt2d -> Ptr Vec2d -> IO (Ptr Pnt2d)

translated :: Ptr Pnt2d -> Ptr Vec2d -> Acquire (Ptr Pnt2d)
translated point vec = mkAcquire (rawTranslated point vec) deletePnt2d

-- translateRelative/translatedRelative

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_TranslateRelative" translateRelative :: Ptr Pnt2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO ()

foreign import capi unsafe "hs_gp_Pnt2d.h hs_gp_Pnt2d_TranslatedRelative" rawTranslatedRelative :: Ptr Pnt2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO (Ptr Pnt2d)

translatedRelative :: Ptr Pnt2d -> Ptr Pnt2d -> Ptr Pnt2d -> Acquire (Ptr Pnt2d)
translatedRelative point from to = mkAcquire (rawTranslatedRelative point from to) deletePnt2d

