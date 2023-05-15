{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Pnt 
( Pnt
, new
, getX
, getY
, getZ 
, setX
, setY 
, setZ
, distance
, squareDistance
, baryCenter
, isEqual
, mirror
, mirrored
, mirror1
, mirrored1
, mirror2
, mirrored2
, rotate
, rotated
, transform
, transformed
, translate
, translated
, translateRelative
, translatedRelative
) where

import OpenCascade.GP.Types
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 

-- new and delete

foreign import capi unsafe "bottle.h hs_new_gp_Pnt" rawNew :: CDouble -> CDouble -> CDouble -> IO (Ptr Pnt)

foreign import capi unsafe "bottle.h hs_delete_gp_Pnt" delete :: Ptr Pnt -> IO ()

new :: Double -> Double -> Double -> Acquire (Ptr Pnt)
new x y z = mkAcquire (rawNew (CDouble x) (CDouble y) (CDouble z)) delete

-- getters

foreign import capi unsafe "bottle.h hs_gp_Pnt_X" rawX :: Ptr Pnt -> IO (CDouble)

getX :: Ptr Pnt -> IO Double
getX = coerce rawX

foreign import capi unsafe "bottle.h hs_gp_Pnt_Y" rawY :: Ptr Pnt -> IO (CDouble)

getY :: Ptr Pnt -> IO Double
getY = coerce rawY

foreign import capi unsafe "bottle.h hs_gp_Pnt_Z" rawZ :: Ptr Pnt -> IO (CDouble)

getZ :: Ptr Pnt -> IO Double
getZ = coerce rawZ

-- setters

foreign import capi unsafe "bottle.h hs_gp_Pnt_SetX" rawSetX :: Ptr Pnt -> CDouble -> IO ()

setX :: Ptr Pnt -> Double -> IO ()
setX = coerce rawSetX


foreign import capi unsafe "bottle.h hs_gp_Pnt_SetY" rawSetY :: Ptr Pnt -> CDouble -> IO ()

setY :: Ptr Pnt -> Double -> IO ()
setY = coerce rawSetY


foreign import capi unsafe "bottle.h hs_gp_Pnt_SetZ" rawSetZ :: Ptr Pnt -> CDouble -> IO ()

setZ :: Ptr Pnt -> Double -> IO ()
setZ = coerce rawSetZ


foreign import capi unsafe "bottle.h hs_gp_Pnt_Distance" rawDistance :: Ptr Pnt -> Ptr Pnt -> IO CDouble


-- distance and quadrance

distance :: Ptr Pnt -> Ptr Pnt -> IO Double
distance = coerce rawDistance


foreign import capi unsafe "bottle.h hs_gp_Pnt_SquareDistance" rawSquareDistance :: Ptr Pnt -> Ptr Pnt -> IO CDouble

squareDistance :: Ptr Pnt -> Ptr Pnt -> IO Double
squareDistance = coerce rawSquareDistance

-- baryCenter

foreign import capi unsafe "bottle.h hs_gp_Pnt_BaryCenter" rawBaryCenter :: Ptr Pnt -> CDouble -> Ptr Pnt -> CDouble -> IO ()

baryCenter :: Ptr Pnt -> Double -> Ptr Pnt -> Double -> IO ()
baryCenter = coerce rawBaryCenter

-- isEqual

foreign import capi unsafe "bottle.h hs_gp_Pnt_IsEqual" rawIsEqual :: Ptr Pnt -> Ptr Pnt -> CDouble -> IO CBool

isEqual :: Ptr Pnt -> Ptr Pnt -> Double -> IO Bool
isEqual a b tolerance = (/= 0) <$> rawIsEqual a b (CDouble tolerance)

-- mirror/mirrored

foreign import capi unsafe "bottle.h hs_gp_Pnt_Mirror" mirror :: Ptr Pnt -> Ptr Pnt -> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_Mirrored" rawMirrored :: Ptr Pnt -> Ptr Pnt -> IO (Ptr Pnt)

mirrored :: Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Pnt)
mirrored point axis = mkAcquire (rawMirrored point axis) delete

foreign import capi unsafe "bottle.h hs_gp_Pnt_Mirror1" mirror1 :: Ptr Pnt -> Ptr Ax1 -> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_Mirrored1" rawMirrored1 :: Ptr Pnt -> Ptr Ax1 -> IO (Ptr Pnt)

mirrored1 :: Ptr Pnt -> Ptr Ax1 -> Acquire (Ptr Pnt)
mirrored1 point axis = mkAcquire (rawMirrored1 point axis) delete

foreign import capi unsafe "bottle.h hs_gp_Pnt_Mirror2" mirror2 :: Ptr Pnt -> Ptr Ax2 -> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_Mirrored2" rawMirrored2 :: Ptr Pnt -> Ptr Ax2 -> IO (Ptr Pnt)

mirrored2 :: Ptr Pnt -> Ptr Ax2 -> Acquire (Ptr Pnt)
mirrored2 point axis = mkAcquire (rawMirrored2 point axis) delete

-- rotate/rotated

foreign import capi unsafe "bottle.h hs_gp_Pnt_Rotate" rotate :: Ptr Pnt -> Ptr Ax2 -> CDouble-> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_Rotated" rawRotated :: Ptr Pnt -> Ptr Ax2 -> CDouble -> IO (Ptr Pnt)

rotated :: Ptr Pnt -> Ptr Ax2 -> Double -> Acquire (Ptr Pnt)
rotated point axis amount = mkAcquire (rawRotated point axis (CDouble amount)) delete

-- scale/scaled

foreign import capi unsafe "bottle.h hs_gp_Pnt_Scale" scale :: Ptr Pnt -> Ptr Pnt -> CDouble-> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_Scaled" rawScaled :: Ptr Pnt -> Ptr Pnt -> CDouble -> IO (Ptr Pnt)

scaled :: Ptr Pnt -> Ptr Pnt -> Double -> Acquire (Ptr Pnt)
scaled point origin amount = mkAcquire (rawScaled point origin (CDouble amount)) delete

-- transform/transformed

foreign import capi unsafe "bottle.h hs_gp_Pnt_Transform" transform :: Ptr Pnt -> Ptr Trsf -> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_Transformed" rawTransformed :: Ptr Pnt -> Ptr Trsf -> IO (Ptr Pnt)

transformed :: Ptr Pnt -> Ptr Trsf -> Acquire (Ptr Pnt)
transformed point trsf = mkAcquire (rawTransformed point trsf) delete

-- translate/translated

foreign import capi unsafe "bottle.h hs_gp_Pnt_Translate" translate :: Ptr Pnt -> Ptr Vec -> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_Translated" rawTranslated :: Ptr Pnt -> Ptr Vec -> IO (Ptr Pnt)

translated :: Ptr Pnt -> Ptr Vec -> Acquire (Ptr Pnt)
translated point vec = mkAcquire (rawTranslated point vec) delete

-- translateRelative/translatedRelative

foreign import capi unsafe "bottle.h hs_gp_Pnt_TranslateRelative" translateRelative :: Ptr Pnt -> Ptr Pnt -> Ptr Pnt -> IO ()

foreign import capi unsafe "bottle.h hs_gp_Pnt_TranslatedRelative" rawTranslatedRelative :: Ptr Pnt -> Ptr Pnt -> Ptr Pnt -> IO (Ptr Pnt)

translatedRelative :: Ptr Pnt -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Pnt)
translatedRelative point from to = mkAcquire (rawTranslatedRelative point from to) delete
