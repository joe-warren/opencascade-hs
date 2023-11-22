{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Dir2d
( Dir2d
, new
, getX
, getY
, setX
, setY 
, isEqual
, isOpposite
, isNormal
, isParallel
, angle
, crossed
, dot
, reverse
, reversed
, mirror
, mirrored
, mirrorAboutAx2d
, mirroredAboutAx2d
, rotate
, rotated
, transform
, transformed
) where


import Prelude hiding (reverse)
import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 

-- new

foreign import capi unsafe "hs_gp_Dir2d.h hs_new_gp_Dir2d" rawNew :: CDouble -> CDouble -> IO (Ptr Dir2d)

new :: Double -> Double -> Acquire (Ptr Dir2d)
new x y = mkAcquire (rawNew (CDouble x) (CDouble y)) deleteDir2d

-- getters

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_X" rawX :: Ptr Dir2d -> IO (CDouble)

getX :: Ptr Dir2d -> IO Double
getX = coerce rawX

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Y" rawY :: Ptr Dir2d -> IO (CDouble)

getY :: Ptr Dir2d -> IO Double
getY = coerce rawY

-- setters

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_SetX" rawSetX :: Ptr Dir2d -> CDouble -> IO ()

setX :: Ptr Dir2d -> Double -> IO ()
setX = coerce rawSetX


foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_SetY" rawSetY :: Ptr Dir2d -> CDouble -> IO ()

setY :: Ptr Dir2d -> Double -> IO ()
setY = coerce rawSetY

-- tests

-- isEqual

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_IsEqual" rawIsEqual :: Ptr Dir2d -> Ptr Dir2d -> CDouble -> IO CBool

isEqual :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isEqual a b tolerance = (/= 0) <$> rawIsEqual a b (CDouble tolerance)

-- isNormal

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_IsNormal" rawIsNormal :: Ptr Dir2d -> Ptr Dir2d -> CDouble -> IO CBool

isNormal :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isNormal a b tolerance = (/= 0) <$> rawIsNormal a b (CDouble tolerance)


-- isOpposite

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_IsOpposite" rawIsOpposite :: Ptr Dir2d -> Ptr Dir2d -> CDouble -> IO CBool

isOpposite :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isOpposite a b tolerance = (/= 0) <$> rawIsOpposite a b (CDouble tolerance)

-- isParallel

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_IsParallel" rawIsParallel :: Ptr Dir2d -> Ptr Dir2d -> CDouble -> IO CBool

isParallel :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isParallel a b tolerance = (/= 0) <$> rawIsParallel a b (CDouble tolerance)

-- angle

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Angle" rawAngle :: Ptr Dir2d -> Ptr Dir2d -> IO CDouble

angle :: Ptr Dir2d -> Ptr Dir2d -> IO Double
angle = coerce rawAngle

-- crossed

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Crossed" rawCrossed :: Ptr Dir2d -> Ptr Dir2d -> IO CDouble

crossed :: Ptr Dir2d -> Ptr Dir2d -> IO Double
crossed = coerce rawCrossed

-- dot

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Dot" rawDot :: Ptr Dir2d -> Ptr Dir2d -> IO CDouble

dot :: Ptr Dir2d -> Ptr Dir2d -> IO Double
dot = coerce rawDot

-- reverse/reversed

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Reverse" reverse :: Ptr Dir2d -> IO ()

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Reversed" rawReversed :: Ptr Dir2d -> IO (Ptr Dir2d)

reversed :: Ptr Dir2d -> Acquire (Ptr Dir2d)
reversed axis = mkAcquire (rawReversed axis) deleteDir2d

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Mirror" mirror :: Ptr Dir2d -> Ptr Dir2d -> IO ()

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Mirrored" rawMirrored :: Ptr Dir2d -> Ptr Dir2d -> IO (Ptr Dir2d)

mirrored :: Ptr Dir2d -> Ptr Dir2d -> Acquire (Ptr Dir2d)
mirrored point axis = mkAcquire (rawMirrored point axis) deleteDir2d

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_MirrorAboutAx2d" mirrorAboutAx2d :: Ptr Dir2d -> Ptr Ax2d -> IO ()

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_MirroredAboutAx2d" rawMirroredAboutAx2d :: Ptr Dir2d -> Ptr Ax2d -> IO (Ptr Dir2d)

mirroredAboutAx2d :: Ptr Dir2d -> Ptr Ax2d -> Acquire (Ptr Dir2d)
mirroredAboutAx2d point axis = mkAcquire (rawMirroredAboutAx2d point axis) deleteDir2d

-- rotate/rotated

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Rotate" rotate :: Ptr Dir2d -> CDouble-> IO ()

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Rotated" rawRotated :: Ptr Dir2d -> CDouble -> IO (Ptr Dir2d)

rotated :: Ptr Dir2d -> Double -> Acquire (Ptr Dir2d)
rotated point amount = mkAcquire (rawRotated point (CDouble amount)) deleteDir2d

-- transform/transformed

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Transform" transform :: Ptr Dir2d -> Ptr Trsf2d -> IO ()

foreign import capi unsafe "hs_gp_Dir2d.h hs_gp_Dir2d_Transformed" rawTransformed :: Ptr Dir2d -> Ptr Trsf2d -> IO (Ptr Dir2d)

transformed :: Ptr Dir2d -> Ptr Trsf2d -> Acquire (Ptr Dir2d)
transformed point trsf = mkAcquire (rawTransformed point trsf) deleteDir2d
