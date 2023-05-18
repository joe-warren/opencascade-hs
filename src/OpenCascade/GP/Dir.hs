{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Dir 
( Dir
, new
, getX
, getY
, getZ 
, setX
, setY 
, setZ
, isEqual
, isOpposite
, isNormal
, isParallel
, angle
, angleWithRef
, cross
, crossed
, crossCross
, crossCrossed
, dot
, dotCross
, reverse
, reversed
, mirror
, mirrored
, mirrorAboutAx1
, mirroredAboutAx1
, mirrorAboutAx2
, mirroredAboutAx2
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

foreign import capi unsafe "hs_gp_Dir.h hs_new_gp_Dir" rawNew :: CDouble -> CDouble -> CDouble -> IO (Ptr Dir)

new :: Double -> Double -> Double -> Acquire (Ptr Dir)
new x y z = mkAcquire (rawNew (CDouble x) (CDouble y) (CDouble z)) deleteDir

-- getters

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_X" rawX :: Ptr Dir -> IO (CDouble)

getX :: Ptr Dir -> IO Double
getX = coerce rawX

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Y" rawY :: Ptr Dir -> IO (CDouble)

getY :: Ptr Dir -> IO Double
getY = coerce rawY

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Z" rawZ :: Ptr Dir -> IO (CDouble)

getZ :: Ptr Dir -> IO Double
getZ = coerce rawZ

-- setters

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_SetX" rawSetX :: Ptr Dir -> CDouble -> IO ()

setX :: Ptr Dir -> Double -> IO ()
setX = coerce rawSetX


foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_SetY" rawSetY :: Ptr Dir -> CDouble -> IO ()

setY :: Ptr Dir -> Double -> IO ()
setY = coerce rawSetY


foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_SetZ" rawSetZ :: Ptr Dir -> CDouble -> IO ()

setZ :: Ptr Dir -> Double -> IO ()
setZ = coerce rawSetZ

-- tests

-- isEqual

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_IsEqual" rawIsEqual :: Ptr Dir -> Ptr Dir -> CDouble -> IO CBool

isEqual :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isEqual a b tolerance = (/= 0) <$> rawIsEqual a b (CDouble tolerance)

-- isNormal

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_IsNormal" rawIsNormal :: Ptr Dir -> Ptr Dir -> CDouble -> IO CBool

isNormal :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isNormal a b tolerance = (/= 0) <$> rawIsNormal a b (CDouble tolerance)


-- isOpposite

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_IsOpposite" rawIsOpposite :: Ptr Dir -> Ptr Dir -> CDouble -> IO CBool

isOpposite :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isOpposite a b tolerance = (/= 0) <$> rawIsOpposite a b (CDouble tolerance)

-- isParallel

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_IsParallel" rawIsParallel :: Ptr Dir -> Ptr Dir -> CDouble -> IO CBool

isParallel :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isParallel a b tolerance = (/= 0) <$> rawIsParallel a b (CDouble tolerance)

-- angle

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Angle" rawAngle :: Ptr Dir -> Ptr Dir -> IO CDouble

angle :: Ptr Dir -> Ptr Dir -> IO Double
angle = coerce rawAngle

-- angleWithRef

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_AngleWithRef" rawAngleWithRef :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO CDouble

angleWithRef :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO Double
angleWithRef = coerce rawAngleWithRef

-- cross/crossed

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Cross" cross :: Ptr Dir -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Crossed" rawCrossed :: Ptr Dir -> Ptr Dir -> IO (Ptr Dir)

crossed :: Ptr Dir -> Ptr Dir -> Acquire (Ptr Dir)
crossed a b = mkAcquire (rawCrossed a b) deleteDir


-- crossCross/crossCrossed

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_CrossCross" crossCross :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_CrossCrossed" rawCrossCrossed :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO (Ptr Dir)

crossCrossed :: Ptr Dir -> Ptr Dir -> Ptr Dir -> Acquire (Ptr Dir)
crossCrossed a b c = mkAcquire (rawCrossCrossed a b c) deleteDir


-- dot

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Dot" rawDot :: Ptr Dir -> Ptr Dir -> IO CDouble

dot :: Ptr Dir -> Ptr Dir -> IO Double
dot = coerce rawDot


-- dotCross

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_DotCross" rawDotCross :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO CDouble

dotCross :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO Double
dotCross = coerce rawDotCross


-- reverse/reversed

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Reverse" reverse :: Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Reversed" rawReversed :: Ptr Dir -> IO (Ptr Dir)

reversed :: Ptr Dir -> Acquire (Ptr Dir)
reversed axis = mkAcquire (rawReversed axis) deleteDir

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Mirror" mirror :: Ptr Dir -> Ptr Dir -> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Mirrored" rawMirrored :: Ptr Dir -> Ptr Dir -> IO (Ptr Dir)

mirrored :: Ptr Dir -> Ptr Dir -> Acquire (Ptr Dir)
mirrored point axis = mkAcquire (rawMirrored point axis) deleteDir

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_MirrorAboutAx1" mirrorAboutAx1 :: Ptr Dir -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_MirroredAboutAx1" rawMirroredAboutAx1 :: Ptr Dir -> Ptr Ax1 -> IO (Ptr Dir)

mirroredAboutAx1 :: Ptr Dir -> Ptr Ax1 -> Acquire (Ptr Dir)
mirroredAboutAx1 point axis = mkAcquire (rawMirroredAboutAx1 point axis) deleteDir

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_MirrorAboutAx2" mirrorAboutAx2 :: Ptr Dir -> Ptr Ax2 -> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_MirroredAboutAx2" rawMirroredAboutAx2 :: Ptr Dir -> Ptr Ax2 -> IO (Ptr Dir)

mirroredAboutAx2 :: Ptr Dir -> Ptr Ax2 -> Acquire (Ptr Dir)
mirroredAboutAx2 point axis = mkAcquire (rawMirroredAboutAx2 point axis) deleteDir

-- rotate/rotated

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Rotate" rotate :: Ptr Dir -> Ptr Ax1 -> CDouble-> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Rotated" rawRotated :: Ptr Dir -> Ptr Ax1 -> CDouble -> IO (Ptr Dir)

rotated :: Ptr Dir -> Ptr Ax1 -> Double -> Acquire (Ptr Dir)
rotated point axis amount = mkAcquire (rawRotated point axis (CDouble amount)) deleteDir

-- transform/transformed

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Transform" transform :: Ptr Dir -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Dir.h hs_gp_Dir_Transformed" rawTransformed :: Ptr Dir -> Ptr Trsf -> IO (Ptr Dir)

transformed :: Ptr Dir -> Ptr Trsf -> Acquire (Ptr Dir)
transformed point trsf = mkAcquire (rawTransformed point trsf) deleteDir
