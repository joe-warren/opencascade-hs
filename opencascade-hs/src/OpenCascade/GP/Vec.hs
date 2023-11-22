{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Vec
( Vec
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
, magnitude
, squareMagnitude
, add
, added
, subtract
, subtracted
, multiply
, multiplied
, divide
, divided
, cross
, crossed
, crossCross
, crossCrossed
, crossMagnitude
, crossSquareMagnitude
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
, scale
, scaled
, transform
, transformed
) where


import Prelude hiding (reverse, subtract)
import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 

-- new

foreign import capi unsafe "hs_gp_Vec.h hs_new_gp_Vec" rawNew :: CDouble -> CDouble -> CDouble -> IO (Ptr Vec)

new :: Double -> Double -> Double -> Acquire (Ptr Vec)
new x y z = mkAcquire (rawNew (CDouble x) (CDouble y) (CDouble z)) deleteVec

-- getters

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_X" rawX :: Ptr Vec -> IO (CDouble)

getX :: Ptr Vec -> IO Double
getX = coerce rawX

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Y" rawY :: Ptr Vec -> IO (CDouble)

getY :: Ptr Vec -> IO Double
getY = coerce rawY

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Z" rawZ :: Ptr Vec -> IO (CDouble)

getZ :: Ptr Vec -> IO Double
getZ = coerce rawZ

-- setters

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_SetX" rawSetX :: Ptr Vec -> CDouble -> IO ()

setX :: Ptr Vec -> Double -> IO ()
setX = coerce rawSetX


foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_SetY" rawSetY :: Ptr Vec -> CDouble -> IO ()

setY :: Ptr Vec -> Double -> IO ()
setY = coerce rawSetY


foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_SetZ" rawSetZ :: Ptr Vec -> CDouble -> IO ()

setZ :: Ptr Vec -> Double -> IO ()
setZ = coerce rawSetZ

-- tests

-- isEqual

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_IsEqual" rawIsEqual :: Ptr Vec -> Ptr Vec -> CDouble -> CDouble -> IO CBool

isEqual :: Ptr Vec -> Ptr Vec -> Double -> Double -> IO Bool
isEqual a b linearTolerance angularTolerance = (/= 0) <$> rawIsEqual a b (CDouble linearTolerance) (CDouble angularTolerance)

-- isNormal

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_IsNormal" rawIsNormal :: Ptr Vec -> Ptr Vec -> CDouble -> IO CBool

isNormal :: Ptr Vec -> Ptr Vec -> Double -> IO Bool
isNormal a b tolerance = (/= 0) <$> rawIsNormal a b (CDouble tolerance)

-- isOpposite

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_IsOpposite" rawIsOpposite :: Ptr Vec -> Ptr Vec -> CDouble -> IO CBool

isOpposite :: Ptr Vec -> Ptr Vec -> Double -> IO Bool
isOpposite a b tolerance = (/= 0) <$> rawIsOpposite a b (CDouble tolerance)

-- isParallel

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_IsParallel" rawIsParallel :: Ptr Vec -> Ptr Vec -> CDouble -> IO CBool

isParallel :: Ptr Vec -> Ptr Vec -> Double -> IO Bool
isParallel a b tolerance = (/= 0) <$> rawIsParallel a b (CDouble tolerance)

-- angle

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Angle" rawAngle :: Ptr Vec -> Ptr Vec -> IO CDouble

angle :: Ptr Vec -> Ptr Vec -> IO Double
angle = coerce rawAngle

-- angleWithRef

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_AngleWithRef" rawAngleWithRef :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO CDouble

angleWithRef :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO Double
angleWithRef = coerce rawAngleWithRef


-- magnitude

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Magnitude" rawMagnitude :: Ptr Vec -> IO CDouble

magnitude :: Ptr Vec -> IO Double
magnitude = coerce rawMagnitude


-- squareMagnitude

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_SquareMagnitude" rawSquareMagnitude :: Ptr Vec -> IO CDouble

squareMagnitude :: Ptr Vec -> IO Double
squareMagnitude = coerce rawSquareMagnitude

-- add/added

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Add" add :: Ptr Vec -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Added" rawAdded :: Ptr Vec -> Ptr Vec -> IO (Ptr Vec)

added :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
added a b = mkAcquire (rawAdded a b) deleteVec


-- subtract/subtracted

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Subtract" subtract :: Ptr Vec -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Subtracted" rawSubtracted :: Ptr Vec -> Ptr Vec -> IO (Ptr Vec)

subtracted :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
subtracted a b = mkAcquire (rawSubtracted a b) deleteVec


-- multiply/multiplied

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Multiply" rawMultiply :: Ptr Vec -> CDouble -> IO ()

multiply :: Ptr Vec -> Double -> IO ()
multiply = coerce rawMultiply

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Multiplied" rawMultiplied :: Ptr Vec -> CDouble -> IO (Ptr Vec)

multiplied :: Ptr Vec -> Double -> Acquire (Ptr Vec)
multiplied a b = mkAcquire (rawMultiplied a (CDouble b)) deleteVec

-- divide/divided

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Divide" rawDivide :: Ptr Vec -> CDouble -> IO ()

divide :: Ptr Vec -> Double -> IO ()
divide = coerce rawDivide

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Divided" rawDivided :: Ptr Vec -> CDouble -> IO (Ptr Vec)

divided :: Ptr Vec -> Double -> Acquire (Ptr Vec)
divided a b = mkAcquire (rawDivided a (CDouble b)) deleteVec

-- cross/crossed

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Cross" cross :: Ptr Vec -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Crossed" rawCrossed :: Ptr Vec -> Ptr Vec -> IO (Ptr Vec)

crossed :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
crossed a b = mkAcquire (rawCrossed a b) deleteVec

-- crossCross/crossCrossed

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_CrossCross" crossCross :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_CrossCrossed" rawCrossCrossed :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO (Ptr Vec)

crossCrossed :: Ptr Vec -> Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
crossCrossed a b c = mkAcquire (rawCrossCrossed a b c) deleteVec

-- crossMagnitude

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_CrossMagnitude" rawCrossMagnitude :: Ptr Vec -> Ptr Vec -> IO CDouble

crossMagnitude :: Ptr Vec -> Ptr Vec -> IO Double
crossMagnitude = coerce rawCrossMagnitude

-- crossSquareMagnitude

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_CrossSquareMagnitude" rawCrossSquareMagnitude :: Ptr Vec -> Ptr Vec -> IO CDouble

crossSquareMagnitude :: Ptr Vec -> Ptr Vec -> IO Double
crossSquareMagnitude = coerce rawCrossSquareMagnitude

-- dot

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Dot" rawDot :: Ptr Vec -> Ptr Vec -> IO CDouble

dot :: Ptr Vec -> Ptr Vec -> IO Double
dot = coerce rawDot


-- dotCross

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_DotCross" rawDotCross :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO CDouble

dotCross :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO Double
dotCross = coerce rawDotCross


-- reverse/reversed

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Reverse" reverse :: Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Reversed" rawReversed :: Ptr Vec -> IO (Ptr Vec)

reversed :: Ptr Vec -> Acquire (Ptr Vec)
reversed axis = mkAcquire (rawReversed axis) deleteVec

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Mirror" mirror :: Ptr Vec -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Mirrored" rawMirrored :: Ptr Vec -> Ptr Vec -> IO (Ptr Vec)

mirrored :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
mirrored point axis = mkAcquire (rawMirrored point axis) deleteVec

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_MirrorAboutAx1" mirrorAboutAx1 :: Ptr Vec -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_MirroredAboutAx1" rawMirroredAboutAx1 :: Ptr Vec -> Ptr Ax1 -> IO (Ptr Vec)

mirroredAboutAx1 :: Ptr Vec -> Ptr Ax1 -> Acquire (Ptr Vec)
mirroredAboutAx1 point axis = mkAcquire (rawMirroredAboutAx1 point axis) deleteVec

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_MirrorAboutAx2" mirrorAboutAx2 :: Ptr Vec -> Ptr Ax2 -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_MirroredAboutAx2" rawMirroredAboutAx2 :: Ptr Vec -> Ptr Ax2 -> IO (Ptr Vec)

mirroredAboutAx2 :: Ptr Vec -> Ptr Ax2 -> Acquire (Ptr Vec)
mirroredAboutAx2 point axis = mkAcquire (rawMirroredAboutAx2 point axis) deleteVec

-- rotate/rotated

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Rotate" rotate :: Ptr Vec -> Ptr Ax1 -> CDouble-> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Rotated" rawRotated :: Ptr Vec -> Ptr Ax1 -> CDouble -> IO (Ptr Vec)

rotated :: Ptr Vec -> Ptr Ax1 -> Double -> Acquire (Ptr Vec)
rotated point axis amount = mkAcquire (rawRotated point axis (CDouble amount)) deleteVec

-- scale/scaled

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Scale" rawScale :: Ptr Vec -> CDouble -> IO ()

scale :: Ptr Vec -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Scaled" rawScaled :: Ptr Vec -> CDouble -> IO (Ptr Vec)

scaled :: Ptr Vec -> Double -> Acquire (Ptr Vec)
scaled a b = mkAcquire (rawScaled a (CDouble b)) deleteVec


-- transform/transformed

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Transform" transform :: Ptr Vec -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Vec.h hs_gp_Vec_Transformed" rawTransformed :: Ptr Vec -> Ptr Trsf -> IO (Ptr Vec)

transformed :: Ptr Vec -> Ptr Trsf -> Acquire (Ptr Vec)
transformed point trsf = mkAcquire (rawTransformed point trsf) deleteVec
