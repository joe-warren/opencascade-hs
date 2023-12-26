{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Vec2d
( Vec2d
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
, crossed
, crossMagnitude
, crossSquareMagnitude
, dot
, reverse
, reversed
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
) where


import Prelude hiding (reverse, subtract)
import OpenCascade.GP.Types
import OpenCascade.GP.Internal.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 

-- new

foreign import capi unsafe "hs_gp_Vec2d.h hs_new_gp_Vec2d" rawNew :: CDouble -> CDouble -> IO (Ptr Vec2d)

new :: Double -> Double -> Acquire (Ptr Vec2d)
new x y = mkAcquire (rawNew (CDouble x) (CDouble y)) deleteVec2d

-- getters

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_X" rawX :: Ptr Vec2d -> IO (CDouble)

getX :: Ptr Vec2d -> IO Double
getX = coerce rawX

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Y" rawY :: Ptr Vec2d -> IO (CDouble)

getY :: Ptr Vec2d -> IO Double
getY = coerce rawY

-- setters

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_SetX" rawSetX :: Ptr Vec2d -> CDouble -> IO ()

setX :: Ptr Vec2d -> Double -> IO ()
setX = coerce rawSetX


foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_SetY" rawSetY :: Ptr Vec2d -> CDouble -> IO ()

setY :: Ptr Vec2d -> Double -> IO ()
setY = coerce rawSetY

-- tests

-- isEqual

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_IsEqual" rawIsEqual :: Ptr Vec2d -> Ptr Vec2d -> CDouble -> CDouble -> IO CBool

isEqual :: Ptr Vec2d -> Ptr Vec2d -> Double -> Double -> IO Bool
isEqual a b linearTolerance angularTolerance = (/= 0) <$> rawIsEqual a b (CDouble linearTolerance) (CDouble angularTolerance)

-- isNormal

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_IsNormal" rawIsNormal :: Ptr Vec2d -> Ptr Vec2d -> CDouble -> IO CBool

isNormal :: Ptr Vec2d -> Ptr Vec2d -> Double -> IO Bool
isNormal a b tolerance = (/= 0) <$> rawIsNormal a b (CDouble tolerance)

-- isOpposite

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_IsOpposite" rawIsOpposite :: Ptr Vec2d -> Ptr Vec2d -> CDouble -> IO CBool

isOpposite :: Ptr Vec2d -> Ptr Vec2d -> Double -> IO Bool
isOpposite a b tolerance = (/= 0) <$> rawIsOpposite a b (CDouble tolerance)

-- isParallel

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_IsParallel" rawIsParallel :: Ptr Vec2d -> Ptr Vec2d -> CDouble -> IO CBool

isParallel :: Ptr Vec2d -> Ptr Vec2d -> Double -> IO Bool
isParallel a b tolerance = (/= 0) <$> rawIsParallel a b (CDouble tolerance)

-- angle

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Angle" rawAngle :: Ptr Vec2d -> Ptr Vec2d -> IO CDouble

angle :: Ptr Vec2d -> Ptr Vec2d -> IO Double
angle = coerce rawAngle


-- magnitude

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Magnitude" rawMagnitude :: Ptr Vec2d -> IO CDouble

magnitude :: Ptr Vec2d -> IO Double
magnitude = coerce rawMagnitude


-- squareMagnitude

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_SquareMagnitude" rawSquareMagnitude :: Ptr Vec2d -> IO CDouble

squareMagnitude :: Ptr Vec2d -> IO Double
squareMagnitude = coerce rawSquareMagnitude

-- add/added

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Add" add :: Ptr Vec2d -> Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Added" rawAdded :: Ptr Vec2d -> Ptr Vec2d -> IO (Ptr Vec2d)

added :: Ptr Vec2d -> Ptr Vec2d -> Acquire (Ptr Vec2d)
added a b = mkAcquire (rawAdded a b) deleteVec2d


-- subtract/subtracted

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Subtract" subtract :: Ptr Vec2d -> Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Subtracted" rawSubtracted :: Ptr Vec2d -> Ptr Vec2d -> IO (Ptr Vec2d)

subtracted :: Ptr Vec2d -> Ptr Vec2d -> Acquire (Ptr Vec2d)
subtracted a b = mkAcquire (rawSubtracted a b) deleteVec2d


-- multiply/multiplied

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Multiply" rawMultiply :: Ptr Vec2d -> CDouble -> IO ()

multiply :: Ptr Vec2d -> Double -> IO ()
multiply = coerce rawMultiply

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Multiplied" rawMultiplied :: Ptr Vec2d -> CDouble -> IO (Ptr Vec2d)

multiplied :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
multiplied a b = mkAcquire (rawMultiplied a (CDouble b)) deleteVec2d

-- divide/divided

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Divide" rawDivide :: Ptr Vec2d -> CDouble -> IO ()

divide :: Ptr Vec2d -> Double -> IO ()
divide = coerce rawDivide

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Divided" rawDivided :: Ptr Vec2d -> CDouble -> IO (Ptr Vec2d)

divided :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
divided a b = mkAcquire (rawDivided a (CDouble b)) deleteVec2d

-- cross/crossed

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Crossed" rawCrossed :: Ptr Vec2d -> Ptr Vec2d -> IO (CDouble)

crossed :: Ptr Vec2d -> Ptr Vec2d -> IO CDouble
crossed = coerce rawCrossed

-- crossMagnitude

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_CrossMagnitude" rawCrossMagnitude :: Ptr Vec2d -> Ptr Vec2d -> IO CDouble

crossMagnitude :: Ptr Vec2d -> Ptr Vec2d -> IO Double
crossMagnitude = coerce rawCrossMagnitude

-- crossSquareMagnitude

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_CrossSquareMagnitude" rawCrossSquareMagnitude :: Ptr Vec2d -> Ptr Vec2d -> IO CDouble

crossSquareMagnitude :: Ptr Vec2d -> Ptr Vec2d -> IO Double
crossSquareMagnitude = coerce rawCrossSquareMagnitude

-- dot

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Dot" rawDot :: Ptr Vec2d -> Ptr Vec2d -> IO CDouble

dot :: Ptr Vec2d -> Ptr Vec2d -> IO Double
dot = coerce rawDot

-- reverse/reversed

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Reverse" reverse :: Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Reversed" rawReversed :: Ptr Vec2d -> IO (Ptr Vec2d)

reversed :: Ptr Vec2d -> Acquire (Ptr Vec2d)
reversed axis = mkAcquire (rawReversed axis) deleteVec2d

-- mirror/mirrored

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Mirror" mirror :: Ptr Vec2d -> Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Mirrored" rawMirrored :: Ptr Vec2d -> Ptr Vec2d -> IO (Ptr Vec2d)

mirrored :: Ptr Vec2d -> Ptr Vec2d -> Acquire (Ptr Vec2d)
mirrored point axis = mkAcquire (rawMirrored point axis) deleteVec2d

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_MirrorAboutAx2d" mirrorAboutAx2d :: Ptr Vec2d -> Ptr Ax2d -> IO ()

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_MirroredAboutAx2d" rawMirroredAboutAx2d :: Ptr Vec2d -> Ptr Ax2d -> IO (Ptr Vec2d)

mirroredAboutAx2d :: Ptr Vec2d -> Ptr Ax2d -> Acquire (Ptr Vec2d)
mirroredAboutAx2d point axis = mkAcquire (rawMirroredAboutAx2d point axis) deleteVec2d

-- rotate/rotated

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Rotate" rotate :: Ptr Vec2d -> CDouble-> IO ()

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Rotated" rawRotated :: Ptr Vec2d -> CDouble -> IO (Ptr Vec2d)

rotated :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
rotated point angleOfRotation = mkAcquire (rawRotated point (CDouble angleOfRotation)) deleteVec2d

-- scale/scaled

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Scale" rawScale :: Ptr Vec2d -> CDouble -> IO ()

scale :: Ptr Vec2d -> Double -> IO ()
scale = coerce rawScale

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Scaled" rawScaled :: Ptr Vec2d -> CDouble -> IO (Ptr Vec2d)

scaled :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
scaled a b = mkAcquire (rawScaled a (CDouble b)) deleteVec2d


-- transform/transformed

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Transform" transform :: Ptr Vec2d -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Vec2d.h hs_gp_Vec2d_Transformed" rawTransformed :: Ptr Vec2d -> Ptr Trsf -> IO (Ptr Vec2d)

transformed :: Ptr Vec2d -> Ptr Trsf -> Acquire (Ptr Vec2d)
transformed point trsf = mkAcquire (rawTransformed point trsf) deleteVec2d
