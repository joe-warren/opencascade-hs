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
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext)

C.include "<gp_Vec.hxx>"
C.include "<gp_Dir.hxx>"
C.include "<gp_Ax1.hxx>"
C.include "<gp_Ax2.hxx>"
C.include "<gp_Trsf.hxx>" 

-- new

new :: Double -> Double -> Double -> Acquire (Ptr Vec)
new x y z = 
  let cx = realToFrac x
      cy = realToFrac y
      cz = realToFrac z
      createVec = [C.throwBlock| gp_Vec* {
        return new gp_Vec($(double cx), $(double cy), $(double cz));
      } |]
  in mkAcquire createVec deleteVec

-- getters

getX :: Ptr Vec -> IO Double
getX vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* vec)->X();
  } |]
  return (realToFrac result)

getY :: Ptr Vec -> IO Double
getY vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* vec)->Y();
  } |]
  return (realToFrac result)

getZ :: Ptr Vec -> IO Double
getZ vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* vec)->Z();
  } |]
  return (realToFrac result)

-- setters

setX :: Ptr Vec -> Double -> IO ()
setX vec x = 
  let cx = realToFrac x
  in [C.throwBlock| void {
    $(gp_Vec* vec)->SetX($(double cx));
  } |]

setY :: Ptr Vec -> Double -> IO ()
setY vec y = 
  let cy = realToFrac y
  in [C.throwBlock| void {
    $(gp_Vec* vec)->SetY($(double cy));
  } |]

setZ :: Ptr Vec -> Double -> IO ()
setZ vec z = 
  let cz = realToFrac z
  in [C.throwBlock| void {
    $(gp_Vec* vec)->SetZ($(double cz));
  } |]

-- tests

-- isEqual

isEqual :: Ptr Vec -> Ptr Vec -> Double -> Double -> IO Bool
isEqual a b linearTolerance angularTolerance = do
  let cLinearTolerance = realToFrac linearTolerance
      cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec* a)->IsEqual(*$(gp_Vec* b), $(double cLinearTolerance), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- isNormal

isNormal :: Ptr Vec -> Ptr Vec -> Double -> IO Bool
isNormal a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec* a)->IsNormal(*$(gp_Vec* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isOpposite

isOpposite :: Ptr Vec -> Ptr Vec -> Double -> IO Bool
isOpposite a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec* a)->IsOpposite(*$(gp_Vec* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isParallel

isParallel :: Ptr Vec -> Ptr Vec -> Double -> IO Bool
isParallel a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec* a)->IsParallel(*$(gp_Vec* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- angle

angle :: Ptr Vec -> Ptr Vec -> IO Double
angle a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* a)->Angle(*$(gp_Vec* b));
  } |]
  return (realToFrac result)

-- angleWithRef

angleWithRef :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO Double
angleWithRef a b ref = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* a)->AngleWithRef(*$(gp_Vec* b), *$(gp_Vec* ref));
  } |]
  return (realToFrac result)


-- magnitude

magnitude :: Ptr Vec -> IO Double
magnitude vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* vec)->Magnitude();
  } |]
  return (realToFrac result)


-- squareMagnitude

squareMagnitude :: Ptr Vec -> IO Double
squareMagnitude vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* vec)->SquareMagnitude();
  } |]
  return (realToFrac result)

-- add/added

add :: Ptr Vec -> Ptr Vec -> IO ()
add theVec other = [C.throwBlock| void {
  $(gp_Vec* theVec)->Add(*$(gp_Vec* other));
} |]

added :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
added a b = mkAcquire createAdded deleteVec
  where
    createAdded = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* a)->Added(*$(gp_Vec* b)));
    } |]


-- subtract/subtracted

subtract :: Ptr Vec -> Ptr Vec -> IO ()
subtract theVec other = [C.throwBlock| void {
  $(gp_Vec* theVec)->Subtract(*$(gp_Vec* other));
} |]

subtracted :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
subtracted a b = mkAcquire createSubtracted deleteVec
  where
    createSubtracted = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* a)->Subtracted(*$(gp_Vec* b)));
    } |]


-- multiply/multiplied

multiply :: Ptr Vec -> Double -> IO ()
multiply theVec scalar = 
  let cScalar = realToFrac scalar
  in [C.throwBlock| void {
    $(gp_Vec* theVec)->Multiply($(double cScalar));
  } |]

multiplied :: Ptr Vec -> Double -> Acquire (Ptr Vec)
multiplied vec scalar = 
  let cScalar = realToFrac scalar
      createMultiplied = [C.throwBlock| gp_Vec* {
        return new gp_Vec($(gp_Vec* vec)->Multiplied($(double cScalar)));
      } |]
  in mkAcquire createMultiplied deleteVec

-- divide/divided

divide :: Ptr Vec -> Double -> IO ()
divide theVec scalar = 
  let cScalar = realToFrac scalar
  in [C.throwBlock| void {
    $(gp_Vec* theVec)->Divide($(double cScalar));
  } |]

divided :: Ptr Vec -> Double -> Acquire (Ptr Vec)
divided vec scalar = 
  let cScalar = realToFrac scalar
      createDivided = [C.throwBlock| gp_Vec* {
        return new gp_Vec($(gp_Vec* vec)->Divided($(double cScalar)));
      } |]
  in mkAcquire createDivided deleteVec

-- cross/crossed

cross :: Ptr Vec -> Ptr Vec -> IO ()
cross theVec other = [C.throwBlock| void {
  $(gp_Vec* theVec)->Cross(*$(gp_Vec* other));
} |]

crossed :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
crossed a b = mkAcquire createCrossed deleteVec
  where
    createCrossed = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* a)->Crossed(*$(gp_Vec* b)));
    } |]

-- crossCross/crossCrossed

crossCross :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO ()
crossCross theVec v1 v2 = [C.throwBlock| void {
  $(gp_Vec* theVec)->CrossCross(*$(gp_Vec* v1), *$(gp_Vec* v2));
} |]

crossCrossed :: Ptr Vec -> Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
crossCrossed a b c = mkAcquire createCrossCrossed deleteVec
  where
    createCrossCrossed = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* a)->CrossCrossed(*$(gp_Vec* b), *$(gp_Vec* c)));
    } |]

-- crossMagnitude

crossMagnitude :: Ptr Vec -> Ptr Vec -> IO Double
crossMagnitude a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* a)->CrossMagnitude(*$(gp_Vec* b));
  } |]
  return (realToFrac result)

-- crossSquareMagnitude

crossSquareMagnitude :: Ptr Vec -> Ptr Vec -> IO Double
crossSquareMagnitude a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* a)->CrossSquareMagnitude(*$(gp_Vec* b));
  } |]
  return (realToFrac result)

-- dot

dot :: Ptr Vec -> Ptr Vec -> IO Double
dot a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* a)->Dot(*$(gp_Vec* b));
  } |]
  return (realToFrac result)


-- dotCross

dotCross :: Ptr Vec -> Ptr Vec -> Ptr Vec -> IO Double
dotCross a b c = do
  result <- [C.throwBlock| double {
    return $(gp_Vec* a)->DotCross(*$(gp_Vec* b), *$(gp_Vec* c));
  } |]
  return (realToFrac result)


-- reverse/reversed

reverse :: Ptr Vec -> IO ()
reverse vec = [C.throwBlock| void {
  $(gp_Vec* vec)->Reverse();
} |]

reversed :: Ptr Vec -> Acquire (Ptr Vec)
reversed axis = mkAcquire createReversed deleteVec
  where
    createReversed = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* axis)->Reversed());
    } |]

-- mirror/mirrored

mirror :: Ptr Vec -> Ptr Vec -> IO ()
mirror theVec mirrorVec = [C.throwBlock| void {
  $(gp_Vec* theVec)->Mirror(*$(gp_Vec* mirrorVec));
} |]

mirrored :: Ptr Vec -> Ptr Vec -> Acquire (Ptr Vec)
mirrored point axis = mkAcquire createMirrored deleteVec
  where
    createMirrored = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* point)->Mirrored(*$(gp_Vec* axis)));
    } |]

mirrorAboutAx1 :: Ptr Vec -> Ptr Ax1 -> IO ()
mirrorAboutAx1 theVec mirrorAxis = [C.throwBlock| void {
  $(gp_Vec* theVec)->Mirror(*$(gp_Ax1* mirrorAxis));
} |]

mirroredAboutAx1 :: Ptr Vec -> Ptr Ax1 -> Acquire (Ptr Vec)
mirroredAboutAx1 point axis = mkAcquire createMirrored deleteVec
  where
    createMirrored = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* point)->Mirrored(*$(gp_Ax1* axis)));
    } |]

mirrorAboutAx2 :: Ptr Vec -> Ptr Ax2 -> IO ()
mirrorAboutAx2 theVec mirrorAxis = [C.throwBlock| void {
  $(gp_Vec* theVec)->Mirror(*$(gp_Ax2* mirrorAxis));
} |]

mirroredAboutAx2 :: Ptr Vec -> Ptr Ax2 -> Acquire (Ptr Vec)
mirroredAboutAx2 point axis = mkAcquire createMirrored deleteVec
  where
    createMirrored = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* point)->Mirrored(*$(gp_Ax2* axis)));
    } |]

-- rotate/rotated

rotate :: Ptr Vec -> Ptr Ax1 -> Double -> IO ()
rotate theVec axis amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Vec* theVec)->Rotate(*$(gp_Ax1* axis), $(double cAmount));
  } |]

rotated :: Ptr Vec -> Ptr Ax1 -> Double -> Acquire (Ptr Vec)
rotated point axis amount = 
  let cAmount = realToFrac amount
      createRotated = [C.throwBlock| gp_Vec* {
        return new gp_Vec($(gp_Vec* point)->Rotated(*$(gp_Ax1* axis), $(double cAmount)));
      } |]
  in mkAcquire createRotated deleteVec

-- scale/scaled

scale :: Ptr Vec -> Double -> IO ()
scale theVec factor = 
  let cFactor = realToFrac factor
  in [C.throwBlock| void {
    $(gp_Vec* theVec)->Scale($(double cFactor));
  } |]

scaled :: Ptr Vec -> Double -> Acquire (Ptr Vec)
scaled vec factor = 
  let cFactor = realToFrac factor
      createScaled = [C.throwBlock| gp_Vec* {
        return new gp_Vec($(gp_Vec* vec)->Scaled($(double cFactor)));
      } |]
  in mkAcquire createScaled deleteVec


-- transform/transformed

transform :: Ptr Vec -> Ptr Trsf -> IO ()
transform theVec trsf = [C.throwBlock| void {
  $(gp_Vec* theVec)->Transform(*$(gp_Trsf* trsf));
} |]

transformed :: Ptr Vec -> Ptr Trsf -> Acquire (Ptr Vec)
transformed point trsf = mkAcquire createTransformed deleteVec
  where
    createTransformed = [C.throwBlock| gp_Vec* {
      return new gp_Vec($(gp_Vec* point)->Transformed(*$(gp_Trsf* trsf)));
    } |]
