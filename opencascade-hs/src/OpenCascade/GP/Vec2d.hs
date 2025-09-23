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
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext)

C.include "<gp_Vec2d.hxx>"
C.include "<gp_Pnt2d.hxx>"
C.include "<gp_Ax2d.hxx>"
C.include "<gp_Trsf2d.hxx>" 

-- new

new :: Double -> Double -> Acquire (Ptr Vec2d)
new x y = 
  let cx = realToFrac x
      cy = realToFrac y
      createVec2d = [C.throwBlock| gp_Vec2d* {
        return new gp_Vec2d($(double cx), $(double cy));
      } |]
  in mkAcquire createVec2d deleteVec2d

-- getters

getX :: Ptr Vec2d -> IO Double
getX vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* vec)->X();
  } |]
  return (realToFrac result)

getY :: Ptr Vec2d -> IO Double
getY vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* vec)->Y();
  } |]
  return (realToFrac result)

-- setters

setX :: Ptr Vec2d -> Double -> IO ()
setX vec x = 
  let cx = realToFrac x
  in [C.throwBlock| void {
    $(gp_Vec2d* vec)->SetX($(double cx));
  } |]


setY :: Ptr Vec2d -> Double -> IO ()
setY vec y = 
  let cy = realToFrac y
  in [C.throwBlock| void {
    $(gp_Vec2d* vec)->SetY($(double cy));
  } |]

-- tests

-- isEqual

isEqual :: Ptr Vec2d -> Ptr Vec2d -> Double -> Double -> IO Bool
isEqual a b linearTolerance angularTolerance = do
  let cLinearTolerance = realToFrac linearTolerance
      cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec2d* a)->IsEqual(*$(gp_Vec2d* b), $(double cLinearTolerance), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- isNormal

isNormal :: Ptr Vec2d -> Ptr Vec2d -> Double -> IO Bool
isNormal a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec2d* a)->IsNormal(*$(gp_Vec2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isOpposite

isOpposite :: Ptr Vec2d -> Ptr Vec2d -> Double -> IO Bool
isOpposite a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec2d* a)->IsOpposite(*$(gp_Vec2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isParallel

isParallel :: Ptr Vec2d -> Ptr Vec2d -> Double -> IO Bool
isParallel a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Vec2d* a)->IsParallel(*$(gp_Vec2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- angle

angle :: Ptr Vec2d -> Ptr Vec2d -> IO Double
angle a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* a)->Angle(*$(gp_Vec2d* b));
  } |]
  return (realToFrac result)


-- magnitude

magnitude :: Ptr Vec2d -> IO Double
magnitude vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* vec)->Magnitude();
  } |]
  return (realToFrac result)


-- squareMagnitude

squareMagnitude :: Ptr Vec2d -> IO Double
squareMagnitude vec = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* vec)->SquareMagnitude();
  } |]
  return (realToFrac result)

-- add/added

add :: Ptr Vec2d -> Ptr Vec2d -> IO ()
add theVec other = [C.throwBlock| void {
  $(gp_Vec2d* theVec)->Add(*$(gp_Vec2d* other));
} |]

added :: Ptr Vec2d -> Ptr Vec2d -> Acquire (Ptr Vec2d)
added a b = mkAcquire createAdded deleteVec2d
  where
    createAdded = [C.throwBlock| gp_Vec2d* {
      return new gp_Vec2d($(gp_Vec2d* a)->Added(*$(gp_Vec2d* b)));
    } |]


-- subtract/subtracted

subtract :: Ptr Vec2d -> Ptr Vec2d -> IO ()
subtract theVec other = [C.throwBlock| void {
  $(gp_Vec2d* theVec)->Subtract(*$(gp_Vec2d* other));
} |]

subtracted :: Ptr Vec2d -> Ptr Vec2d -> Acquire (Ptr Vec2d)
subtracted a b = mkAcquire createSubtracted deleteVec2d
  where
    createSubtracted = [C.throwBlock| gp_Vec2d* {
      return new gp_Vec2d($(gp_Vec2d* a)->Subtracted(*$(gp_Vec2d* b)));
    } |]


-- multiply/multiplied

multiply :: Ptr Vec2d -> Double -> IO ()
multiply theVec scalar = 
  let cScalar = realToFrac scalar
  in [C.throwBlock| void {
    $(gp_Vec2d* theVec)->Multiply($(double cScalar));
  } |]

multiplied :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
multiplied vec scalar = mkAcquire createMultiplied deleteVec2d
  where
    createMultiplied = let
      cScalar = realToFrac scalar
      in [C.throwBlock| gp_Vec2d* {
        return new gp_Vec2d($(gp_Vec2d* vec)->Multiplied($(double cScalar)));
      } |]

-- divide/divided

divide :: Ptr Vec2d -> Double -> IO ()
divide theVec scalar = 
  let cScalar = realToFrac scalar
  in [C.throwBlock| void {
    $(gp_Vec2d* theVec)->Divide($(double cScalar));
  } |]

divided :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
divided vec scalar = mkAcquire createDivided deleteVec2d
  where
    createDivided = let
      cScalar = realToFrac scalar
      in [C.throwBlock| gp_Vec2d* {
        return new gp_Vec2d($(gp_Vec2d* vec)->Divided($(double cScalar)));
      } |]

-- cross/crossed

crossed :: Ptr Vec2d -> Ptr Vec2d -> IO Double
crossed a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* a)->Crossed(*$(gp_Vec2d* b));
  } |]
  return (realToFrac result)

-- crossMagnitude

crossMagnitude :: Ptr Vec2d -> Ptr Vec2d -> IO Double
crossMagnitude a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* a)->CrossMagnitude(*$(gp_Vec2d* b));
  } |]
  return (realToFrac result)

-- crossSquareMagnitude

crossSquareMagnitude :: Ptr Vec2d -> Ptr Vec2d -> IO Double
crossSquareMagnitude a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* a)->CrossSquareMagnitude(*$(gp_Vec2d* b));
  } |]
  return (realToFrac result)

-- dot

dot :: Ptr Vec2d -> Ptr Vec2d -> IO Double
dot a b = do
  result <- [C.throwBlock| double {
    return $(gp_Vec2d* a)->Dot(*$(gp_Vec2d* b));
  } |]
  return (realToFrac result)

-- reverse/reversed

reverse :: Ptr Vec2d -> IO ()
reverse vec = [C.throwBlock| void {
  $(gp_Vec2d* vec)->Reverse();
} |]

reversed :: Ptr Vec2d -> Acquire (Ptr Vec2d)
reversed axis = mkAcquire createReversed deleteVec2d
  where
    createReversed = [C.throwBlock| gp_Vec2d* {
      return new gp_Vec2d($(gp_Vec2d* axis)->Reversed());
    } |]

-- mirror/mirrored

mirror :: Ptr Vec2d -> Ptr Vec2d -> IO ()
mirror theVec mirrorVec = [C.throwBlock| void {
  $(gp_Vec2d* theVec)->Mirror(*$(gp_Vec2d* mirrorVec));
} |]

mirrored :: Ptr Vec2d -> Ptr Vec2d -> Acquire (Ptr Vec2d)
mirrored point axis = mkAcquire createMirrored deleteVec2d
  where
    createMirrored = [C.throwBlock| gp_Vec2d* {
      return new gp_Vec2d($(gp_Vec2d* point)->Mirrored(*$(gp_Vec2d* axis)));
    } |]

mirrorAboutAx2d :: Ptr Vec2d -> Ptr Ax2d -> IO ()
mirrorAboutAx2d theVec mirrorAxis = [C.throwBlock| void {
  $(gp_Vec2d* theVec)->Mirror(*$(gp_Ax2d* mirrorAxis));
} |]

mirroredAboutAx2d :: Ptr Vec2d -> Ptr Ax2d -> Acquire (Ptr Vec2d)
mirroredAboutAx2d point axis = mkAcquire createMirrored deleteVec2d
  where
    createMirrored = [C.throwBlock| gp_Vec2d* {
      return new gp_Vec2d($(gp_Vec2d* point)->Mirrored(*$(gp_Ax2d* axis)));
    } |]

-- rotate/rotated

rotate :: Ptr Vec2d -> Double -> IO ()
rotate theVec amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Vec2d* theVec)->Rotate($(double cAmount));
  } |]

rotated :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
rotated point angleOfRotation = mkAcquire createRotated deleteVec2d
  where
    createRotated = let
      cAngle = realToFrac angleOfRotation
      in [C.throwBlock| gp_Vec2d* {
        return new gp_Vec2d($(gp_Vec2d* point)->Rotated($(double cAngle)));
      } |]

-- scale/scaled

scale :: Ptr Vec2d -> Double -> IO ()
scale theVec factor = 
  let cFactor = realToFrac factor
  in [C.throwBlock| void {
    $(gp_Vec2d* theVec)->Scale($(double cFactor));
  } |]

scaled :: Ptr Vec2d -> Double -> Acquire (Ptr Vec2d)
scaled vec factor = mkAcquire createScaled deleteVec2d
  where
    createScaled = let
      cFactor = realToFrac factor
      in [C.throwBlock| gp_Vec2d* {
        return new gp_Vec2d($(gp_Vec2d* vec)->Scaled($(double cFactor)));
      } |]


-- transform/transformed

transform :: Ptr Vec2d -> Ptr Trsf2d -> IO ()
transform theVec trsf = [C.throwBlock| void {
  $(gp_Vec2d* theVec)->Transform(*$(gp_Trsf2d* trsf));
} |]

transformed :: Ptr Vec2d -> Ptr Trsf2d -> Acquire (Ptr Vec2d)
transformed point trsf = mkAcquire createTransformed deleteVec2d
  where
    createTransformed = [C.throwBlock| gp_Vec2d* {
      return new gp_Vec2d($(gp_Vec2d* point)->Transformed(*$(gp_Trsf2d* trsf)));
    } |]
