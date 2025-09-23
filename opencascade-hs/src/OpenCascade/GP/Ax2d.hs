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
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext)

C.include "<gp_Ax2d.hxx>"
C.include "<gp_Pnt2d.hxx>"
C.include "<gp_Dir2d.hxx>"
C.include "<gp_Vec2d.hxx>"
C.include "<gp_Trsf2d.hxx>"


-- new and delete

new :: Ptr Pnt2d -> Ptr Dir2d -> Acquire (Ptr Ax2d)
new origin dir = mkAcquire createAx2d deleteAx2d
  where
    createAx2d = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d(*$(gp_Pnt2d* origin), *$(gp_Dir2d* dir));
    } |]

-- getters

location :: Ptr Ax2d -> Acquire (Ptr Pnt2d)
location ax2d = mkAcquire createLocation deletePnt2d
  where
    createLocation = [C.throwBlock| gp_Pnt2d* {
      return new gp_Pnt2d($(gp_Ax2d* ax2d)->Location());
    } |]

direction :: Ptr Ax2d -> Acquire (Ptr Dir2d)
direction ax2d = mkAcquire createDirection deleteDir2d
  where
    createDirection = [C.throwBlock| gp_Dir2d* {
      return new gp_Dir2d($(gp_Ax2d* ax2d)->Direction());
    } |]

-- setters

setDirection :: Ptr Ax2d -> Ptr Dir2d -> IO ()
setDirection ax2d dir = [C.throwBlock| void {
  $(gp_Ax2d* ax2d)->SetDirection(*$(gp_Dir2d* dir));
} |]

setLocation :: Ptr Ax2d -> Ptr Pnt2d -> IO ()
setLocation ax2d pnt = [C.throwBlock| void {
  $(gp_Ax2d* ax2d)->SetLocation(*$(gp_Pnt2d* pnt));
} |]

-- tests

-- isCoaxial

isCoaxial :: Ptr Ax2d -> Ptr Ax2d -> Double -> Double -> IO Bool
isCoaxial a b angularTolerance linearTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
      cLinearTolerance = realToFrac linearTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax2d* a)->IsCoaxial(*$(gp_Ax2d* b), $(double cAngularTolerance), $(double cLinearTolerance));
  } |]
  return (cBoolToBool result)

-- isNormal

isNormal :: Ptr Ax2d -> Ptr Ax2d -> Double -> IO Bool
isNormal a b angularTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax2d* a)->IsNormal(*$(gp_Ax2d* b), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- isOpposite

isOpposite :: Ptr Ax2d -> Ptr Ax2d -> Double -> IO Bool
isOpposite a b angularTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax2d* a)->IsOpposite(*$(gp_Ax2d* b), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- isParallel

isParallel :: Ptr Ax2d -> Ptr Ax2d -> Double -> IO Bool
isParallel a b angularTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax2d* a)->IsParallel(*$(gp_Ax2d* b), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- angle

angle :: Ptr Ax2d -> Ptr Ax2d -> IO Double
angle a b = do
  result <- [C.throwBlock| double {
    return $(gp_Ax2d* a)->Angle(*$(gp_Ax2d* b));
  } |]
  return (realToFrac result)

-- reverse/reversed

reverse :: Ptr Ax2d -> IO ()
reverse axis = [C.throwBlock| void {
  $(gp_Ax2d* axis)->Reverse();
} |]

reversed :: Ptr Ax2d -> Acquire (Ptr Ax2d)
reversed axis = mkAcquire createReversed deleteAx2d
  where
    createReversed = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d($(gp_Ax2d* axis)->Reversed());
    } |]

-- mirror/mirrored

mirror :: Ptr Ax2d -> Ptr Ax2d -> IO ()
mirror theAx2d mirrorAxis = [C.throwBlock| void {
  $(gp_Ax2d* theAx2d)->Mirror(*$(gp_Ax2d* mirrorAxis));
} |]

mirrored :: Ptr Ax2d -> Ptr Ax2d -> Acquire (Ptr Ax2d)
mirrored axis mirrorAxis = mkAcquire createMirrored deleteAx2d
  where
    createMirrored = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d($(gp_Ax2d* axis)->Mirrored(*$(gp_Ax2d* mirrorAxis)));
    } |]

mirrorAboutPnt2d :: Ptr Ax2d -> Ptr Pnt2d -> IO ()
mirrorAboutPnt2d theAx2d mirrorPnt = [C.throwBlock| void {
  $(gp_Ax2d* theAx2d)->Mirror(*$(gp_Pnt2d* mirrorPnt));
} |]

mirroredAboutPnt2d :: Ptr Ax2d -> Ptr Pnt2d -> Acquire (Ptr Ax2d)
mirroredAboutPnt2d axis mirrorPnt = mkAcquire createMirrored deleteAx2d
  where
    createMirrored = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d($(gp_Ax2d* axis)->Mirrored(*$(gp_Pnt2d* mirrorPnt)));
    } |]

-- rotate/rotated 

rotate :: Ptr Ax2d -> Ptr Pnt2d -> Double -> IO ()
rotate theAx2d axisOfRotation angleOfRotation = 
  let cAngle = realToFrac angleOfRotation
  in [C.throwBlock| void {
    $(gp_Ax2d* theAx2d)->Rotate(*$(gp_Pnt2d* axisOfRotation), $(double cAngle));
  } |]

rotated :: Ptr Ax2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Ax2d)
rotated axis axisOfRotation angleOfRotation = 
  let cAngle = realToFrac angleOfRotation
      createRotated = [C.throwBlock| gp_Ax2d* {
        return new gp_Ax2d($(gp_Ax2d* axis)->Rotated(*$(gp_Pnt2d* axisOfRotation), $(double cAngle)));
      } |]
  in mkAcquire createRotated deleteAx2d

-- scale/scaled 

scale :: Ptr Ax2d -> Ptr Pnt2d -> Double -> IO ()
scale theAx2d origin amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Ax2d* theAx2d)->Scale(*$(gp_Pnt2d* origin), $(double cAmount));
  } |]

scaled :: Ptr Ax2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Ax2d)
scaled axis origin amount = 
  let cAmount = realToFrac amount
      createScaled = [C.throwBlock| gp_Ax2d* {
        return new gp_Ax2d($(gp_Ax2d* axis)->Scaled(*$(gp_Pnt2d* origin), $(double cAmount)));
      } |]
  in mkAcquire createScaled deleteAx2d

-- transform/transformed 

transform :: Ptr Ax2d -> Ptr Trsf2d -> IO ()
transform theAx2d trsf = [C.throwBlock| void {
  $(gp_Ax2d* theAx2d)->Transform(*$(gp_Trsf2d* trsf));
} |]

transformed :: Ptr Ax2d -> Ptr Trsf2d -> Acquire (Ptr Ax2d)
transformed axis trsf = mkAcquire createTransformed deleteAx2d
  where
    createTransformed = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d($(gp_Ax2d* axis)->Transformed(*$(gp_Trsf2d* trsf)));
    } |]


-- translate/translated

translate :: Ptr Ax2d -> Ptr Vec2d -> IO ()
translate theAx2d vec = [C.throwBlock| void {
  $(gp_Ax2d* theAx2d)->Translate(*$(gp_Vec2d* vec));
} |]

translated :: Ptr Ax2d -> Ptr Vec2d -> Acquire (Ptr Ax2d)
translated axis vec = mkAcquire createTranslated deleteAx2d
  where
    createTranslated = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d($(gp_Ax2d* axis)->Translated(*$(gp_Vec2d* vec)));
    } |]

translateRelative :: Ptr Ax2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO ()
translateRelative theAx2d from to = [C.throwBlock| void {
  $(gp_Ax2d* theAx2d)->Translate(*$(gp_Pnt2d* from), *$(gp_Pnt2d* to));
} |]

translatedRelative :: Ptr Ax2d -> Ptr Pnt2d -> Ptr Pnt2d -> Acquire (Ptr Ax2d)
translatedRelative axis from to = mkAcquire createTranslatedRelative deleteAx2d
  where
    createTranslatedRelative = [C.throwBlock| gp_Ax2d* {
      return new gp_Ax2d($(gp_Ax2d* axis)->Translated(*$(gp_Pnt2d* from), *$(gp_Pnt2d* to)));
    } |]

