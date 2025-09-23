module OpenCascade.GP.Ax1
( Ax1
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
, mirrorAboutPnt
, mirroredAboutPnt
, mirrorAboutAx2
, mirroredAboutAx2
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

C.include "<gp_Ax1.hxx>"
C.include "<gp_Pnt.hxx>"
C.include "<gp_Dir.hxx>"
C.include "<gp_Ax2.hxx>"
C.include "<gp_Trsf.hxx>"
C.include "<gp_Vec.hxx>"

-- new and delete

new :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Ax1)
new origin dir = mkAcquire createAx1 deleteAx1
  where
    createAx1 = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1(*$(gp_Pnt* origin), *$(gp_Dir* dir));
    } |]

-- getters

location :: Ptr Ax1 -> Acquire (Ptr Pnt)
location ax1 = mkAcquire createLocation deletePnt
  where
    createLocation = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Ax1* ax1)->Location());
    } |]

direction :: Ptr Ax1 -> Acquire (Ptr Dir)
direction ax1 = mkAcquire createDirection deleteDir
  where
    createDirection = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Ax1* ax1)->Direction());
    } |]

-- setters

setDirection :: Ptr Ax1 -> Ptr Dir -> IO ()
setDirection ax1 dir = [C.throwBlock| void {
  $(gp_Ax1* ax1)->SetDirection(*$(gp_Dir* dir));
} |]

setLocation :: Ptr Ax1 -> Ptr Pnt -> IO ()
setLocation ax1 pnt = [C.throwBlock| void {
  $(gp_Ax1* ax1)->SetLocation(*$(gp_Pnt* pnt));
} |]

-- tests

-- isCoaxial

isCoaxial :: Ptr Ax1 -> Ptr Ax1 -> Double -> Double -> IO Bool
isCoaxial a b angularTolerance linearTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
      cLinearTolerance = realToFrac linearTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax1* a)->IsCoaxial(*$(gp_Ax1* b), $(double cAngularTolerance), $(double cLinearTolerance));
  } |]
  return (cBoolToBool result)

-- isNormal

isNormal :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO Bool
isNormal a b angularTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax1* a)->IsNormal(*$(gp_Ax1* b), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- isOpposite

isOpposite :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO Bool
isOpposite a b angularTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax1* a)->IsOpposite(*$(gp_Ax1* b), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- isParallel

isParallel :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO Bool
isParallel a b angularTolerance = do
  let cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax1* a)->IsParallel(*$(gp_Ax1* b), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- angle

angle :: Ptr Ax1 -> Ptr Ax1 -> IO Double
angle a b = do
  result <- [C.throwBlock| double {
    return $(gp_Ax1* a)->Angle(*$(gp_Ax1* b));
  } |]
  return (realToFrac result)

-- reverse/reversed

reverse :: Ptr Ax1 -> IO ()
reverse axis = [C.throwBlock| void {
  $(gp_Ax1* axis)->Reverse();
} |]

reversed :: Ptr Ax1 -> Acquire (Ptr Ax1)
reversed axis = mkAcquire createReversed deleteAx1
  where
    createReversed = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax1* axis)->Reversed());
    } |]

-- mirror/mirrored

mirror :: Ptr Ax1 -> Ptr Ax1 -> IO ()
mirror theAx1 mirrorAxis = [C.throwBlock| void {
  $(gp_Ax1* theAx1)->Mirror(*$(gp_Ax1* mirrorAxis));
} |]

mirrored :: Ptr Ax1 -> Ptr Ax1 -> Acquire (Ptr Ax1)
mirrored axis mirrorAxis = mkAcquire createMirrored deleteAx1
  where
    createMirrored = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax1* axis)->Mirrored(*$(gp_Ax1* mirrorAxis)));
    } |]

mirrorAboutPnt :: Ptr Ax1 -> Ptr Pnt -> IO ()
mirrorAboutPnt theAx1 mirrorPnt = [C.throwBlock| void {
  $(gp_Ax1* theAx1)->Mirror(*$(gp_Pnt* mirrorPnt));
} |]

mirroredAboutPnt :: Ptr Ax1 -> Ptr Pnt -> Acquire (Ptr Ax1)
mirroredAboutPnt axis mirrorPnt = mkAcquire createMirrored deleteAx1
  where
    createMirrored = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax1* axis)->Mirrored(*$(gp_Pnt* mirrorPnt)));
    } |]


mirrorAboutAx2 :: Ptr Ax1 -> Ptr Ax2 -> IO ()
mirrorAboutAx2 theAx1 mirrorAxis = [C.throwBlock| void {
  $(gp_Ax1* theAx1)->Mirror(*$(gp_Ax2* mirrorAxis));
} |]

mirroredAboutAx2 :: Ptr Ax1 -> Ptr Ax2 -> Acquire (Ptr Ax1)
mirroredAboutAx2 axis mirrorAxis = mkAcquire createMirrored deleteAx1
  where
    createMirrored = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax1* axis)->Mirrored(*$(gp_Ax2* mirrorAxis)));
    } |]

-- rotate/rotated 

rotate :: Ptr Ax1 -> Ptr Ax1 -> Double -> IO ()
rotate theAx1 axisOfRotation angleOfRotation = 
  let cAngle = realToFrac angleOfRotation
  in [C.throwBlock| void {
    $(gp_Ax1* theAx1)->Rotate(*$(gp_Ax1* axisOfRotation), $(double cAngle));
  } |]

rotated :: Ptr Ax1 -> Ptr Ax1 -> Double -> Acquire (Ptr Ax1)
rotated axis axisOfRotation angleOfRotation = 
  let cAngle = realToFrac angleOfRotation
      createRotated = [C.throwBlock| gp_Ax1* {
        return new gp_Ax1($(gp_Ax1* axis)->Rotated(*$(gp_Ax1* axisOfRotation), $(double cAngle)));
      } |]
  in mkAcquire createRotated deleteAx1

-- scale/scaled 

scale :: Ptr Ax1 -> Ptr Pnt -> Double -> IO ()
scale theAx1 origin amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Ax1* theAx1)->Scale(*$(gp_Pnt* origin), $(double cAmount));
  } |]

scaled :: Ptr Ax1 -> Ptr Pnt -> Double -> Acquire (Ptr Ax1)
scaled axis origin amount = 
  let cAmount = realToFrac amount
      createScaled = [C.throwBlock| gp_Ax1* {
        return new gp_Ax1($(gp_Ax1* axis)->Scaled(*$(gp_Pnt* origin), $(double cAmount)));
      } |]
  in mkAcquire createScaled deleteAx1

-- transform/transformed 

transform :: Ptr Ax1 -> Ptr Trsf -> IO ()
transform theAx1 trsf = [C.throwBlock| void {
  $(gp_Ax1* theAx1)->Transform(*$(gp_Trsf* trsf));
} |]

transformed :: Ptr Ax1 -> Ptr Trsf -> Acquire (Ptr Ax1)
transformed axis trsf = mkAcquire createTransformed deleteAx1
  where
    createTransformed = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax1* axis)->Transformed(*$(gp_Trsf* trsf)));
    } |]


-- translate/translated

translate :: Ptr Ax1 -> Ptr Vec -> IO ()
translate theAx1 vec = [C.throwBlock| void {
  $(gp_Ax1* theAx1)->Translate(*$(gp_Vec* vec));
} |]

translated :: Ptr Ax1 -> Ptr Vec -> Acquire (Ptr Ax1)
translated axis vec = mkAcquire createTranslated deleteAx1
  where
    createTranslated = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax1* axis)->Translated(*$(gp_Vec* vec)));
    } |]

translateRelative :: Ptr Ax1 -> Ptr Pnt -> Ptr Pnt -> IO ()
translateRelative theAx1 from to = [C.throwBlock| void {
  $(gp_Ax1* theAx1)->Translate(*$(gp_Pnt* from), *$(gp_Pnt* to));
} |]

translatedRelative :: Ptr Ax1 -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Ax1)
translatedRelative axis from to = mkAcquire createTranslatedRelative deleteAx1
  where
    createTranslatedRelative = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax1* axis)->Translated(*$(gp_Pnt* from), *$(gp_Pnt* to)));
    } |]
