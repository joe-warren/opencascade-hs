module OpenCascade.GP.Ax3 
( Ax3
, new
, fromAx2
, fromPntDirAndDir
, fromPntAndDir
, xReverse
, yReverse
, zReverse
, setAxis
, setDirection
, setLocation
, setXDirection
, setYDirection
, angle
, axis
, ax2
, direction
, location
, xDirection
, yDirection
, direct
, isCoplanar
, isCoplanarAx1
, mirror
, mirrored
, mirrorAx2
, mirroredAx2
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
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext)

C.include "<gp_Ax3.hxx>"
C.include "<gp_Pnt.hxx>"
C.include "<gp_Dir.hxx>"
C.include "<gp_Ax1.hxx>"
C.include "<gp_Ax2.hxx>"
C.include "<gp_Trsf.hxx>"
C.include "<gp_Vec.hxx>"

new :: Acquire (Ptr Ax3)
new = mkAcquire createAx3 deleteAx3
  where
    createAx3 = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3();
    } |]

fromAx2 :: Ptr Ax2 -> Acquire (Ptr Ax3)
fromAx2 ax = mkAcquire createFromAx2 deleteAx3
  where
    createFromAx2 = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3(*$(gp_Ax2* ax));
    } |]

fromPntDirAndDir :: Ptr Pnt -> Ptr Dir -> Ptr Dir -> Acquire (Ptr Ax3)
fromPntDirAndDir pnt u v = mkAcquire createFromPntDirAndDir deleteAx3
  where
    createFromPntDirAndDir = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3(*$(gp_Pnt* pnt), *$(gp_Dir* u), *$(gp_Dir* v));
    } |] 

fromPntAndDir :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Ax3)
fromPntAndDir pnt dir = mkAcquire createFromPntAndDir deleteAx3
  where
    createFromPntAndDir = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3(*$(gp_Pnt* pnt), *$(gp_Dir* dir));
    } |] 

xReverse :: Ptr Ax3 -> IO ()
xReverse ax3 = [C.throwBlock| void {
  $(gp_Ax3* ax3)->XReverse();
} |]

yReverse :: Ptr Ax3 -> IO ()
yReverse ax3 = [C.throwBlock| void {
  $(gp_Ax3* ax3)->YReverse();
} |]

zReverse :: Ptr Ax3 -> IO ()
zReverse ax3 = [C.throwBlock| void {
  $(gp_Ax3* ax3)->ZReverse();
} |]

setAxis :: Ptr Ax3 -> Ptr Ax1 -> IO ()
setAxis ax3 axis = [C.throwBlock| void {
  $(gp_Ax3* ax3)->SetAxis(*$(gp_Ax1* axis));
} |]

setDirection :: Ptr Ax3 -> Ptr Dir -> IO ()
setDirection ax3 dir = [C.throwBlock| void {
  $(gp_Ax3* ax3)->SetDirection(*$(gp_Dir* dir));
} |]

setLocation :: Ptr Ax3 -> Ptr Pnt -> IO ()
setLocation ax3 pnt = [C.throwBlock| void {
  $(gp_Ax3* ax3)->SetLocation(*$(gp_Pnt* pnt));
} |]

setXDirection :: Ptr Ax3 -> Ptr Dir -> IO ()
setXDirection ax3 dir = [C.throwBlock| void {
  $(gp_Ax3* ax3)->SetXDirection(*$(gp_Dir* dir));
} |]

setYDirection :: Ptr Ax3 -> Ptr Dir -> IO ()
setYDirection ax3 dir = [C.throwBlock| void {
  $(gp_Ax3* ax3)->SetYDirection(*$(gp_Dir* dir));
} |]

angle :: Ptr Ax3 -> Ptr Ax3 -> IO Double
angle a b = do
  result <- [C.throwBlock| double {
    return $(gp_Ax3* a)->Angle(*$(gp_Ax3* b));
  } |]
  return (realToFrac result)

axis :: Ptr Ax3 -> Acquire (Ptr Ax1)
axis this = mkAcquire createAxis deleteAx1
  where
    createAxis = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax3* this)->Axis());
    } |]

ax2 :: Ptr Ax3 -> Acquire (Ptr Ax2)
ax2 this = mkAcquire createAx2 deleteAx2
  where
    createAx2 = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2($(gp_Ax3* this)->Ax2());
    } |]

direction :: Ptr Ax3 -> Acquire (Ptr Dir)
direction this = mkAcquire createDirection deleteDir
  where
    createDirection = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Ax3* this)->Direction());
    } |]

location :: Ptr Ax3 -> Acquire (Ptr Pnt)
location this = mkAcquire createLocation deletePnt
  where
    createLocation = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Ax3* this)->Location());
    } |]

xDirection :: Ptr Ax3 -> Acquire (Ptr Dir)
xDirection this = mkAcquire createXDirection deleteDir
  where
    createXDirection = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Ax3* this)->XDirection());
    } |]

yDirection :: Ptr Ax3 -> Acquire (Ptr Dir)
yDirection this = mkAcquire createYDirection deleteDir
  where
    createYDirection = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Ax3* this)->YDirection());
    } |]

direct :: Ptr Ax3 -> IO Bool
direct this = do
  result <- [C.throwBlock| bool {
    return $(gp_Ax3* this)->Direct();
  } |]
  return (cBoolToBool result)

isCoplanar :: Ptr Ax3 -> Ptr Ax3 -> Double -> Double -> IO Bool
isCoplanar a b linearTol angularTol = do
  let cLinearTol = realToFrac linearTol
      cAngularTol = realToFrac angularTol
  result <- [C.throwBlock| bool {
    return $(gp_Ax3* a)->IsCoplanar(*$(gp_Ax3* b), $(double cLinearTol), $(double cAngularTol));
  } |]
  return (cBoolToBool result)

isCoplanarAx1 :: Ptr Ax3 -> Ptr Ax1 -> Double -> Double -> IO Bool
isCoplanarAx1 a b linearTol angularTol = do
  let cLinearTol = realToFrac linearTol
      cAngularTol = realToFrac angularTol
  result <- [C.throwBlock| bool {
    return $(gp_Ax3* a)->IsCoplanar(*$(gp_Ax1* b), $(double cLinearTol), $(double cAngularTol));
  } |]
  return (cBoolToBool result)

mirror:: Ptr Ax3 -> Ptr Ax1 -> IO ()
mirror theAx3 mirrorAxis = [C.throwBlock| void {
  $(gp_Ax3* theAx3)->Mirror(*$(gp_Ax1* mirrorAxis));
} |]

mirrored :: Ptr Ax3 -> Ptr Ax1 -> Acquire (Ptr Ax3)
mirrored ax mirrorAxis = mkAcquire createMirrored deleteAx3
  where
    createMirrored = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3($(gp_Ax3* ax)->Mirrored(*$(gp_Ax1* mirrorAxis)));
    } |]

mirrorAx2:: Ptr Ax3 -> Ptr Ax2 -> IO ()
mirrorAx2 theAx3 mirrorAxis = [C.throwBlock| void {
  $(gp_Ax3* theAx3)->Mirror(*$(gp_Ax2* mirrorAxis));
} |]

mirroredAx2 :: Ptr Ax3 -> Ptr Ax2 -> Acquire (Ptr Ax3)
mirroredAx2 ax mirrorAxis = mkAcquire createMirrored deleteAx3
  where
    createMirrored = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3($(gp_Ax3* ax)->Mirrored(*$(gp_Ax2* mirrorAxis)));
    } |]

rotate :: Ptr Ax3 -> Ptr Ax1 -> Double -> IO ()
rotate theAx3 axisOfRotation angle = 
  let cAngle = realToFrac angle
  in [C.throwBlock| void {
    $(gp_Ax3* theAx3)->Rotate(*$(gp_Ax1* axisOfRotation), $(double cAngle));
  } |]

rotated :: Ptr Ax3 -> Ptr Ax1 -> Double -> Acquire (Ptr Ax3)
rotated ax axisOfRotation angleOfRotation = 
  let cAngle = realToFrac angleOfRotation
      createRotated = [C.throwBlock| gp_Ax3* {
        return new gp_Ax3($(gp_Ax3* ax)->Rotated(*$(gp_Ax1* axisOfRotation), $(double cAngle)));
      } |]
  in mkAcquire createRotated deleteAx3

scale :: Ptr Ax3 -> Ptr Pnt -> Double -> IO ()
scale theAx3 origin factor = 
  let cFactor = realToFrac factor
  in [C.throwBlock| void {
    $(gp_Ax3* theAx3)->Scale(*$(gp_Pnt* origin), $(double cFactor));
  } |]

scaled :: Ptr Ax3 -> Ptr Pnt -> Double -> Acquire (Ptr Ax3)
scaled ax origin factor = 
  let cFactor = realToFrac factor
      createScaled = [C.throwBlock| gp_Ax3* {
        return new gp_Ax3($(gp_Ax3* ax)->Scaled(*$(gp_Pnt* origin), $(double cFactor)));
      } |]
  in mkAcquire createScaled deleteAx3


transform:: Ptr Ax3 -> Ptr Trsf -> IO ()
transform theAx3 trsf = [C.throwBlock| void {
  $(gp_Ax3* theAx3)->Transform(*$(gp_Trsf* trsf));
} |]

transformed :: Ptr Ax3 -> Ptr Trsf -> Acquire (Ptr Ax3)
transformed ax trsf = mkAcquire createTransformed deleteAx3
  where
    createTransformed = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3($(gp_Ax3* ax)->Transformed(*$(gp_Trsf* trsf)));
    } |]


translate :: Ptr Ax3 -> Ptr Vec -> IO ()
translate theAx3 vec = [C.throwBlock| void {
  $(gp_Ax3* theAx3)->Translate(*$(gp_Vec* vec));
} |]

translated :: Ptr Ax3 -> Ptr Vec -> Acquire (Ptr Ax3)
translated ax vec = mkAcquire createTranslated deleteAx3
  where
    createTranslated = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3($(gp_Ax3* ax)->Translated(*$(gp_Vec* vec)));
    } |]

translateRelative :: Ptr Ax3 -> Ptr Pnt -> Ptr Pnt -> IO ()
translateRelative theAx3 from to = [C.throwBlock| void {
  $(gp_Ax3* theAx3)->Translate(*$(gp_Pnt* from), *$(gp_Pnt* to));
} |]

translatedRelative :: Ptr Ax3 -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Ax3)
translatedRelative ax from to = mkAcquire createTranslatedRelative deleteAx3
  where
    createTranslatedRelative = [C.throwBlock| gp_Ax3* {
      return new gp_Ax3($(gp_Ax3* ax)->Translated(*$(gp_Pnt* from), *$(gp_Pnt* to)));
    } |]