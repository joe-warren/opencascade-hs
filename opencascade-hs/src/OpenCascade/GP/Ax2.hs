module OpenCascade.GP.Ax2
( Ax2
, new
, newAutoX
, location
, direction
, xDirection
, yDirection
, axis
, setLocation
, setDirection
, setXDirection
, setYDirection
, setAxis
, isCoplanar
, isCoplanarWithAx1
, mirror
, mirrored
, mirrorAboutPnt
, mirroredAboutPnt
, mirrorAboutAx1
, mirroredAboutAx1
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

C.include "<gp_Ax2.hxx>"
C.include "<gp_Pnt.hxx>"
C.include "<gp_Dir.hxx>"
C.include "<gp_Ax1.hxx>"
C.include "<gp_Trsf.hxx>"
C.include "<gp_Vec.hxx>"

-- new and delete

new :: Ptr Pnt -> Ptr Dir -> Ptr Dir -> Acquire (Ptr Ax2)
new origin vAxis vX = mkAcquire createAx2 deleteAx2
  where
    createAx2 = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2(*$(gp_Pnt* origin), *$(gp_Dir* vAxis), *$(gp_Dir* vX));
    } |]

newAutoX :: Ptr Pnt -> Ptr Dir -> Acquire (Ptr Ax2)
newAutoX origin dir = mkAcquire createAx2AutoX deleteAx2
  where
    createAx2AutoX = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2(*$(gp_Pnt* origin), *$(gp_Dir* dir));
    } |]


-- getters

location :: Ptr Ax2 -> Acquire (Ptr Pnt)
location ax2 = mkAcquire createLocation deletePnt
  where
    createLocation = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Ax2* ax2)->Location());
    } |]

direction :: Ptr Ax2 -> Acquire (Ptr Dir)
direction ax2 = mkAcquire createDirection deleteDir
  where
    createDirection = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Ax2* ax2)->Direction());
    } |]

xDirection :: Ptr Ax2 -> Acquire (Ptr Dir)
xDirection ax2 = mkAcquire createXDirection deleteDir
  where
    createXDirection = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Ax2* ax2)->XDirection());
    } |]

yDirection :: Ptr Ax2 -> Acquire (Ptr Dir)
yDirection ax2 = mkAcquire createYDirection deleteDir
  where
    createYDirection = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Ax2* ax2)->YDirection());
    } |]

axis :: Ptr Ax2 -> Acquire (Ptr Ax1)
axis ax2 = mkAcquire createAxis deleteAx1
  where
    createAxis = [C.throwBlock| gp_Ax1* {
      return new gp_Ax1($(gp_Ax2* ax2)->Axis());
    } |]

-- setters

setDirection :: Ptr Ax2 -> Ptr Dir -> IO ()
setDirection ax2 dir = [C.throwBlock| void {
  $(gp_Ax2* ax2)->SetDirection(*$(gp_Dir* dir));
} |]

setLocation :: Ptr Ax2 -> Ptr Pnt -> IO ()
setLocation ax2 pnt = [C.throwBlock| void {
  $(gp_Ax2* ax2)->SetLocation(*$(gp_Pnt* pnt));
} |]

setXDirection :: Ptr Ax2 -> Ptr Dir -> IO ()
setXDirection ax2 dir = [C.throwBlock| void {
  $(gp_Ax2* ax2)->SetXDirection(*$(gp_Dir* dir));
} |]

setYDirection :: Ptr Ax2 -> Ptr Dir -> IO ()
setYDirection ax2 dir = [C.throwBlock| void {
  $(gp_Ax2* ax2)->SetYDirection(*$(gp_Dir* dir));
} |]

setAxis :: Ptr Ax2 -> Ptr Ax1 -> IO ()
setAxis ax2 axis = [C.throwBlock| void {
  $(gp_Ax2* ax2)->SetAxis(*$(gp_Ax1* axis));
} |]

-- isCoplanar

isCoplanar :: Ptr Ax2 -> Ptr Ax2 -> Double -> Double -> IO Bool
isCoplanar a b linearTolerance angularTolerance = do
  let cLinearTolerance = realToFrac linearTolerance
      cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax2* a)->IsCoplanar(*$(gp_Ax2* b), $(double cLinearTolerance), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

isCoplanarWithAx1 :: Ptr Ax2 -> Ptr Ax1 -> Double -> Double -> IO Bool
isCoplanarWithAx1 a b linearTolerance angularTolerance = do
  let cLinearTolerance = realToFrac linearTolerance
      cAngularTolerance = realToFrac angularTolerance
  result <- [C.throwBlock| bool {
    return $(gp_Ax2* a)->IsCoplanar(*$(gp_Ax1* b), $(double cLinearTolerance), $(double cAngularTolerance));
  } |]
  return (cBoolToBool result)

-- mirror/mirrored

mirror :: Ptr Ax2 -> Ptr Ax2 -> IO ()
mirror theAx2 mirrorAxis = [C.throwBlock| void {
  $(gp_Ax2* theAx2)->Mirror(*$(gp_Ax2* mirrorAxis));
} |]

mirrored :: Ptr Ax2 -> Ptr Ax2 -> Acquire (Ptr Ax2)
mirrored ax mirrorAxis = mkAcquire createMirrored deleteAx2
  where
    createMirrored = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2($(gp_Ax2* ax)->Mirrored(*$(gp_Ax2* mirrorAxis)));
    } |]

mirrorAboutPnt :: Ptr Ax2 -> Ptr Pnt -> IO ()
mirrorAboutPnt theAx2 mirrorPnt = [C.throwBlock| void {
  $(gp_Ax2* theAx2)->Mirror(*$(gp_Pnt* mirrorPnt));
} |]

mirroredAboutPnt :: Ptr Ax2 -> Ptr Pnt -> Acquire (Ptr Ax2)
mirroredAboutPnt ax mirrorPnt = mkAcquire createMirrored deleteAx2
  where
    createMirrored = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2($(gp_Ax2* ax)->Mirrored(*$(gp_Pnt* mirrorPnt)));
    } |]

mirrorAboutAx1 :: Ptr Ax2 -> Ptr Ax1 -> IO ()
mirrorAboutAx1 theAx2 mirrorAxis = [C.throwBlock| void {
  $(gp_Ax2* theAx2)->Mirror(*$(gp_Ax1* mirrorAxis));
} |]

mirroredAboutAx1 :: Ptr Ax2 -> Ptr Ax1 -> Acquire (Ptr Ax2)
mirroredAboutAx1 ax mirrorAxis = mkAcquire createMirrored deleteAx2
  where
    createMirrored = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2($(gp_Ax2* ax)->Mirrored(*$(gp_Ax1* mirrorAxis)));
    } |]


-- rotate/rotated 

rotate :: Ptr Ax2 -> Ptr Ax1 -> Double -> IO ()
rotate theAx2 axisOfRotation angle = 
  let cAngle = realToFrac angle
  in [C.throwBlock| void {
    $(gp_Ax2* theAx2)->Rotate(*$(gp_Ax1* axisOfRotation), $(double cAngle));
  } |]

rotated :: Ptr Ax2 -> Ptr Ax1 -> Double -> Acquire (Ptr Ax2)
rotated ax axisOfRotation angle = 
  let cAngle = realToFrac angle
      createRotated = [C.throwBlock| gp_Ax2* {
        return new gp_Ax2($(gp_Ax2* ax)->Rotated(*$(gp_Ax1* axisOfRotation), $(double cAngle)));
      } |]
  in mkAcquire createRotated deleteAx2

-- scale/scaled 

scale :: Ptr Ax2 -> Ptr Pnt -> Double -> IO ()
scale theAx2 origin amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Ax2* theAx2)->Scale(*$(gp_Pnt* origin), $(double cAmount));
  } |]

scaled :: Ptr Ax2 -> Ptr Pnt -> Double -> Acquire (Ptr Ax2)
scaled ax origin amount = 
  let cAmount = realToFrac amount
      createScaled = [C.throwBlock| gp_Ax2* {
        return new gp_Ax2($(gp_Ax2* ax)->Scaled(*$(gp_Pnt* origin), $(double cAmount)));
      } |]
  in mkAcquire createScaled deleteAx2

-- transform/transformed 

transform :: Ptr Ax2 -> Ptr Trsf -> IO ()
transform theAx2 trsf = [C.throwBlock| void {
  $(gp_Ax2* theAx2)->Transform(*$(gp_Trsf* trsf));
} |]

transformed :: Ptr Ax2 -> Ptr Trsf -> Acquire (Ptr Ax2)
transformed ax trsf = mkAcquire createTransformed deleteAx2
  where
    createTransformed = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2($(gp_Ax2* ax)->Transformed(*$(gp_Trsf* trsf)));
    } |]


-- translate/translated

translate :: Ptr Ax2 -> Ptr Vec -> IO ()
translate theAx2 vec = [C.throwBlock| void {
  $(gp_Ax2* theAx2)->Translate(*$(gp_Vec* vec));
} |]

translated :: Ptr Ax2 -> Ptr Vec -> Acquire (Ptr Ax2)
translated ax vec = mkAcquire createTranslated deleteAx2
  where
    createTranslated = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2($(gp_Ax2* ax)->Translated(*$(gp_Vec* vec)));
    } |]

translateRelative :: Ptr Ax2 -> Ptr Pnt -> Ptr Pnt -> IO ()
translateRelative theAx2 from to = [C.throwBlock| void {
  $(gp_Ax2* theAx2)->Translate(*$(gp_Pnt* from), *$(gp_Pnt* to));
} |]

translatedRelative :: Ptr Ax2 -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Ax2)
translatedRelative ax from to = mkAcquire createTranslatedRelative deleteAx2
  where
    createTranslatedRelative = [C.throwBlock| gp_Ax2* {
      return new gp_Ax2($(gp_Ax2* ax)->Translated(*$(gp_Pnt* from), *$(gp_Pnt* to)));
    } |]

