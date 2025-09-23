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
import OpenCascade.GP.Internal.Context
import OpenCascade.GP.Internal.Destructors
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire

C.context (C.cppCtx <> gpContext)

C.include "<gp_Dir.hxx>"
C.include "<gp_Vec.hxx>"
C.include "<gp_Ax1.hxx>"
C.include "<gp_Ax2.hxx>"
C.include "<gp_Trsf.hxx>" 

-- new

new :: Double -> Double -> Double -> Acquire (Ptr Dir)
new x y z = 
  let cx = realToFrac x
      cy = realToFrac y
      cz = realToFrac z
      createDir = [C.throwBlock| gp_Dir* {
        return new gp_Dir($(double cx), $(double cy), $(double cz));
      } |]
  in mkAcquire createDir deleteDir

-- getters

getX :: Ptr Dir -> IO Double
getX dir = do
  result <- [C.throwBlock| double {
    return $(gp_Dir* dir)->X();
  } |]
  return (realToFrac result)

getY :: Ptr Dir -> IO Double
getY dir = do
  result <- [C.throwBlock| double {
    return $(gp_Dir* dir)->Y();
  } |]
  return (realToFrac result)

getZ :: Ptr Dir -> IO Double
getZ dir = do
  result <- [C.throwBlock| double {
    return $(gp_Dir* dir)->Z();
  } |]
  return (realToFrac result)

-- setters

setX :: Ptr Dir -> Double -> IO ()
setX dir x = 
  let cx = realToFrac x
  in [C.throwBlock| void {
    $(gp_Dir* dir)->SetX($(double cx));
  } |]

setY :: Ptr Dir -> Double -> IO ()
setY dir y = 
  let cy = realToFrac y
  in [C.throwBlock| void {
    $(gp_Dir* dir)->SetY($(double cy));
  } |]

setZ :: Ptr Dir -> Double -> IO ()
setZ dir z = 
  let cz = realToFrac z
  in [C.throwBlock| void {
    $(gp_Dir* dir)->SetZ($(double cz));
  } |]

-- tests

-- isEqual

isEqual :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isEqual a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir* a)->IsEqual(*$(gp_Dir* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isNormal

isNormal :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isNormal a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir* a)->IsNormal(*$(gp_Dir* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)


-- isOpposite

isOpposite :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isOpposite a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir* a)->IsOpposite(*$(gp_Dir* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isParallel

isParallel :: Ptr Dir -> Ptr Dir -> Double -> IO Bool
isParallel a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir* a)->IsParallel(*$(gp_Dir* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- angle

angle :: Ptr Dir -> Ptr Dir -> IO Double
angle a b = do
  result <- [C.throwBlock| double {
    return $(gp_Dir* a)->Angle(*$(gp_Dir* b));
  } |]
  return (realToFrac result)

-- angleWithRef

angleWithRef :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO Double
angleWithRef a b ref = do
  result <- [C.throwBlock| double {
    return $(gp_Dir* a)->AngleWithRef(*$(gp_Dir* b), *$(gp_Dir* ref));
  } |]
  return (realToFrac result)

-- cross/crossed

cross :: Ptr Dir -> Ptr Dir -> IO ()
cross theDir other = [C.throwBlock| void {
  $(gp_Dir* theDir)->Cross(*$(gp_Dir* other));
} |]

crossed :: Ptr Dir -> Ptr Dir -> Acquire (Ptr Dir)
crossed a b = mkAcquire createCrossed deleteDir
  where
    createCrossed = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Dir* a)->Crossed(*$(gp_Dir* b)));
    } |]


-- crossCross/crossCrossed

crossCross :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO ()
crossCross theDir v1 v2 = [C.throwBlock| void {
  $(gp_Dir* theDir)->CrossCross(*$(gp_Dir* v1), *$(gp_Dir* v2));
} |]

crossCrossed :: Ptr Dir -> Ptr Dir -> Ptr Dir -> Acquire (Ptr Dir)
crossCrossed a b c = mkAcquire createCrossCrossed deleteDir
  where
    createCrossCrossed = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Dir* a)->CrossCrossed(*$(gp_Dir* b), *$(gp_Dir* c)));
    } |]


-- dot

dot :: Ptr Dir -> Ptr Dir -> IO Double
dot a b = do
  result <- [C.throwBlock| double {
    return $(gp_Dir* a)->Dot(*$(gp_Dir* b));
  } |]
  return (realToFrac result)


-- dotCross

dotCross :: Ptr Dir -> Ptr Dir -> Ptr Dir -> IO Double
dotCross a b c = do
  result <- [C.throwBlock| double {
    return $(gp_Dir* a)->DotCross(*$(gp_Dir* b), *$(gp_Dir* c));
  } |]
  return (realToFrac result)


-- reverse/reversed

reverse :: Ptr Dir -> IO ()
reverse dir = [C.throwBlock| void {
  $(gp_Dir* dir)->Reverse();
} |]

reversed :: Ptr Dir -> Acquire (Ptr Dir)
reversed axis = mkAcquire createReversed deleteDir
  where
    createReversed = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Dir* axis)->Reversed());
    } |]

-- mirror/mirrored

mirror :: Ptr Dir -> Ptr Dir -> IO ()
mirror theDir mirrorDir = [C.throwBlock| void {
  $(gp_Dir* theDir)->Mirror(*$(gp_Dir* mirrorDir));
} |]

mirrored :: Ptr Dir -> Ptr Dir -> Acquire (Ptr Dir)
mirrored point axis = mkAcquire createMirrored deleteDir
  where
    createMirrored = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Dir* point)->Mirrored(*$(gp_Dir* axis)));
    } |]

mirrorAboutAx1 :: Ptr Dir -> Ptr Ax1 -> IO ()
mirrorAboutAx1 theDir mirrorAxis = [C.throwBlock| void {
  $(gp_Dir* theDir)->Mirror(*$(gp_Ax1* mirrorAxis));
} |]

mirroredAboutAx1 :: Ptr Dir -> Ptr Ax1 -> Acquire (Ptr Dir)
mirroredAboutAx1 point axis = mkAcquire createMirrored deleteDir
  where
    createMirrored = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Dir* point)->Mirrored(*$(gp_Ax1* axis)));
    } |]

mirrorAboutAx2 :: Ptr Dir -> Ptr Ax2 -> IO ()
mirrorAboutAx2 theDir mirrorAxis = [C.throwBlock| void {
  $(gp_Dir* theDir)->Mirror(*$(gp_Ax2* mirrorAxis));
} |]

mirroredAboutAx2 :: Ptr Dir -> Ptr Ax2 -> Acquire (Ptr Dir)
mirroredAboutAx2 point axis = mkAcquire createMirrored deleteDir
  where
    createMirrored = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Dir* point)->Mirrored(*$(gp_Ax2* axis)));
    } |]

-- rotate/rotated

rotate :: Ptr Dir -> Ptr Ax1 -> Double -> IO ()
rotate theDir axis amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Dir* theDir)->Rotate(*$(gp_Ax1* axis), $(double cAmount));
  } |]

rotated :: Ptr Dir -> Ptr Ax1 -> Double -> Acquire (Ptr Dir)
rotated point axis amount = 
  let cAmount = realToFrac amount
      createRotated = [C.throwBlock| gp_Dir* {
        return new gp_Dir($(gp_Dir* point)->Rotated(*$(gp_Ax1* axis), $(double cAmount)));
      } |]
  in mkAcquire createRotated deleteDir

-- transform/transformed

transform :: Ptr Dir -> Ptr Trsf -> IO ()
transform theDir trsf = [C.throwBlock| void {
  $(gp_Dir* theDir)->Transform(*$(gp_Trsf* trsf));
} |]

transformed :: Ptr Dir -> Ptr Trsf -> Acquire (Ptr Dir)
transformed point trsf = mkAcquire createTransformed deleteDir
  where
    createTransformed = [C.throwBlock| gp_Dir* {
      return new gp_Dir($(gp_Dir* point)->Transformed(*$(gp_Trsf* trsf)));
    } |]
