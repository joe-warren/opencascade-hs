module OpenCascade.GP.Dir2d
( Dir2d
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
, crossed
, dot
, reverse
, reversed
, mirror
, mirrored
, mirrorAboutAx2d
, mirroredAboutAx2d
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

C.include "<gp_Dir2d.hxx>"
C.include "<gp_Pnt2d.hxx>"
C.include "<gp_Ax2d.hxx>"
C.include "<gp_Trsf2d.hxx>" 

-- new

new :: Double -> Double -> Acquire (Ptr Dir2d)
new x y = mkAcquire createDir2d deleteDir2d
  where
    createDir2d = let
      cx = realToFrac x
      cy = realToFrac y
      in [C.throwBlock| gp_Dir2d* {
        return new gp_Dir2d($(double cx), $(double cy));
      } |]

-- getters

getX :: Ptr Dir2d -> IO Double
getX dir = do
  result <- [C.throwBlock| double {
    return $(gp_Dir2d* dir)->X();
  } |]
  return (realToFrac result)

getY :: Ptr Dir2d -> IO Double
getY dir = do
  result <- [C.throwBlock| double {
    return $(gp_Dir2d* dir)->Y();
  } |]
  return (realToFrac result)

-- setters

setX :: Ptr Dir2d -> Double -> IO ()
setX dir x = 
  let cx = realToFrac x
  in [C.throwBlock| void {
    $(gp_Dir2d* dir)->SetX($(double cx));
  } |]

setY :: Ptr Dir2d -> Double -> IO ()
setY dir y = 
  let cy = realToFrac y
  in [C.throwBlock| void {
    $(gp_Dir2d* dir)->SetY($(double cy));
  } |]

-- tests

-- isEqual

isEqual :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isEqual a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir2d* a)->IsEqual(*$(gp_Dir2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isNormal

isNormal :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isNormal a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir2d* a)->IsNormal(*$(gp_Dir2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)


-- isOpposite

isOpposite :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isOpposite a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir2d* a)->IsOpposite(*$(gp_Dir2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- isParallel

isParallel :: Ptr Dir2d -> Ptr Dir2d -> Double -> IO Bool
isParallel a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Dir2d* a)->IsParallel(*$(gp_Dir2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- angle

angle :: Ptr Dir2d -> Ptr Dir2d -> IO Double
angle a b = do
  result <- [C.throwBlock| double {
    return $(gp_Dir2d* a)->Angle(*$(gp_Dir2d* b));
  } |]
  return (realToFrac result)

-- crossed

crossed :: Ptr Dir2d -> Ptr Dir2d -> IO Double
crossed a b = do
  result <- [C.throwBlock| double {
    return $(gp_Dir2d* a)->Crossed(*$(gp_Dir2d* b));
  } |]
  return (realToFrac result)

-- dot

dot :: Ptr Dir2d -> Ptr Dir2d -> IO Double
dot a b = do
  result <- [C.throwBlock| double {
    return $(gp_Dir2d* a)->Dot(*$(gp_Dir2d* b));
  } |]
  return (realToFrac result)

-- reverse/reversed

reverse :: Ptr Dir2d -> IO ()
reverse dir = [C.throwBlock| void {
  $(gp_Dir2d* dir)->Reverse();
} |]

reversed :: Ptr Dir2d -> Acquire (Ptr Dir2d)
reversed axis = mkAcquire createReversed deleteDir2d
  where
    createReversed = [C.throwBlock| gp_Dir2d* {
      return new gp_Dir2d($(gp_Dir2d* axis)->Reversed());
    } |]

-- mirror/mirrored

mirror :: Ptr Dir2d -> Ptr Dir2d -> IO ()
mirror theDir mirrorDir = [C.throwBlock| void {
  $(gp_Dir2d* theDir)->Mirror(*$(gp_Dir2d* mirrorDir));
} |]

mirrored :: Ptr Dir2d -> Ptr Dir2d -> Acquire (Ptr Dir2d)
mirrored point axis = mkAcquire createMirrored deleteDir2d
  where
    createMirrored = [C.throwBlock| gp_Dir2d* {
      return new gp_Dir2d($(gp_Dir2d* point)->Mirrored(*$(gp_Dir2d* axis)));
    } |]

mirrorAboutAx2d :: Ptr Dir2d -> Ptr Ax2d -> IO ()
mirrorAboutAx2d theDir mirrorAxis = [C.throwBlock| void {
  $(gp_Dir2d* theDir)->Mirror(*$(gp_Ax2d* mirrorAxis));
} |]

mirroredAboutAx2d :: Ptr Dir2d -> Ptr Ax2d -> Acquire (Ptr Dir2d)
mirroredAboutAx2d point axis = mkAcquire createMirrored deleteDir2d
  where
    createMirrored = [C.throwBlock| gp_Dir2d* {
      return new gp_Dir2d($(gp_Dir2d* point)->Mirrored(*$(gp_Ax2d* axis)));
    } |]

-- rotate/rotated

rotate :: Ptr Dir2d -> Double -> IO ()
rotate theDir amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Dir2d* theDir)->Rotate($(double cAmount));
  } |]

rotated :: Ptr Dir2d -> Double -> Acquire (Ptr Dir2d)
rotated point amount = 
  let cAmount = realToFrac amount
      createRotated = [C.throwBlock| gp_Dir2d* {
        return new gp_Dir2d($(gp_Dir2d* point)->Rotated($(double cAmount)));
      } |]
  in mkAcquire createRotated deleteDir2d

-- transform/transformed

transform :: Ptr Dir2d -> Ptr Trsf2d -> IO ()
transform theDir trsf = [C.throwBlock| void {
  $(gp_Dir2d* theDir)->Transform(*$(gp_Trsf2d* trsf));
} |]

transformed :: Ptr Dir2d -> Ptr Trsf2d -> Acquire (Ptr Dir2d)
transformed point trsf = mkAcquire createTransformed deleteDir2d
  where
    createTransformed = [C.throwBlock| gp_Dir2d* {
      return new gp_Dir2d($(gp_Dir2d* point)->Transformed(*$(gp_Trsf2d* trsf)));
    } |]
