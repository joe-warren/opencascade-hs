module OpenCascade.GP.Pnt 
( Pnt
, new
, getX
, getY
, getZ 
, setX
, setY 
, setZ
, distance
, squareDistance
, baryCenter
, isEqual
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

C.include "<gp_Pnt.hxx>"

-- new

new :: Double -> Double -> Double -> Acquire (Ptr Pnt)
new x y z = 
  let cx = realToFrac x
      cy = realToFrac y
      cz = realToFrac z
      createPnt = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt($(double cx), $(double cy), $(double cz));
      } |]
  in mkAcquire createPnt deletePnt

-- getters

getX :: Ptr Pnt -> IO Double
getX pnt = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt* pnt)->X();
  } |]
  return (realToFrac result)

getY :: Ptr Pnt -> IO Double
getY pnt = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt* pnt)->Y();
  } |]
  return (realToFrac result)

getZ :: Ptr Pnt -> IO Double
getZ pnt = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt* pnt)->Z();
  } |]
  return (realToFrac result)

-- setters

setX :: Ptr Pnt -> Double -> IO ()
setX pnt x = 
  let cx = realToFrac x
  in [C.throwBlock| void {
    $(gp_Pnt* pnt)->SetX($(double cx));
  } |]

setY :: Ptr Pnt -> Double -> IO ()
setY pnt y = 
  let cy = realToFrac y
  in [C.throwBlock| void {
    $(gp_Pnt* pnt)->SetY($(double cy));
  } |]

setZ :: Ptr Pnt -> Double -> IO ()
setZ pnt z = 
  let cz = realToFrac z
  in [C.throwBlock| void {
    $(gp_Pnt* pnt)->SetZ($(double cz));
  } |]


-- distance and quadrance

distance :: Ptr Pnt -> Ptr Pnt -> IO Double
distance a b = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt* a)->Distance(*$(gp_Pnt* b));
  } |]
  return (realToFrac result)

squareDistance :: Ptr Pnt -> Ptr Pnt -> IO Double
squareDistance a b = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt* a)->SquareDistance(*$(gp_Pnt* b));
  } |]
  return (realToFrac result)

-- baryCenter

baryCenter :: Ptr Pnt -> Double -> Ptr Pnt -> Double -> IO ()
baryCenter a alpha b beta = 
  let cAlpha = realToFrac alpha
      cBeta = realToFrac beta
  in [C.throwBlock| void {
    $(gp_Pnt* a)->BaryCenter($(double cAlpha), *$(gp_Pnt* b), $(double cBeta));
  } |]

-- isEqual

isEqual :: Ptr Pnt -> Ptr Pnt -> Double -> IO Bool
isEqual a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Pnt* a)->IsEqual(*$(gp_Pnt* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- mirror/mirrored

mirror :: Ptr Pnt -> Ptr Pnt -> IO ()
mirror thePnt theAxis = [C.throwBlock| void {
  $(gp_Pnt* thePnt)->Mirror(*$(gp_Pnt* theAxis));
} |]

mirrored :: Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Pnt)
mirrored point axis = mkAcquire createMirrored deletePnt
  where
    createMirrored = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Pnt* point)->Mirrored(*$(gp_Pnt* axis)));
    } |]

mirrorAboutAx1 :: Ptr Pnt -> Ptr Ax1 -> IO ()
mirrorAboutAx1 thePnt theAxis = [C.throwBlock| void {
  $(gp_Pnt* thePnt)->Mirror(*$(gp_Ax1* theAxis));
} |]

mirroredAboutAx1 :: Ptr Pnt -> Ptr Ax1 -> Acquire (Ptr Pnt)
mirroredAboutAx1 point axis = mkAcquire createMirrored deletePnt
  where
    createMirrored = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Pnt* point)->Mirrored(*$(gp_Ax1* axis)));
    } |]

mirrorAboutAx2 :: Ptr Pnt -> Ptr Ax2 -> IO ()
mirrorAboutAx2 thePnt theAxis = [C.throwBlock| void {
  $(gp_Pnt* thePnt)->Mirror(*$(gp_Ax2* theAxis));
} |]

mirroredAboutAx2 :: Ptr Pnt -> Ptr Ax2 -> Acquire (Ptr Pnt)
mirroredAboutAx2 point axis = mkAcquire createMirrored deletePnt
  where
    createMirrored = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Pnt* point)->Mirrored(*$(gp_Ax2* axis)));
    } |]

-- rotate/rotated

rotate :: Ptr Pnt -> Ptr Ax1 -> Double -> IO ()
rotate thePnt theAxis amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Pnt* thePnt)->Rotate(*$(gp_Ax1* theAxis), $(double cAmount));
  } |]

rotated :: Ptr Pnt -> Ptr Ax1 -> Double -> Acquire (Ptr Pnt)
rotated point axis amount = 
  let cAmount = realToFrac amount
      createRotated = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt($(gp_Pnt* point)->Rotated(*$(gp_Ax1* axis), $(double cAmount)));
      } |]
  in mkAcquire createRotated deletePnt

-- scale/scaled

scale :: Ptr Pnt -> Ptr Pnt -> Double -> IO ()
scale thePnt origin amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Pnt* thePnt)->Scale(*$(gp_Pnt* origin), $(double cAmount));
  } |]

scaled :: Ptr Pnt -> Ptr Pnt -> Double -> Acquire (Ptr Pnt)
scaled point origin amount = 
  let cAmount = realToFrac amount
      createScaled = [C.throwBlock| gp_Pnt* {
        return new gp_Pnt($(gp_Pnt* point)->Scaled(*$(gp_Pnt* origin), $(double cAmount)));
      } |]
  in mkAcquire createScaled deletePnt

-- transform/transformed

transform :: Ptr Pnt -> Ptr Trsf -> IO ()
transform thePnt trsf = [C.throwBlock| void {
  $(gp_Pnt* thePnt)->Transform(*$(gp_Trsf* trsf));
} |]

transformed :: Ptr Pnt -> Ptr Trsf -> Acquire (Ptr Pnt)
transformed point trsf = mkAcquire createTransformed deletePnt
  where
    createTransformed = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Pnt* point)->Transformed(*$(gp_Trsf* trsf)));
    } |]

-- translate/translated

translate :: Ptr Pnt -> Ptr Vec -> IO ()
translate thePnt vec = [C.throwBlock| void {
  $(gp_Pnt* thePnt)->Translate(*$(gp_Vec* vec));
} |]

translated :: Ptr Pnt -> Ptr Vec -> Acquire (Ptr Pnt)
translated point vec = mkAcquire createTranslated deletePnt
  where
    createTranslated = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Pnt* point)->Translated(*$(gp_Vec* vec)));
    } |]

-- translateRelative/translatedRelative

translateRelative :: Ptr Pnt -> Ptr Pnt -> Ptr Pnt -> IO ()
translateRelative thePnt from to = [C.throwBlock| void {
  $(gp_Pnt* thePnt)->Translate(*$(gp_Pnt* from), *$(gp_Pnt* to));
} |]

translatedRelative :: Ptr Pnt -> Ptr Pnt -> Ptr Pnt -> Acquire (Ptr Pnt)
translatedRelative point from to = mkAcquire createTranslatedRelative deletePnt
  where
    createTranslatedRelative = [C.throwBlock| gp_Pnt* {
      return new gp_Pnt($(gp_Pnt* point)->Translated(*$(gp_Pnt* from), *$(gp_Pnt* to)));
    } |]
