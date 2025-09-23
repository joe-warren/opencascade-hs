module OpenCascade.GP.Pnt2d 
( Pnt2d
, new
, getX
, getY
, setX
, setY 
, distance
, squareDistance
, isEqual
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

C.include "<gp_Pnt2d.hxx>"
C.include "<gp_Ax2d.hxx>"
C.include "<gp_Vec2d.hxx>"
C.include "<gp_Trsf2d.hxx>" 

-- new

new :: Double -> Double -> Acquire (Ptr Pnt2d)
new x y = 
  let cx = realToFrac x
      cy = realToFrac y
      createPnt = [C.throwBlock| gp_Pnt2d* {
        return new gp_Pnt2d($(double cx), $(double cy));
      } |]
  in mkAcquire createPnt deletePnt2d

-- getters

getX :: Ptr Pnt2d -> IO Double
getX pnt = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt2d* pnt)->X();
  } |]
  return (realToFrac result)

getY :: Ptr Pnt2d -> IO Double
getY pnt = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt2d* pnt)->Y();
  } |]
  return (realToFrac result)

-- setters

setX :: Ptr Pnt2d -> Double -> IO ()
setX pnt x = 
  let cx = realToFrac x
  in [C.throwBlock| void {
    $(gp_Pnt2d* pnt)->SetX($(double cx));
  } |]

setY :: Ptr Pnt2d -> Double -> IO ()
setY pnt y = 
  let cy = realToFrac y
  in [C.throwBlock| void {
    $(gp_Pnt2d* pnt)->SetY($(double cy));
  } |]

-- distance and quadrance

distance :: Ptr Pnt2d -> Ptr Pnt2d -> IO Double
distance a b = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt2d* a)->Distance(*$(gp_Pnt2d* b));
  } |]
  return (realToFrac result)

squareDistance :: Ptr Pnt2d -> Ptr Pnt2d -> IO Double
squareDistance a b = do
  result <- [C.throwBlock| double {
    return $(gp_Pnt2d* a)->SquareDistance(*$(gp_Pnt2d* b));
  } |]
  return (realToFrac result)

-- isEqual

isEqual :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> IO Bool
isEqual a b tolerance = do
  let cTolerance = realToFrac tolerance
  result <- [C.throwBlock| bool {
    return $(gp_Pnt2d* a)->IsEqual(*$(gp_Pnt2d* b), $(double cTolerance));
  } |]
  return (cBoolToBool result)

-- mirror/mirrored

mirror :: Ptr Pnt2d -> Ptr Pnt2d -> IO ()
mirror thePnt theAxis = [C.throwBlock| void {
  $(gp_Pnt2d* thePnt)->Mirror(*$(gp_Pnt2d* theAxis));
} |]

mirrored :: Ptr Pnt2d -> Ptr Pnt2d -> Acquire (Ptr Pnt2d)
mirrored point axis = mkAcquire createMirrored deletePnt2d
  where
    createMirrored = [C.throwBlock| gp_Pnt2d* {
      return new gp_Pnt2d($(gp_Pnt2d* point)->Mirrored(*$(gp_Pnt2d* axis)));
    } |]

mirrorAboutAx2d :: Ptr Pnt2d -> Ptr Ax2d -> IO ()
mirrorAboutAx2d thePnt theAxis = [C.throwBlock| void {
  $(gp_Pnt2d* thePnt)->Mirror(*$(gp_Ax2d* theAxis));
} |]

mirroredAboutAx2d :: Ptr Pnt2d -> Ptr Ax2d -> Acquire (Ptr Pnt2d)
mirroredAboutAx2d point axis = mkAcquire createMirrored deletePnt2d
  where
    createMirrored = [C.throwBlock| gp_Pnt2d* {
      return new gp_Pnt2d($(gp_Pnt2d* point)->Mirrored(*$(gp_Ax2d* axis)));
    } |]

-- rotate/rotated

rotate :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> IO ()
rotate thePnt theAxis amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Pnt2d* thePnt)->Rotate(*$(gp_Pnt2d* theAxis), $(double cAmount));
  } |]

rotated :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Pnt2d)
rotated point axis amount = 
  let cAmount = realToFrac amount
      createRotated = [C.throwBlock| gp_Pnt2d* {
        return new gp_Pnt2d($(gp_Pnt2d* point)->Rotated(*$(gp_Pnt2d* axis), $(double cAmount)));
      } |]
  in mkAcquire createRotated deletePnt2d

-- scale/scaled

scale :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> IO ()
scale thePnt origin amount = 
  let cAmount = realToFrac amount
  in [C.throwBlock| void {
    $(gp_Pnt2d* thePnt)->Scale(*$(gp_Pnt2d* origin), $(double cAmount));
  } |]

scaled :: Ptr Pnt2d -> Ptr Pnt2d -> Double -> Acquire (Ptr Pnt2d)
scaled point origin amount = 
  let cAmount = realToFrac amount
      createScaled = [C.throwBlock| gp_Pnt2d* {
        return new gp_Pnt2d($(gp_Pnt2d* point)->Scaled(*$(gp_Pnt2d* origin), $(double cAmount)));
      } |]
  in mkAcquire createScaled deletePnt2d

-- transform/transformed

transform :: Ptr Pnt2d -> Ptr Trsf2d -> IO ()
transform thePnt trsf = [C.throwBlock| void {
  $(gp_Pnt2d* thePnt)->Transform(*$(gp_Trsf2d* trsf));
} |]

transformed :: Ptr Pnt2d -> Ptr Trsf2d -> Acquire (Ptr Pnt2d)
transformed point trsf = mkAcquire createTransformed deletePnt2d
  where
    createTransformed = [C.throwBlock| gp_Pnt2d* {
      return new gp_Pnt2d($(gp_Pnt2d* point)->Transformed(*$(gp_Trsf2d* trsf)));
    } |]

-- translate/translated

translate :: Ptr Pnt2d -> Ptr Vec2d -> IO ()
translate thePnt vec = [C.throwBlock| void {
  $(gp_Pnt2d* thePnt)->Translate(*$(gp_Vec2d* vec));
} |]

translated :: Ptr Pnt2d -> Ptr Vec2d -> Acquire (Ptr Pnt2d)
translated point vec = mkAcquire createTranslated deletePnt2d
  where
    createTranslated = [C.throwBlock| gp_Pnt2d* {
      return new gp_Pnt2d($(gp_Pnt2d* point)->Translated(*$(gp_Vec2d* vec)));
    } |]

-- translateRelative/translatedRelative

translateRelative :: Ptr Pnt2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO ()
translateRelative thePnt from to = [C.throwBlock| void {
  $(gp_Pnt2d* thePnt)->Translate(*$(gp_Pnt2d* from), *$(gp_Pnt2d* to));
} |]

translatedRelative :: Ptr Pnt2d -> Ptr Pnt2d -> Ptr Pnt2d -> Acquire (Ptr Pnt2d)
translatedRelative point from to = mkAcquire createTranslatedRelative deletePnt2d
  where
    createTranslatedRelative = [C.throwBlock| gp_Pnt2d* {
      return new gp_Pnt2d($(gp_Pnt2d* point)->Translated(*$(gp_Pnt2d* from), *$(gp_Pnt2d* to)));
    } |]

