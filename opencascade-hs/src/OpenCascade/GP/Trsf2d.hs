module OpenCascade.GP.Trsf2d
( Trsf2d
, new
, fromTrsf
, setMirrorAboutPnt2d
, setMirrorAboutAx2d
, setRotation
, setScale
, setTransformation
, setTransformationRelative
, setTranslation
, setScaleFactor
, setTranslationPart
, setTranslationRelative
, setValues
, isNegative
, scaleFactor
, value
, invert
, inverted
, multiply
, multiplied
, preMultiply
, power
, powered
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

C.include "<gp_Trsf2d.hxx>"
C.include "<gp_Trsf.hxx>"
C.include "<gp_Pnt2d.hxx>"
C.include "<gp_Ax2d.hxx>"
C.include "<gp_Vec2d.hxx>" 


-- new

new :: Acquire (Ptr Trsf2d)
new = mkAcquire createTrsf2d deleteTrsf2d
  where
    createTrsf2d = [C.throwBlock| gp_Trsf2d* {
      return new gp_Trsf2d();
    } |]

fromTrsf :: Ptr Trsf -> Acquire (Ptr Trsf2d)
fromTrsf t = mkAcquire createTrsf2d deleteTrsf2d
  where
    createTrsf2d = [C.throwBlock| gp_Trsf2d* {
      return new gp_Trsf2d(*$(gp_Trsf* t));
    } |]

-- mirror 

setMirrorAboutPnt2d :: Ptr Trsf2d -> Ptr Pnt2d -> IO ()
setMirrorAboutPnt2d trsf pnt = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->SetMirror(*$(gp_Pnt2d* pnt));
} |]

setMirrorAboutAx2d :: Ptr Trsf2d -> Ptr Ax2d -> IO ()
setMirrorAboutAx2d trsf axis = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->SetMirror(*$(gp_Ax2d* axis));
} |]

-- rotate

setRotation :: Ptr Trsf2d -> Ptr Pnt2d -> Double -> IO ()
setRotation trsf pnt angle = 
  let cAngle = realToFrac angle
  in [C.throwBlock| void {
    $(gp_Trsf2d* trsf)->SetRotation(*$(gp_Pnt2d* pnt), $(double cAngle));
  } |]

-- scale

setScale :: Ptr Trsf2d -> Ptr Pnt2d -> Double -> IO ()
setScale trsf pnt scale = 
  let cScale = realToFrac scale
  in [C.throwBlock| void {
    $(gp_Trsf2d* trsf)->SetScale(*$(gp_Pnt2d* pnt), $(double cScale));
  } |]

-- transformation

setTransformation :: Ptr Trsf2d -> Ptr Ax2d -> IO ()
setTransformation trsf axis = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->SetTransformation(*$(gp_Ax2d* axis));
} |]

setTransformationRelative :: Ptr Trsf2d -> Ptr Ax2d -> Ptr Ax2d -> IO ()
setTransformationRelative trsf fromAx toAx = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->SetTransformation(*$(gp_Ax2d* fromAx), *$(gp_Ax2d* toAx));
} |]

-- translation

setTranslation :: Ptr Trsf2d -> Ptr Vec2d -> IO ()
setTranslation trsf vec = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->SetTranslation(*$(gp_Vec2d* vec));
} |]

setTranslationPart :: Ptr Trsf2d -> Ptr Vec2d -> IO ()
setTranslationPart trsf vec = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->SetTranslationPart(*$(gp_Vec2d* vec));
} |]

setTranslationRelative :: Ptr Trsf2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO ()
setTranslationRelative trsf fromPnt toPnt = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->SetTranslation(*$(gp_Pnt2d* fromPnt), *$(gp_Pnt2d* toPnt));
} |]

-- scaleFactor

setScaleFactor :: Ptr Trsf2d -> Double -> IO ()
setScaleFactor trsf factor = 
  let cFactor = realToFrac factor
  in [C.throwBlock| void {
    $(gp_Trsf2d* trsf)->SetScaleFactor($(double cFactor));
  } |]

-- setValues

setValues :: Ptr Trsf2d -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
setValues trsf a11 a12 a13 a21 a22 a23 = 
  let ca11 = realToFrac a11
      ca12 = realToFrac a12
      ca13 = realToFrac a13
      ca21 = realToFrac a21
      ca22 = realToFrac a22
      ca23 = realToFrac a23
  in [C.throwBlock| void {
    $(gp_Trsf2d* trsf)->SetValues($(double ca11), $(double ca12), $(double ca13),
                                  $(double ca21), $(double ca22), $(double ca23));
  } |]

-- tests 

isNegative :: Ptr Trsf2d -> IO Bool
isNegative trsf = do
  result <- [C.throwBlock| bool {
    return $(gp_Trsf2d* trsf)->IsNegative();
  } |]
  return (cBoolToBool result)

scaleFactor :: Ptr Trsf2d -> IO Double
scaleFactor trsf = do
  result <- [C.throwBlock| double {
    return $(gp_Trsf2d* trsf)->ScaleFactor();
  } |]
  return (realToFrac result)

value :: Ptr Trsf2d -> Int -> Int -> IO Double
value trsf row col = do
  let cRow = fromIntegral row
      cCol = fromIntegral col
  result <- [C.throwBlock| double {
    return $(gp_Trsf2d* trsf)->Value($(int cRow), $(int cCol));
  } |]
  return (realToFrac result)

-- invert/inverted

invert :: Ptr Trsf2d -> IO ()
invert trsf = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->Invert();
} |]

inverted :: Ptr Trsf2d -> Acquire (Ptr Trsf2d)
inverted trsf = mkAcquire createInverted deleteTrsf2d
  where
    createInverted = [C.throwBlock| gp_Trsf2d* {
      return new gp_Trsf2d($(gp_Trsf2d* trsf)->Inverted());
    } |]

-- multiply/multiplied

multiply :: Ptr Trsf2d -> Ptr Trsf2d -> IO ()
multiply trsf other = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->Multiply(*$(gp_Trsf2d* other));
} |]

multiplied :: Ptr Trsf2d -> Ptr Trsf2d -> Acquire (Ptr Trsf2d)
multiplied a b = mkAcquire createMultiplied deleteTrsf2d
  where
    createMultiplied = [C.throwBlock| gp_Trsf2d* {
      return new gp_Trsf2d($(gp_Trsf2d* a)->Multiplied(*$(gp_Trsf2d* b)));
    } |]

-- PreMultiply

preMultiply :: Ptr Trsf2d -> Ptr Trsf2d -> IO ()
preMultiply trsf other = [C.throwBlock| void {
  $(gp_Trsf2d* trsf)->PreMultiply(*$(gp_Trsf2d* other));
} |]

-- power/powered

power :: Ptr Trsf2d -> Int -> IO ()
power trsf times = 
  let cTimes = fromIntegral times
  in [C.throwBlock| void {
    $(gp_Trsf2d* trsf)->Power($(int cTimes));
  } |]

powered :: Ptr Trsf2d -> Int -> Acquire (Ptr Trsf2d)
powered trsf times = 
  let cTimes = fromIntegral times
      createPowered = [C.throwBlock| gp_Trsf2d* {
        return new gp_Trsf2d($(gp_Trsf2d* trsf)->Powered($(int cTimes)));
      } |]
  in mkAcquire createPowered deleteTrsf2d
