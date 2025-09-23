module OpenCascade.GP.Trsf
( Trsf
, new
, fromTrsf2d
, setMirrorAboutPnt
, setMirrorAboutAx1
, setMirrorAboutAx2
, setRotationAboutAxisAngle
, setScale
, setTranslation
, setDisplacement
, setScaleFactor
, setTranslationPart
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

C.include "<gp_Trsf.hxx>"
C.include "<gp_Trsf2d.hxx>"
C.include "<gp_Pnt.hxx>"
C.include "<gp_Ax1.hxx>"
C.include "<gp_Ax2.hxx>"
C.include "<gp_Vec.hxx>" 


-- new

new :: Acquire (Ptr Trsf)
new = mkAcquire createTrsf deleteTrsf
  where
    createTrsf = [C.throwBlock| gp_Trsf* {
      return new gp_Trsf();
    } |]

fromTrsf2d :: Ptr Trsf2d -> Acquire (Ptr Trsf)
fromTrsf2d t = mkAcquire createTrsf deleteTrsf
  where
    createTrsf = [C.throwBlock| gp_Trsf* {
      return new gp_Trsf(*$(gp_Trsf2d* t));
    } |]

-- mirror 

setMirrorAboutPnt :: Ptr Trsf -> Ptr Pnt -> IO ()
setMirrorAboutPnt trsf pnt = [C.throwBlock| void {
  $(gp_Trsf* trsf)->SetMirror(*$(gp_Pnt* pnt));
} |]

setMirrorAboutAx1 :: Ptr Trsf -> Ptr Ax1 -> IO ()
setMirrorAboutAx1 trsf axis = [C.throwBlock| void {
  $(gp_Trsf* trsf)->SetMirror(*$(gp_Ax1* axis));
} |]

setMirrorAboutAx2 :: Ptr Trsf -> Ptr Ax2 -> IO ()
setMirrorAboutAx2 trsf axis = [C.throwBlock| void {
  $(gp_Trsf* trsf)->SetMirror(*$(gp_Ax2* axis));
} |]

-- rotate

setRotationAboutAxisAngle :: Ptr Trsf -> Ptr Ax1 -> Double -> IO ()
setRotationAboutAxisAngle trsf axis angle = 
  let cAngle = realToFrac angle
  in [C.throwBlock| void {
    $(gp_Trsf* trsf)->SetRotation(*$(gp_Ax1* axis), $(double cAngle));
  } |]

-- scale

setScale :: Ptr Trsf -> Ptr Pnt -> Double -> IO ()
setScale trsf pnt scale = 
  let cScale = realToFrac scale
  in [C.throwBlock| void {
    $(gp_Trsf* trsf)->SetScale(*$(gp_Pnt* pnt), $(double cScale));
  } |]

-- translation

setTranslation :: Ptr Trsf -> Ptr Vec -> IO ()
setTranslation trsf vec = [C.throwBlock| void {
  $(gp_Trsf* trsf)->SetTranslation(*$(gp_Vec* vec));
} |]

setTranslationPart :: Ptr Trsf -> Ptr Vec -> IO ()
setTranslationPart trsf vec = [C.throwBlock| void {
  $(gp_Trsf* trsf)->SetTranslationPart(*$(gp_Vec* vec));
} |]

-- setDisplacement

setDisplacement :: Ptr Trsf -> Ptr Ax3 -> Ptr Ax3 -> IO ()
setDisplacement trsf fromAx toAx = [C.throwBlock| void {
  $(gp_Trsf* trsf)->SetDisplacement(*$(gp_Ax3* fromAx), *$(gp_Ax3* toAx));
} |]

-- scaleFactor

setScaleFactor :: Ptr Trsf -> Double -> IO ()
setScaleFactor trsf factor = 
  let cFactor = realToFrac factor
  in [C.throwBlock| void {
    $(gp_Trsf* trsf)->SetScaleFactor($(double cFactor));
  } |]

-- setValues

setValues :: Ptr Trsf -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
setValues trsf a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 = 
  let ca11 = realToFrac a11
      ca12 = realToFrac a12
      ca13 = realToFrac a13
      ca14 = realToFrac a14
      ca21 = realToFrac a21
      ca22 = realToFrac a22
      ca23 = realToFrac a23
      ca24 = realToFrac a24
      ca31 = realToFrac a31
      ca32 = realToFrac a32
      ca33 = realToFrac a33
      ca34 = realToFrac a34
  in [C.throwBlock| void {
    $(gp_Trsf* trsf)->SetValues($(double ca11), $(double ca12), $(double ca13), $(double ca14), 
                                $(double ca21), $(double ca22), $(double ca23), $(double ca24), 
                                $(double ca31), $(double ca32), $(double ca33), $(double ca34));
  } |]

-- tests 

isNegative :: Ptr Trsf -> IO Bool
isNegative trsf = do
  result <- [C.throwBlock| bool {
    return $(gp_Trsf* trsf)->IsNegative();
  } |]
  return (cBoolToBool result)

scaleFactor :: Ptr Trsf -> IO Double
scaleFactor trsf = do
  result <- [C.throwBlock| double {
    return $(gp_Trsf* trsf)->ScaleFactor();
  } |]
  return (realToFrac result)

value :: Ptr Trsf -> Int -> Int -> IO Double
value trsf row col = do
  let cRow = fromIntegral row
      cCol = fromIntegral col
  result <- [C.throwBlock| double {
    return $(gp_Trsf* trsf)->Value($(int cRow), $(int cCol));
  } |]
  return (realToFrac result)

-- invert/inverted

invert :: Ptr Trsf -> IO ()
invert trsf = [C.throwBlock| void {
  $(gp_Trsf* trsf)->Invert();
} |]

inverted :: Ptr Trsf -> Acquire (Ptr Trsf)
inverted trsf = mkAcquire createInverted deleteTrsf
  where
    createInverted = [C.throwBlock| gp_Trsf* {
      return new gp_Trsf($(gp_Trsf* trsf)->Inverted());
    } |]

-- multiply/multiplied

multiply :: Ptr Trsf -> Ptr Trsf -> IO ()
multiply trsf other = [C.throwBlock| void {
  $(gp_Trsf* trsf)->Multiply(*$(gp_Trsf* other));
} |]

multiplied :: Ptr Trsf -> Ptr Trsf -> Acquire (Ptr Trsf)
multiplied a b = mkAcquire createMultiplied deleteTrsf
  where
    createMultiplied = [C.throwBlock| gp_Trsf* {
      return new gp_Trsf($(gp_Trsf* a)->Multiplied(*$(gp_Trsf* b)));
    } |]

-- PreMultiply

preMultiply :: Ptr Trsf -> Ptr Trsf -> IO ()
preMultiply trsf other = [C.throwBlock| void {
  $(gp_Trsf* trsf)->PreMultiply(*$(gp_Trsf* other));
} |]

-- power/powered

power :: Ptr Trsf -> Int -> IO ()
power trsf times = 
  let cTimes = fromIntegral times
  in [C.throwBlock| void {
    $(gp_Trsf* trsf)->Power($(int cTimes));
  } |]

powered :: Ptr Trsf -> Int -> Acquire (Ptr Trsf)
powered trsf times = 
  let cTimes = fromIntegral times
      createPowered = [C.throwBlock| gp_Trsf* {
        return new gp_Trsf($(gp_Trsf* trsf)->Powered($(int cTimes)));
      } |]
  in mkAcquire createPowered deleteTrsf
