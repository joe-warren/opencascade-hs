{-# LANGUAGE CApiFFI #-}
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
import OpenCascade.GP.Internal.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 


-- new

foreign import capi unsafe "hs_gp_Trsf2d.h hs_new_gp_Trsf2d" rawNew ::IO (Ptr Trsf2d)

new :: Acquire (Ptr Trsf2d)
new = mkAcquire rawNew deleteTrsf2d

foreign import capi unsafe "hs_gp_Trsf2d.h hs_new_gp_Trsf2d_fromTrsf" rawFromTrsf :: Ptr Trsf -> IO (Ptr Trsf2d)

fromTrsf :: Ptr Trsf -> Acquire (Ptr Trsf2d)
fromTrsf t = mkAcquire (rawFromTrsf t) deleteTrsf2d

-- mirror 

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetMirrorAboutPnt2d" setMirrorAboutPnt2d :: Ptr Trsf2d -> Ptr Pnt2d -> IO ()

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetMirrorAboutAx2d" setMirrorAboutAx2d :: Ptr Trsf2d -> Ptr Ax2d -> IO ()

-- rotate

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetRotation" rawSetRotation :: Ptr Trsf2d -> Ptr Pnt2d -> CDouble -> IO ()

setRotation :: Ptr Trsf2d -> Ptr Pnt2d -> Double -> IO ()
setRotation = coerce rawSetRotation

-- scale

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetScale" rawSetScale :: Ptr Trsf2d -> Ptr Pnt2d -> CDouble -> IO ()

setScale :: Ptr Trsf2d -> Ptr Pnt2d -> Double -> IO ()
setScale = coerce rawSetScale

-- transformation

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetTransformation" setTransformation :: Ptr Trsf2d -> Ptr Ax2d -> IO ()

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetTransformationRelative" setTransformationRelative :: Ptr Trsf2d -> Ptr Ax2d -> Ptr Ax2d -> IO ()

-- translation

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetTranslation" setTranslation :: Ptr Trsf2d -> Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetTranslationPart" setTranslationPart :: Ptr Trsf2d -> Ptr Vec2d -> IO ()

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetTranslationRelative" setTranslationRelative :: Ptr Trsf2d -> Ptr Pnt2d -> Ptr Pnt2d -> IO ()

-- scaleFactor

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetScaleFactor" rawSetScaleFactor :: Ptr Trsf2d -> CDouble -> IO ()

setScaleFactor :: Ptr Trsf2d -> Double -> IO ()
setScaleFactor = coerce rawSetScaleFactor

-- setValues

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_SetValues" rawSetValues :: Ptr Trsf2d -> CDouble -> CDouble-> CDouble-> CDouble-> CDouble-> CDouble-> IO ()

setValues :: Ptr Trsf2d -> Double -> Double -> Double -> Double-> Double -> Double -> IO ()
setValues = coerce rawSetValues

-- tests 

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_IsNegative" rawIsNegative :: Ptr Trsf2d -> IO CBool

isNegative :: Ptr Trsf2d -> IO Bool
isNegative = fmap (/= 0) . rawIsNegative

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_ScaleFactor" rawScaleFactor :: Ptr Trsf2d -> IO CDouble

scaleFactor :: Ptr Trsf2d -> IO Double
scaleFactor = coerce rawScaleFactor

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_Value" rawValue :: Ptr Trsf2d -> CInt -> CInt -> IO CDouble

value :: Ptr Trsf2d -> Int -> Int -> IO Double
value t row col = coerce $ rawValue t (fromIntegral row) (fromIntegral col)

-- invert/inverted

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_Invert" invert :: Ptr Trsf2d-> IO ()

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_Inverted" rawInverted :: Ptr Trsf2d-> IO (Ptr Trsf2d)

inverted :: Ptr Trsf2d -> Acquire (Ptr Trsf2d)
inverted t = mkAcquire (rawInverted t) deleteTrsf2d

-- multiply/multiplied

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_Multiply" multiply :: Ptr Trsf2d -> Ptr Trsf2d -> IO ()

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_Multiplied" rawMultiplied :: Ptr Trsf2d -> Ptr Trsf2d -> IO (Ptr Trsf2d)

multiplied :: Ptr Trsf2d -> Ptr Trsf2d -> Acquire (Ptr Trsf2d)
multiplied a b = mkAcquire (rawMultiplied a b) deleteTrsf2d

-- PreMultiply

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_PreMultiply" preMultiply :: Ptr Trsf2d -> Ptr Trsf2d -> IO ()

-- power/powered

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_Power" rawPower :: Ptr Trsf2d -> CInt -> IO ()

power :: Ptr Trsf2d -> Int -> IO ()
power trsf times = rawPower trsf (fromIntegral times)

foreign import capi unsafe "hs_gp_Trsf2d.h hs_gp_Trsf2d_Powered" rawPowered :: Ptr Trsf2d -> CInt -> IO (Ptr Trsf2d)

powered :: Ptr Trsf2d -> Int -> Acquire (Ptr Trsf2d)
powered trsf times = mkAcquire (rawPowered trsf (fromIntegral times)) deleteTrsf2d
