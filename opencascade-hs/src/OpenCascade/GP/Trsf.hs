{-# LANGUAGE CApiFFI #-}
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
import OpenCascade.GP.Internal.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Coerce (coerce)
import Data.Acquire 


-- new

foreign import capi unsafe "hs_gp_Trsf.h hs_new_gp_Trsf" rawNew ::IO (Ptr Trsf)

new :: Acquire (Ptr Trsf)
new = mkAcquire rawNew deleteTrsf

foreign import capi unsafe "hs_gp_Trsf.h hs_new_gp_Trsf_fromTrsf2d" rawFromTrsf2d :: Ptr Trsf2d -> IO (Ptr Trsf)

fromTrsf2d :: Ptr Trsf2d -> Acquire (Ptr Trsf)
fromTrsf2d t = mkAcquire (rawFromTrsf2d t) deleteTrsf

-- mirror 

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetMirrorAboutPnt" setMirrorAboutPnt :: Ptr Trsf -> Ptr Pnt -> IO ()

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetMirrorAboutAx1" setMirrorAboutAx1 :: Ptr Trsf -> Ptr Ax1 -> IO ()

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetMirrorAboutAx2" setMirrorAboutAx2 :: Ptr Trsf -> Ptr Ax2 -> IO ()

-- rotate

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetRotationAboutAxisAngle" rawSetRotationAboutAxisAngle :: Ptr Trsf -> Ptr Ax1 -> CDouble -> IO ()

setRotationAboutAxisAngle :: Ptr Trsf -> Ptr Ax1 -> Double -> IO ()
setRotationAboutAxisAngle = coerce rawSetRotationAboutAxisAngle

-- scale

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetScale" rawSetScale :: Ptr Trsf -> Ptr Pnt -> CDouble -> IO ()

setScale :: Ptr Trsf -> Ptr Pnt -> Double -> IO ()
setScale = coerce rawSetScale

-- translation

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetTranslation" setTranslation :: Ptr Trsf -> Ptr Vec -> IO ()

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetTranslationPart" setTranslationPart :: Ptr Trsf -> Ptr Vec -> IO ()

-- setDisplacement

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetDisplacement" setDisplacement :: Ptr Trsf -> Ptr Ax3 -> Ptr Ax3 -> IO ()

-- scaleFactor

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetScaleFactor" rawSetScaleFactor :: Ptr Trsf -> CDouble -> IO ()

setScaleFactor :: Ptr Trsf -> Double -> IO ()
setScaleFactor = coerce rawSetScaleFactor

-- setValues

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_SetValues" rawSetValues :: Ptr Trsf -> CDouble -> CDouble-> CDouble-> CDouble-> CDouble-> CDouble-> CDouble-> CDouble-> CDouble-> CDouble-> CDouble-> CDouble -> IO ()

setValues :: Ptr Trsf -> Double -> Double-> Double-> Double-> Double-> Double-> Double-> Double-> Double-> Double-> Double-> Double -> IO ()
setValues = coerce rawSetValues

-- tests 

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_IsNegative" rawIsNegative :: Ptr Trsf -> IO CBool

isNegative :: Ptr Trsf -> IO Bool
isNegative = fmap (/= 0) . rawIsNegative

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_ScaleFactor" rawScaleFactor :: Ptr Trsf -> IO CDouble

scaleFactor :: Ptr Trsf -> IO Double
scaleFactor = coerce rawScaleFactor

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_Value" rawValue :: Ptr Trsf -> CInt -> CInt -> IO CDouble

value :: Ptr Trsf -> Int -> Int -> IO Double
value t row col = coerce $ rawValue t (fromIntegral row) (fromIntegral col)

-- invert/inverted

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_Invert" invert :: Ptr Trsf-> IO ()

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_Inverted" rawInverted :: Ptr Trsf-> IO (Ptr Trsf)

inverted :: Ptr Trsf -> Acquire (Ptr Trsf)
inverted t = mkAcquire (rawInverted t) deleteTrsf

-- multiply/multiplied

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_Multiply" multiply :: Ptr Trsf -> Ptr Trsf -> IO ()

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_Multiplied" rawMultiplied :: Ptr Trsf -> Ptr Trsf -> IO (Ptr Trsf)

multiplied :: Ptr Trsf -> Ptr Trsf -> Acquire (Ptr Trsf)
multiplied a b = mkAcquire (rawMultiplied a b) deleteTrsf

-- PreMultiply

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_PreMultiply" preMultiply :: Ptr Trsf -> Ptr Trsf -> IO ()

-- power/powered

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_Power" rawPower :: Ptr Trsf -> CInt -> IO ()

power :: Ptr Trsf -> Int -> IO ()
power trsf times = rawPower trsf (fromIntegral times)

foreign import capi unsafe "hs_gp_Trsf.h hs_gp_Trsf_Powered" rawPowered :: Ptr Trsf -> CInt -> IO (Ptr Trsf)

powered :: Ptr Trsf -> Int -> Acquire (Ptr Trsf)
powered trsf times = mkAcquire (rawPowered trsf (fromIntegral times)) deleteTrsf
