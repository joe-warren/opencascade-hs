{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.VisMaterialPBR
( VisMaterialPBR
, new
, setBaseColor
, setMetallic
, setRoughness
, setEmissiveFactor
, setRefractionIndex
) where

import OpenCascade.XCAFDoc.Types (VisMaterialPBR)
import OpenCascade.XCAFDoc.Internal.Destructors (deleteVisMaterialPBR)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..))
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_XCAFDoc_VisMaterialPBR.h hs_new_XCAFDoc_VisMaterialPBR" rawNew :: IO (Ptr VisMaterialPBR)

new :: Acquire (Ptr VisMaterialPBR)
new = mkAcquire rawNew deleteVisMaterialPBR

foreign import capi unsafe "hs_XCAFDoc_VisMaterialPBR.h hs_XCAFDoc_VisMaterialPBR_setBaseColor" rawSetBaseColor :: Ptr VisMaterialPBR -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

setBaseColor :: Ptr VisMaterialPBR -> Double -> Double -> Double -> Double -> IO ()
setBaseColor = coerce rawSetBaseColor

foreign import capi unsafe "hs_XCAFDoc_VisMaterialPBR.h hs_XCAFDoc_VisMaterialPBR_setMetallic" rawSetMetallic :: Ptr VisMaterialPBR -> CDouble -> IO ()

setMetallic :: Ptr VisMaterialPBR -> Double -> IO ()
setMetallic = coerce rawSetMetallic

foreign import capi unsafe "hs_XCAFDoc_VisMaterialPBR.h hs_XCAFDoc_VisMaterialPBR_setRoughness" rawSetRoughness :: Ptr VisMaterialPBR -> CDouble -> IO ()

setRoughness :: Ptr VisMaterialPBR -> Double -> IO ()
setRoughness = coerce rawSetRoughness

foreign import capi unsafe "hs_XCAFDoc_VisMaterialPBR.h hs_XCAFDoc_VisMaterialPBR_setEmissiveFactor" rawSetEmissiveFactor :: Ptr VisMaterialPBR -> CDouble -> CDouble -> CDouble -> IO ()

setEmissiveFactor :: Ptr VisMaterialPBR -> Double -> Double -> Double -> IO ()
setEmissiveFactor = coerce rawSetEmissiveFactor

foreign import capi unsafe "hs_XCAFDoc_VisMaterialPBR.h hs_XCAFDoc_VisMaterialPBR_setRefractionIndex" rawSetRefractionIndex :: Ptr VisMaterialPBR -> CDouble -> IO ()

setRefractionIndex :: Ptr VisMaterialPBR -> Double -> IO ()
setRefractionIndex = coerce rawSetRefractionIndex
