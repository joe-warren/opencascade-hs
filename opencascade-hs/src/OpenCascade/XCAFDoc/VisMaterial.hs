{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.VisMaterial
( VisMaterial
, new
, setPbrMaterial
, setAlphaMode
) where

import OpenCascade.XCAFDoc.Types (VisMaterial, VisMaterialPBR)
import OpenCascade.XCAFDoc.Internal.Destructors (deleteVisMaterialHandle)
import OpenCascade.Graphic3D.AlphaMode (AlphaMode)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..), CInt (..))
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_new_XCAFDoc_VisMaterial" rawNew :: IO (Ptr (Handle VisMaterial))

new :: Acquire (Ptr (Handle VisMaterial))
new = mkAcquire rawNew deleteVisMaterialHandle

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_XCAFDoc_VisMaterial_setPbrMaterial" rawSetPbrMaterial
    :: Ptr (Handle VisMaterial)
    -> Ptr VisMaterialPBR
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setPbrMaterial :: Ptr (Handle VisMaterial) -> Ptr VisMaterialPBR -> IO ()
setPbrMaterial material pbr = wrapException $ rawSetPbrMaterial material pbr

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_XCAFDoc_VisMaterial_setAlphaMode" rawSetAlphaMode
    :: Ptr (Handle VisMaterial)
    -> CInt
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setAlphaMode :: Ptr (Handle VisMaterial) -> AlphaMode -> Double -> IO ()
setAlphaMode material mode cutOff = wrapException $ rawSetAlphaMode material (fromIntegral . fromEnum $ mode) (realToFrac cutOff)
