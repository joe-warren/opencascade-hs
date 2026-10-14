{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.VisMaterial
( VisMaterial
, new
, setPbrMaterial
, setAlphaMode
, setDoubleSided
, setRawName
) where

import OpenCascade.XCAFDoc.Types (VisMaterial, VisMaterialPBR)
import OpenCascade.XCAFDoc.Internal.Destructors (deleteVisMaterialHandle)
import OpenCascade.Graphic3D.AlphaMode (AlphaMode)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..), CInt (..), CBool (..))
import Foreign.C.String (CString, withCString)
import OpenCascade.Internal.Bool (boolToCBool)
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

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_XCAFDoc_VisMaterial_setDoubleSided" rawSetDoubleSided
    :: Ptr (Handle VisMaterial)
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setDoubleSided :: Ptr (Handle VisMaterial) -> Bool -> IO ()
setDoubleSided material isDoubleSided = wrapException $ rawSetDoubleSided material (boolToCBool isDoubleSided)

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_XCAFDoc_VisMaterial_setRawName" rawSetRawName
    :: Ptr (Handle VisMaterial)
    -> CString
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setRawName :: Ptr (Handle VisMaterial) -> String -> IO ()
setRawName material name = withCString name $ \str -> wrapException $ rawSetRawName material str
