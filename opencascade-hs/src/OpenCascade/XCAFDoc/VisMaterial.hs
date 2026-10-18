{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.VisMaterial
( VisMaterial
, new
, setPbrMaterial
, setAlphaMode
, setFaceCulling
, setRawName
) where

import OpenCascade.XCAFDoc.Types (VisMaterial, VisMaterialPBR)
import OpenCascade.XCAFDoc.Internal.Destructors (deleteVisMaterialHandle)
import OpenCascade.Graphic3D.AlphaMode (AlphaMode)
import OpenCascade.Graphic3D.TypeOfBackfacingModel (TypeOfBackfacingModel)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CDouble (..), CInt (..))
import Foreign.C.String (CString, withCString)
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

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_XCAFDoc_VisMaterial_setFaceCulling" rawSetFaceCulling
    :: Ptr (Handle VisMaterial)
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setFaceCulling :: Ptr (Handle VisMaterial) -> TypeOfBackfacingModel -> IO ()
setFaceCulling material faceCulling = wrapException $ rawSetFaceCulling material (fromIntegral . fromEnum $ faceCulling)

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_XCAFDoc_VisMaterial_setRawName" rawSetRawName
    :: Ptr (Handle VisMaterial)
    -> CString
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setRawName :: Ptr (Handle VisMaterial) -> String -> IO ()
setRawName material name = withCString name $ \str -> wrapException $ rawSetRawName material str
