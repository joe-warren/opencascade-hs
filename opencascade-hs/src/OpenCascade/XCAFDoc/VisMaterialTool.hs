{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.VisMaterialTool
( VisMaterialTool
, addMaterial
, setShapeMaterial
, setShapeMaterialFromShape
) where

import OpenCascade.XCAFDoc.Types (VisMaterialTool, VisMaterial)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.Handle (Handle)
import OpenCascade.TDF.Types (Label)
import OpenCascade.TDF.Internal.Destructors (deleteLabel)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C (CBool (..), CInt)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_XCAFDoc_VisMaterialTool.h hs_XCAFDoc_VisMaterialTool_addMaterial" rawAddMaterial
    :: Ptr (Handle VisMaterialTool)
    -> Ptr (Handle VisMaterial)
    -> CString
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Label)

addMaterial :: Ptr (Handle VisMaterialTool) -> Ptr (Handle VisMaterial) -> String -> Acquire (Ptr Label)
addMaterial tool material name = mkAcquire (withCString name $ \str -> wrapException $ rawAddMaterial tool material str) deleteLabel

foreign import capi unsafe "hs_XCAFDoc_VisMaterialTool.h hs_XCAFDoc_VisMaterialTool_setShapeMaterial" rawSetShapeMaterial
    :: Ptr (Handle VisMaterialTool)
    -> Ptr Label
    -> Ptr Label
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setShapeMaterial :: Ptr (Handle VisMaterialTool) -> Ptr Label -> Ptr Label -> IO ()
setShapeMaterial tool shapeLabel materialLabel = wrapException $ rawSetShapeMaterial tool shapeLabel materialLabel

foreign import capi unsafe "hs_XCAFDoc_VisMaterialTool.h hs_XCAFDoc_VisMaterialTool_setShapeMaterialFromShape" rawSetShapeMaterialFromShape
    :: Ptr (Handle VisMaterialTool)
    -> Ptr TopoDS.Shape
    -> Ptr Label
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

setShapeMaterialFromShape :: Ptr (Handle VisMaterialTool) -> Ptr TopoDS.Shape -> Ptr Label -> IO Bool
setShapeMaterialFromShape tool shape materialLabel = cBoolToBool <$> wrapException (rawSetShapeMaterialFromShape tool shape materialLabel)
