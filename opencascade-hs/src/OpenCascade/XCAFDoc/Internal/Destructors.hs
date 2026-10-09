{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.Internal.Destructors
( deleteShapeToolHandle
, deleteColorToolHandle
, deleteVisMaterialToolHandle
, deleteVisMaterialHandle
, deleteVisMaterialPBR
) where

import OpenCascade.XCAFDoc.Types
import OpenCascade.Handle
import Foreign.Ptr

foreign import capi unsafe "hs_XCAFDoc_ShapeTool.h hs_delete_XCAFDoc_ShapeTool" deleteShapeToolHandle :: Ptr (Handle ShapeTool) -> IO ()

foreign import capi unsafe "hs_XCAFDoc_ColorTool.h hs_delete_XCAFDoc_ColorTool" deleteColorToolHandle :: Ptr (Handle ColorTool) -> IO ()

foreign import capi unsafe "hs_XCAFDoc_VisMaterialTool.h hs_delete_XCAFDoc_VisMaterialTool" deleteVisMaterialToolHandle :: Ptr (Handle VisMaterialTool) -> IO ()

foreign import capi unsafe "hs_XCAFDoc_VisMaterial.h hs_delete_XCAFDoc_VisMaterial" deleteVisMaterialHandle :: Ptr (Handle VisMaterial) -> IO ()

foreign import capi unsafe "hs_XCAFDoc_VisMaterialPBR.h hs_delete_XCAFDoc_VisMaterialPBR" deleteVisMaterialPBR :: Ptr VisMaterialPBR -> IO ()
