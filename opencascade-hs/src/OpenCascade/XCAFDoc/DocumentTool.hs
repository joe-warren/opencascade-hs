{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.DocumentTool
( shapeTool
, colorTool
) where

import OpenCascade.XCAFDoc.Types (ShapeTool, ColorTool)
import OpenCascade.XCAFDoc.Internal.Destructors (deleteShapeToolHandle, deleteColorToolHandle)
import OpenCascade.TDF.Types (Label)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_XCAFDoc_DocumentTool.h hs_XCAFDoc_DocumentTool_shapeTool" rawShapeTool
    :: Ptr Label
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle ShapeTool))

shapeTool :: Ptr Label -> Acquire (Ptr (Handle ShapeTool))
shapeTool label = mkAcquire (wrapException $ rawShapeTool label) deleteShapeToolHandle

foreign import capi unsafe "hs_XCAFDoc_DocumentTool.h hs_XCAFDoc_DocumentTool_colorTool" rawColorTool
    :: Ptr Label
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr (Handle ColorTool))

colorTool :: Ptr Label -> Acquire (Ptr (Handle ColorTool))
colorTool label = mkAcquire (wrapException $ rawColorTool label) deleteColorToolHandle
