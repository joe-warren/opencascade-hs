{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.DocumentTool
( shapeTool
) where

import OpenCascade.XCAFDoc.Types (ShapeTool)
import OpenCascade.XCAFDoc.Internal.Destructors (deleteShapeToolHandle)
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