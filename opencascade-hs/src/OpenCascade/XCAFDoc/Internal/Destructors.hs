{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.Internal.Destructors 
( deleteShapeToolHandle 
) where

import OpenCascade.XCAFDoc.Types
import OpenCascade.Handle
import Foreign.Ptr 

foreign import capi unsafe "hs_XCAFDoc_ShapeTool.h hs_delete_XCAFDoc_ShapeTool" deleteShapeToolHandle :: Ptr (Handle ShapeTool) -> IO ()