{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.ShapeTool
( ShapeTool
, addShape
) where

import OpenCascade.XCAFDoc.Types (ShapeTool)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.Handle (Handle)
import OpenCascade.TDF.Types (Label)
import OpenCascade.TDF.Internal.Destructors (deleteLabel)
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C (CBool (..), CInt)
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_XCAFDoc_ShapeTool.h hs_XCAFDoc_ShapeTool_addShape" rawAddShape
    :: Ptr (Handle ShapeTool)
    -> Ptr TopoDS.Shape
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Label)

addShape :: Ptr (Handle ShapeTool) -> Ptr TopoDS.Shape -> Bool -> Bool -> Acquire (Ptr Label)
addShape tool shape makeAssembly makePrepare = mkAcquire (wrapException $ rawAddShape tool shape (boolToCBool makeAssembly) (boolToCBool makePrepare)) deleteLabel