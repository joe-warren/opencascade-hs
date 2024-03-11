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
import Data.Acquire (Acquire, mkAcquire)
import Foreign.C (CBool (..))
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_XCAFDoc_ShapeTool.h hs_XCAFDoc_ShapeTool_addShape" rawAddShape :: Ptr (Handle ShapeTool) -> Ptr TopoDS.Shape -> CBool -> CBool -> IO (Ptr Label)

addShape :: Ptr (Handle ShapeTool) -> Ptr TopoDS.Shape -> Bool -> Bool -> Acquire (Ptr Label)
addShape tool shape makeAssembly makePrepare = mkAcquire (rawAddShape tool shape (boolToCBool makeAssembly) (boolToCBool makePrepare)) deleteLabel