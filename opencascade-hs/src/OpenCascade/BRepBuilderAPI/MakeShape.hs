{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeShape
( MakeShape
, shape
) where
import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Destructors
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Inheritance (upcast)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_shape" rawShape :: Ptr MakeShape -> IO (Ptr TopoDS.Shape)

shape :: Ptr MakeShape -> Acquire (Ptr TopoDS.Shape)
shape builder = mkAcquire (rawShape builder) (deleteShape)  