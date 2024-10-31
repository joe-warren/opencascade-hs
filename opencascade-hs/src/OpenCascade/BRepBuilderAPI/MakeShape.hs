{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeShape
( MakeShape
, shape
, build
) where
import OpenCascade.BRepBuilderAPI.Types
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr
import Data.Acquire 

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_shape" rawShape :: Ptr MakeShape -> IO (Ptr TopoDS.Shape)

shape :: Ptr MakeShape -> Acquire (Ptr TopoDS.Shape)
shape builder = mkAcquire (rawShape builder) (deleteShape)  

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_build" build :: Ptr MakeShape -> IO ()