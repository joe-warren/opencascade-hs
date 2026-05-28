{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeShape
( MakeShape
, shape
, build
) where
import OpenCascade.BRepBuilderAPI.Types
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr
import Data.Acquire 
import Foreign.C (CInt)

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_shape" rawShape
    :: Ptr MakeShape 
    -> Ptr CInt 
    -> Ptr (Ptr ()) 
    -> IO (Ptr TopoDS.Shape)

shape :: Ptr MakeShape -> Acquire (Ptr TopoDS.Shape)
shape builder = mkAcquire (wrapException $ rawShape builder) (deleteShape)  

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_build" rawBuild
    :: Ptr MakeShape 
    -> Ptr CInt 
    -> Ptr (Ptr ()) 
    -> IO ()

build :: Ptr MakeShape -> IO ()
build builder = wrapException $ rawBuild builder