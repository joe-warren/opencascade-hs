{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeShape
( MakeShape
, shape
, build
, modified
, generated
, isDeleted
) where
import OpenCascade.BRepBuilderAPI.Types
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.TopTools.Types as TopTools
import OpenCascade.TopTools.Internal.Destructors (deleteListOfShape)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr
import Data.Acquire
import Foreign.C (CBool (..), CInt)

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

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_modified" rawModified
    :: Ptr MakeShape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopTools.ListOfShape)

modified :: Ptr MakeShape -> Ptr TopoDS.Shape -> Acquire (Ptr TopTools.ListOfShape)
modified builder s = mkAcquire (wrapException $ rawModified builder s) deleteListOfShape

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_generated" rawGenerated
    :: Ptr MakeShape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopTools.ListOfShape)

generated :: Ptr MakeShape -> Ptr TopoDS.Shape -> Acquire (Ptr TopTools.ListOfShape)
generated builder s = mkAcquire (wrapException $ rawGenerated builder s) deleteListOfShape

foreign import capi unsafe "hs_BRepBuilderAPI_MakeShape.h hs_BRepBuilderAPI_MakeShape_isDeleted" rawIsDeleted
    :: Ptr MakeShape
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

isDeleted :: Ptr MakeShape -> Ptr TopoDS.Shape -> IO Bool
isDeleted builder s = cBoolToBool <$> wrapException (rawIsDeleted builder s)