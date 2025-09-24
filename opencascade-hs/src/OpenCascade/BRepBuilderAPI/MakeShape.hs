module OpenCascade.BRepBuilderAPI.MakeShape
( MakeShape
, shape
, build
) where

import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BRepBuilderAPI.Types
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.Ptr
import Data.Acquire
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakeShape.hxx>"
C.include "<TopoDS_Shape.hxx>"

shape :: Ptr MakeShape -> Acquire (Ptr TopoDS.Shape)
shape builder =
  let createShape = [C.throwBlock| TopoDS_Shape* {
        return new TopoDS_Shape($(BRepBuilderAPI_MakeShape* builder)->Shape());
      } |]
  in mkAcquire createShape deleteShape

build :: Ptr MakeShape -> IO ()
build builder = [C.throwBlock| void {
  $(BRepBuilderAPI_MakeShape* builder)->Build();
} |]