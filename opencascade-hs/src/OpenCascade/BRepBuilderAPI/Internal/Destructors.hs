module OpenCascade.BRepBuilderAPI.Internal.Destructors
( deleteMakeVertex
, deleteMakeWire
, deleteMakeFace
, deleteMakeSolid
, deleteSewing
) where

import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakeVertex.hxx>"
C.include "<BRepBuilderAPI_MakeWire.hxx>"
C.include "<BRepBuilderAPI_MakeFace.hxx>"
C.include "<BRepBuilderAPI_MakeSolid.hxx>"
C.include "<BRepBuilderAPI_Sewing.hxx>"

deleteMakeVertex :: Ptr MakeVertex -> IO ()
deleteMakeVertex makeVertex = [C.throwBlock| void {
  delete $(BRepBuilderAPI_MakeVertex* makeVertex);
} |]

deleteMakeWire :: Ptr MakeWire -> IO ()
deleteMakeWire makeWire = [C.throwBlock| void {
  delete $(BRepBuilderAPI_MakeWire* makeWire);
} |]

deleteMakeFace :: Ptr MakeFace -> IO ()
deleteMakeFace makeFace = [C.throwBlock| void {
  delete $(BRepBuilderAPI_MakeFace* makeFace);
} |]

deleteMakeSolid :: Ptr MakeSolid -> IO ()
deleteMakeSolid makeSolid = [C.throwBlock| void {
  delete $(BRepBuilderAPI_MakeSolid* makeSolid);
} |]

deleteSewing :: Ptr Sewing -> IO ()
deleteSewing sewing = [C.throwBlock| void {
  delete $(BRepBuilderAPI_Sewing* sewing);
} |]


