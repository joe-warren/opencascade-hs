module OpenCascade.BRepTools.WireExplorer
( WireExplorer
, fromWire
, more
, next
, current
, orientation
)where

import OpenCascade.BRepTools.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.BRepTools.Types (WireExplorer)
import OpenCascade.BRepTools.Internal.Destructors (deleteWireExplorer)
import OpenCascade.Internal.Bool (cBoolToBool)
import qualified OpenCascade.TopAbs as TopAbs
import Foreign.Ptr
import Foreign.C
import Data.Acquire
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> brepToolsContext)

C.include "<BRepTools_WireExplorer.hxx>"
C.include "<TopoDS_Wire.hxx>"
C.include "<TopoDS_Edge.hxx>"
C.include "<TopAbs_Orientation.hxx>"

C.verbatim "extern template class NCollection_Map<TopoDS_Shape, TopTools_ShapeMapHasher>;"
C.verbatim "extern template class NCollection_DataMap<TopoDS_Shape, TopTools_ListOfShape, TopTools_ShapeMapHasher>;"
C.verbatim "extern template class NCollection_DataMap<TopoDS_Shape, TopTools_ListOfShape, TopTools_ShapeMapHasher>::Iterator;"

fromWire :: Ptr TopoDS.Wire -> Acquire (Ptr WireExplorer)
fromWire wire =
  let createExplorer = [C.throwBlock| BRepTools_WireExplorer* {
        return new BRepTools_WireExplorer(*$(TopoDS_Wire* wire));
      } |]
  in mkAcquire createExplorer deleteWireExplorer

more :: Ptr WireExplorer -> IO Bool
more explorer = do
  result <- [C.throwBlock| bool {
    return $(BRepTools_WireExplorer* explorer)->More();
  } |]
  return (cBoolToBool result)

next :: Ptr WireExplorer -> IO ()
next explorer = [C.throwBlock| void {
  $(BRepTools_WireExplorer* explorer)->Next();
} |]

current :: Ptr WireExplorer -> IO (Ptr TopoDS.Edge)
current explorer = [C.throwBlock| TopoDS_Edge* {
  return new TopoDS_Edge($(BRepTools_WireExplorer* explorer)->Current());
} |]

orientation :: Ptr WireExplorer -> IO TopAbs.Orientation
orientation explorer = do
  result <- [C.throwBlock| int {
    return static_cast<int>($(BRepTools_WireExplorer* explorer)->Orientation());
  } |]
  return (toEnum $ fromIntegral result)

