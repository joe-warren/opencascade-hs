module OpenCascade.BRepTools.Internal.Destructors
( deleteWireExplorer
) where

import OpenCascade.BRepTools.Internal.Context
import OpenCascade.BRepTools.Types
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> brepToolsContext)

C.include "<BRepTools_WireExplorer.hxx>"

-- needed to avoid a linker error about duplicate symbols
C.verbatim "extern template class NCollection_Map<TopoDS_Shape, TopTools_ShapeMapHasher>;"
C.verbatim "extern template class NCollection_DataMap<TopoDS_Shape, TopTools_ListOfShape, TopTools_ShapeMapHasher>;"

deleteWireExplorer :: Ptr WireExplorer -> IO ()
deleteWireExplorer explorerPtr = [C.throwBlock| void {
  delete $(BRepTools_WireExplorer* explorerPtr);
} |]