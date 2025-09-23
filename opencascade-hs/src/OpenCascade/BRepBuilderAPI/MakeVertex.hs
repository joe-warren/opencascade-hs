module OpenCascade.BRepBuilderAPI.MakeVertex
( MakeVertex
, fromPnt
, vertex
) where

import OpenCascade.BRepBuilderAPI.Types (MakeVertex)
import OpenCascade.BRepBuilderAPI.Internal.Context
import OpenCascade.BRepBuilderAPI.Internal.Destructors (deleteMakeVertex)
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import OpenCascade.Inheritance (upcast)

C.context (C.cppCtx <> gpContext <> topoDSContext <> brepBuilderAPIContext)

C.include "<BRepBuilderAPI_MakeVertex.hxx>"
C.include "<TopoDS_Vertex.hxx>"

fromPnt :: Ptr GP.Pnt -> Acquire (Ptr MakeVertex)
fromPnt pnt = mkAcquire createMakeVertex deleteMakeVertex
  where
    createMakeVertex = [C.throwBlock| BRepBuilderAPI_MakeVertex* {
      return new BRepBuilderAPI_MakeVertex(*$(gp_Pnt* pnt));
    } |]

vertex :: Ptr MakeVertex -> Acquire (Ptr TopoDS.Vertex)
vertex makeVertex = mkAcquire createVertex (TopoDS.Destructors.deleteShape . upcast)
  where
    createVertex = [C.throwBlock| TopoDS_Vertex* {
      return new TopoDS_Vertex($(BRepBuilderAPI_MakeVertex* makeVertex)->Vertex());
    } |]
