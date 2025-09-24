module OpenCascade.BRepMesh.IncrementalMesh
( fromShapeAndLinDeflection
, perform
) where 

import OpenCascade.BRepMesh.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.BRepMesh.Types (IncrementalMesh)
import OpenCascade.BRepMesh.Internal.Destructors (deleteIncrementalMesh)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.Ptr
import Data.Acquire
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> brepMeshContext)

C.include "<BRepMesh_IncrementalMesh.hxx>"
C.include "<TopoDS_Shape.hxx>"


fromShapeAndLinDeflection :: Ptr TopoDS.Shape -> Double -> Acquire (Ptr IncrementalMesh)
fromShapeAndLinDeflection shape linDeflection =
  let cLinDeflection = realToFrac linDeflection
      createMesh = [C.throwBlock| BRepMesh_IncrementalMesh* {
        return new BRepMesh_IncrementalMesh(*$(TopoDS_Shape* shape), $(double cLinDeflection));
      } |]
  in mkAcquire createMesh deleteIncrementalMesh

perform :: Ptr IncrementalMesh -> IO ()
perform mesh = [C.throwBlock| void {
  $(BRepMesh_IncrementalMesh* mesh)->Perform();
} |]
