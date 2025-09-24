module OpenCascade.BRepMesh.Internal.Destructors 
( deleteIncrementalMesh
) where

import OpenCascade.BRepMesh.Internal.Context
import OpenCascade.BRepMesh.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> brepMeshContext)

C.include "<BRepMesh_IncrementalMesh.hxx>"

deleteIncrementalMesh :: Ptr IncrementalMesh -> IO ()
deleteIncrementalMesh meshPtr = [C.throwBlock| void {
  delete $(BRepMesh_IncrementalMesh* meshPtr);
} |]

