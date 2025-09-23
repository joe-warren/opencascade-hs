module OpenCascade.TopoDS.Vertex
( Vertex
, new
) where 

import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Context
import OpenCascade.TopoDS.Internal.Destructors
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Data.Acquire 

C.context (C.cppCtx <> topoDSContext)

C.include "<TopoDS_Vertex.hxx>"

-- new

new :: Acquire (Ptr Vertex)
new = mkAcquire createVertex (deleteShape . castPtr)
  where
    createVertex = [C.throwBlock| TopoDS_Vertex* {
      return new TopoDS_Vertex();
    } |]
