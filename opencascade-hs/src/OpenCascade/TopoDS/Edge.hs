module OpenCascade.TopoDS.Edge
( Edge
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

C.include "<TopoDS_Edge.hxx>"

-- new

new :: Acquire (Ptr Edge)
new = mkAcquire createEdge (deleteShape . castPtr)
  where
    createEdge = [C.throwBlock| TopoDS_Edge* {
      return new TopoDS_Edge();
    } |]
