module OpenCascade.TopoDS.CompSolid
( CompSolid
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

C.include "<TopoDS_CompSolid.hxx>"

-- new

new :: Acquire (Ptr CompSolid)
new = mkAcquire createCompSolid (deleteShape . castPtr)
  where
    createCompSolid = [C.throwBlock| TopoDS_CompSolid* {
      return new TopoDS_CompSolid();
    } |]
