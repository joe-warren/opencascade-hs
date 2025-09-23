module OpenCascade.TopoDS.Wire
( Wire
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

C.include "<TopoDS_Wire.hxx>"

-- new

new :: Acquire (Ptr Wire)
new = mkAcquire createWire (deleteShape . castPtr)
  where
    createWire = [C.throwBlock| TopoDS_Wire* {
      return new TopoDS_Wire();
    } |]
