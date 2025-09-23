module OpenCascade.TopoDS.Solid
( Solid
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

C.include "<TopoDS_Solid.hxx>"

-- new

new :: Acquire (Ptr Solid)
new = mkAcquire createSolid (deleteShape . castPtr)
  where
    createSolid = [C.throwBlock| TopoDS_Solid* {
      return new TopoDS_Solid();
    } |]
