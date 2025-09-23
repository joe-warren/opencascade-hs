module OpenCascade.TopoDS.Compound
( Compound
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

C.include "<TopoDS_Compound.hxx>"

-- new

new :: Acquire (Ptr Compound)
new = mkAcquire createCompound (deleteShape . castPtr)
  where
    createCompound = [C.throwBlock| TopoDS_Compound* {
      return new TopoDS_Compound();
    } |]
