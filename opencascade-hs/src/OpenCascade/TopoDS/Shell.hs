module OpenCascade.TopoDS.Shell
( Shell
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

C.include "<TopoDS_Shell.hxx>"

-- new

new :: Acquire (Ptr Shell)
new = mkAcquire createShell (deleteShape . castPtr)
  where
    createShell = [C.throwBlock| TopoDS_Shell* {
      return new TopoDS_Shell();
    } |]
