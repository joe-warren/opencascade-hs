module OpenCascade.TopoDS.Face
( Face
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

C.include "<TopoDS_Face.hxx>"

-- new

new :: Acquire (Ptr Face)
new = mkAcquire createFace (deleteShape . castPtr)
  where
    createFace = [C.throwBlock| TopoDS_Face* {
      return new TopoDS_Face();
    } |]
