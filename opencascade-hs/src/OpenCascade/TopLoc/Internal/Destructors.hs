module OpenCascade.TopLoc.Internal.Destructors
( deleteLocation
) where

import OpenCascade.TopLoc.Types
import OpenCascade.TopLoc.Internal.Context
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> topLocContext)

C.include "<TopLoc_Location.hxx>"

deleteLocation :: Ptr Location -> IO ()
deleteLocation location = [C.throwBlock| void {
  delete $(TopLoc_Location* location);
} |]
