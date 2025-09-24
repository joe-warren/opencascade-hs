module OpenCascade.NCollection.Internal.Destructors
( deletePntArray
) where

import OpenCascade.NCollection.Internal.Context
import OpenCascade.GP.Internal.Context (gpContext)
import OpenCascade.NCollection.Types
import OpenCascade.GP.Types
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> gpContext <> nCollectionContext)

C.include "<NCollection_Array1.hxx>"
C.include "<gp_Pnt.hxx>"

deletePntArray :: Ptr (Array1 Pnt) -> IO ()
deletePntArray arrayPtr = [C.throwBlock| void {
  delete $(NCollection_Array1<gp_Pnt>* arrayPtr);
} |]

