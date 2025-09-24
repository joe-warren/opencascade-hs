module OpenCascade.Bnd.Internal.Destructors 
( deleteBox
, deleteOBB
) where

import OpenCascade.Bnd.Internal.Context
import Foreign.Ptr
import OpenCascade.Bnd.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> bndContext)

C.include "<Bnd_Box.hxx>"
C.include "<Bnd_OBB.hxx>"

deleteBox :: Ptr Box -> IO ()
deleteBox boxPtr = [C.throwBlock| void {
  delete $(Bnd_Box* boxPtr);
} |]

deleteOBB :: Ptr OBB -> IO ()
deleteOBB obbPtr = [C.throwBlock| void {
  delete $(Bnd_OBB* obbPtr);
} |]