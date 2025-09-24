
module OpenCascade.RWObj.Internal.Destructors
( deleteCafWriter
, deleteCafReader
) where

import OpenCascade.RWObj.Internal.Context
import OpenCascade.RWObj.Types
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> rwObjContext)

C.include "<RWObj_CafWriter.hxx>"
C.include "<RWObj_CafReader.hxx>"

deleteCafWriter :: Ptr CafWriter -> IO ()
deleteCafWriter writerPtr = [C.throwBlock| void {
  delete $(RWObj_CafWriter* writerPtr);
} |]

deleteCafReader :: Ptr CafReader -> IO ()
deleteCafReader readerPtr = [C.throwBlock| void {
  delete $(RWObj_CafReader* readerPtr);
} |]