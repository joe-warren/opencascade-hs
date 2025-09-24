module OpenCascade.RWGltf.Internal.Destructors
( deleteCafWriter
, deleteCafReader
) where

import OpenCascade.RWGltf.Internal.Context
import OpenCascade.RWGltf.Types
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr

C.context (C.cppCtx <> rwGltfContext)

C.include "<RWGltf_CafWriter.hxx>"
C.include "<RWGltf_CafReader.hxx>"

deleteCafWriter :: Ptr CafWriter -> IO ()
deleteCafWriter writerPtr = [C.throwBlock| void {
  delete $(RWGltf_CafWriter* writerPtr);
} |]

deleteCafReader :: Ptr CafReader -> IO ()
deleteCafReader readerPtr = [C.throwBlock| void {
  delete $(RWGltf_CafReader* readerPtr);
} |]